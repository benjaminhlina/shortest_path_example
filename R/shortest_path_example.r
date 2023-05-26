# ---- load packages ----
{
  library(dplyr)
  library(gdistance)
  library(ggplot2)
  library(here)
  library(lwgeom)
  library(purrr)
  library(raster)
  library(readr)
  library(sf)
  library(sp)
  library(tibble)
  library(tidyr)
  make_line <- function(lon, lat, llon, llat) {
    st_linestring(matrix(c(lon, llon, lat, llat), 2, 2))
  }
}
# ---- GOAL - Create shortest distance between Acoustic Telemetry Receivers -----
# ---- bring in shapefile ----
# replace sissabagma_lake with your shapefile 
# I choose Big Siss as an example as this is the lake I grew up fishing on 
# back in Wisco
lake <- st_read(dsn = here("Data",
                           "shapefile",
                           "."),
                layer = "sissabagama_lake")

# create spatialploygondataframe
lake_spd <- as_Spatial(lake)

# ---- bring in rec location sf ----
# replace EXAMPLE with your receiver locations as RDS or csv whatever you use 

rl_sum_sf <- read_rds(here("Data",
                           "receiver locations",
                           "rl_sum_sf.rds"))
# change rec_group to factor 
rl_sum_sf <- rl_sum_sf %>% 
  mutate(rec_group = factor(rec_group, 
                            levels = c("Head",
                                       "Neck",
                                       "Body",
                                       "Arm",
                                       "Legs")))
# create spatialpointsdataframe
rl_sum_spd <- as_Spatial(rl_sum_sf)

# ---- rasterize shapefile  -----
# transform spatialpolygonsdataframe into UTMS
# You will need to replace zone with the correct UTM ZONE
r <- spTransform(lake_spd, CRS("+proj=utm +zone=15 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

# plot them
plot(r)
plot(lake_spd)

# determine the extent of the spatialpoloygonsdataframe
ext <- extent(r)

# create raster and change the resolution to 5 m
# Depending on the system this can be adjusted to whatever resolution you desire 
# if in a large system res could be less fine scaled 
s <- raster(r, res = 5)

# remove and change NA values to fit within the extent
s <- rasterize(x = r, y = s, field = 1)

# plot raster to make sure
#  it looks appropriate
plot(s)

# create transition layer with directions being queens space
# if in larger systems direction could be reduced from queens space to 
# rook or king, 4 or 8 
trans <- transition(x = s, transitionFunction = mean, directions = 16)

# ---- prep path to determine shortest length ----
# convert receiver location sf object to table with each rec possibility 

prep_path <- rl_sum_sf %>%
  mutate(
    lon = st_coordinates(.)[,"X"],# grab lon
    lat = st_coordinates(.)[,"Y"],# grab lat
  ) %>%  
  st_drop_geometry() %>% # drop sf 
  # once geometry removed create to and from lat longs 
  mutate(llon = lon,
         llat = lat,
         lonlat = paste0(lon, ",", lat),
         llonllat = paste0(llon, ",", llat)) %>%
  dplyr::select(-lon, -lat, -llon, -llat) %>%
  expand(lonlat, llonllat) %>% # expand for each to and from combo 
  separate(lonlat, c("lon", "lat"), ",") %>%
  separate(llonllat, c("llon", "llat"), ",") %>%
  mutate_if(is.character, function(x) as.numeric(x)) 

prep_path

# ---- Create df that is each rec combo linked to lat long for combo ----

rec_order <- prep_path %>%
  # we are going to use left join to take prep_path and lineup lat and longs 
  # of froms and tos: first lets join for the froms 
  left_join( 
    rl_sum_sf %>% 
      mutate(
        lon = st_coordinates(.)[,"X"], # grab lon 
        lat = st_coordinates(.)[,"Y"]  # grab lat 
      ) %>% 
      st_drop_geometry() %>% # remove sf 
      rename(from = rec_name) %>% 
      dplyr::select(from, lon, lat), ., by = c("lon", "lat"), 
    multiple = "all"
  ) %>%  # join for the tos  
  left_join(
    rl_sum_sf %>% 
      mutate(
        lon = st_coordinates(.)[,"X"]
      ) %>% 
      st_drop_geometry() %>% 
      rename(to = rec_name,
             llon = lon) %>% 
      dplyr::select(to, llon), ., by = c("llon"), 
    multiple = "all"
  ) %>% 
  mutate(
    from_to = paste0(from, "-", to), 
    id = 1:nrow(.)
  ) %>% 
  dplyr::select(id, from, to, from_to, lon, lat, llon, llat)
rec_order

# ---- convert lat long to utm ---- 
# this is for utm zone north 15 but you will habe to change for your specific 
# utm zone 

# We will take rec order make it an sf object, transform to utsm and then remove
# sf for each start and end point 

rec_order_utm <- st_as_sf(rec_order, 
                         coords = c("lon", "lat"), 
                         crs = st_crs(rl_sum_sf)) %>% 
  st_transform(crs = 32615) %>% 
  mutate(
    lon = st_coordinates(.)[,"X"], # grab lon 
    lat = st_coordinates(.)[,"Y"]  # grab lat 
  ) %>% 
  st_drop_geometry() %>% 
  st_as_sf(., coords = c("llon", "llat"), 
          crs = st_crs(rl_sum_sf)) %>% 
  st_transform(crs = 32615) %>% 
  mutate(
    llon = st_coordinates(.)[,"X"], # grab lon 
    llat = st_coordinates(.)[,"Y"]  # grab lat 
  ) %>% 
  st_drop_geometry()


# ---- create a subset to test map function -----
# rec_5 <- rec_compare %>%
#   filter(id < 5)

# ---- calculate the distance between every receiver and create  sf object -----
rec_dist_sf <- rec_order_utm %>%
  split(.$id) %>%
  map(possibly(~ shortestPath(trans,
                              c(.$llon, .$llat),
                              c(.$lon, .$lat),
                              output = "SpatialLines"), NA)) %>%
  map(possibly(~ st_as_sf(., crs = 32615), NA)) %>% # u will need to replace CRS
  bind_rows(.id = "id") %>%
  mutate(
         cost_dist = as.numeric(st_length(.))
         )# st_length determines path length

rec_dist_sf
# ---- add in to and from columns so we know where the paths go ----- 
rec_order_names <- rec_order_utm %>% 
  mutate(
    id = as.character(id)
  )
  
# join from and to to distance sf object 
rec_dist_sf <- rec_dist_sf %>% 
  left_join(rec_order_names, by = "id") %>% 
  dplyr::select(id, from:llon, cost_dist, geometry)



# ---- covert sf objects from Lat Long to UTM ----
# You will need to replace zone with the correct UTM ZONE
lake_utm <- st_transform(lake, crs = 32615)
rl_sum_utm <- st_transform(rl_sum_sf, crs = 32615)

# ---- plot -----
# First check if paths go to the right locations 
ggplot() +
  geom_sf(data = lake_utm) +
  geom_sf(data = rec_dist_sf %>% 
            filter(from_to %in% "3-12")
            , aes(colour = cost_dist), size = 1) +
  geom_sf_label(data = rl_sum_utm , size = 4, aes(label = rec_name)) +
  scale_colour_viridis_c(name = "Cost Distance (m)", option = "B") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "Longitude",
       y = "Latitude") 

# then plot the whole thing 
ggplot() +
  geom_sf(data = lake_utm) +
  geom_sf(data = rec_dist_sf, aes(colour = cost_dist), size = 1) +
  geom_sf(data = rl_sum_utm , size = 4) +
  scale_colour_viridis_c(name = "Cost Distance (m)", option = "B") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "Longitude",
       y = "Latitude") 



