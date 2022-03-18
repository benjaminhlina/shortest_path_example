# load packages ----
library(dplyr)
library(gdistance)
library(ggplot2)
library(here)
library(purrr)
library(raster)
library(readr)
library(sf)
library(sp)
library(tibble)
library(tidyr)

#### GOAL - Create shortest distance between Acoustic Telemetry Receivers -----



# bring in shapefile ----
# replace sissabagma_lake with your shapefile 
# I choose Big Siss as an example as this is the lake I grew up fishing on 
# back in Wisco
lake <- st_read(dsn = here("Data",
                           "shapefile",
                           "."),
                layer = "sissabagama_lake")

# create spatialploygondataframe
lake_spd <- as_Spatial(lake)

# bring in rec location sf ----
# replace EXAMPLE with your receiver locations as RDS or csv whatever you use 

rl_sum_sf <- read_rds(here("Data",
                           "receiver locations",
                           "rl_sum_sf.rds"))

rl_sum_sf <- rl_sum_sf %>% 
  mutate(rec_group = factor(rec_group, 
                            levels = c("Head",
                                       "Neck",
                                       "Body",
                                       "Arm",
                                       "Legs")))
# create spatialpointsdataframe
rl_sum_spd <- as_Spatial(rl_sum_sf)

# rasterize shapefile  -----
# transform spatialpolygonsdataframe into UTMS
#  # You will need to replace zone with the correct UTM ZONE
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

# plot raster to make sure it looks appropriate
plot(s)


# create transition layer with directions being queens space
# if in larger systems direction could be reduced from queens space to 
# rook or king, 4 or 8 
trans <- transition(x = s, transitionFunction = mean, directions = 16)


# transform rec points to utms -----
# You will need to replace zone with the correct UTM ZONE
rec_sum <- spTransform(rl_sum_spd, CRS("+proj=utm +zone=15 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) %>%
  as_tibble() %>%
  mutate(rec_group = factor(rec_group, 
                            levels = c("Head",
                                       "Neck",
                                       "Body",
                                       "Arm",
                                       "Legs"))) %>% 
  rename(lon = coords.x1,
         lat = coords.x2) %>%
  arrange(lon)



# create a to and from list using rec group names
from_to <- rec_sum %>%
  arrange(lon) %>%
  mutate(from = rec_name,
         to = rec_name) %>%
  expand(from, to) %>%
  mutate(from_to = paste0(from, "-", to))

# create rec lat and long compare tibble -----
rec_compare <- rec_sum %>%
  mutate(llon = lon,
         llat = lat,
         lonlat = paste0(lon, ",", lat),
         llonllat = paste0(llon, ",", llat)) %>%
  dplyr::select(-lon, -lat, -llon, -llat) %>%
  expand(lonlat, llonllat) %>%
  separate(lonlat, c("lon", "lat"), ",") %>%
  separate(llonllat, c("llon", "llat"), ",") %>%
  rownames_to_column("id") %>%
  mutate_if(is.character, function(x) as.numeric(x))



# create a subset to test map function on -----
# rec_5 <- rec_compare %>%
#   filter(id < 5)

# calculate the distance between every receiver and create  sf object -----
rec_dist_sf <- rec_compare %>%
  split(.$id) %>%
  map(possibly(~ shortestPath(trans,
                              c(.$llon, .$llat),
                              c(.$lon, .$lat),
                              output = "SpatialLines"), NA)) %>%
  map(possibly(~ st_as_sf(., crs = 32615), NA)) %>% # u will need to replace CRS
  bind_rows() %>%
  mutate(id = c(1:nrow(.)), 
         cost_dist = as.numeric(st_length(.)))# st_length determines path length

# add in the additional columns to know where the linestrings are going to

rec_dist_sf <- rec_dist_sf %>% 
  mutate(from = from_to$from, 
         to = from_to$to,
         from_to = from_to$from_to,
         from_lon = rec_compare$lon,
         from_lat = rec_compare$lat,
         to_lon = rec_compare$llon,
         to_lat = rec_compare$llat) %>% 
  dplyr::select(id, from:to_lat, cost_dist, geometry)

rec_dist_sf

# covert sf objects from Lat Long to UTM ----
# You will need to replace zone with the correct UTM ZONE
lake_utm <- st_transform(lake, crs = 32615)
rl_sum_utm <- st_transform(rl_sum_sf, crs = 32615)

# plot -----
ggplot() +
  geom_sf(data = lake_utm) +
  geom_sf(data = rec_dist_sf, aes(colour = cost_dist), size = 1) +
  geom_sf(data = rl_sum_utm, size = 4) +
  scale_colour_viridis_c(name = "Cost Distance (m)", option = "B") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "Longitude",
       y = "Latitude")