# load packages ----
library(dplyr)
library(gdistance)
library(ggplot2)
library(here)
library(purrr)
library(raster)
library(readr)
library(rgdal)
library(rgeos)
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
s <- raster(r, res = 5)

# remove and change NA values to fit within the extent
s <- rasterize(x = r, y = s, field = 1)

# plot raster to make sure it looks apporipriate
plot(s)


# create transition layer with directions being queens space
trans <- transition(x = s, transitionFunction = mean, directions = 16)


# transform rec points to utms -----
# You will need to replace zone with the correct UTM ZONE
rec_sum <- spTransform(rl_sum_spd, CRS("+proj=utm +zone=15 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) %>%
  as_tibble() %>%
  rename(lon = coords.x1,
         lat = coords.x2) %>%
  arrange(lon)


# create rec lat and long compare tibble -----
rec_compare <- rec_sum %>%
  dplyr::select(-rec_name, -rec_group) %>%
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

# calculate the distance between every receiver -----
rec_dist <- rec_compare %>%
  split(.$id) %>%
  map(possibly(~shortestPath(trans,
                             c(.$llon, .$llat), c(.$lon, .$lat),
                             output = "SpatialLines"), NA)) %>%
  map(possibly(~gLength(.), NA)) %>%
  map_df(., as_tibble) %>%
  rownames_to_column("id" ) %>%
  mutate_if(is.character, function(x) as.numeric(x))  %>%
  rename(cost_dist = value)



# create sf object for the caclucating the distance between every rec -----
rec_dist_sf <- rec_compare %>%
  split(.$id) %>%
  map(possibly(~shortestPath(trans,
                             c(.$llon, .$llat),
                             c(.$lon, .$lat),
                             output = "SpatialLines"), NA)) %>%
  map(possibly(~st_as_sf(., crs = 26918), NA), .id = "id") %>%
  bind_rows() %>%
  rownames_to_column("id") %>%
  mutate_if(is.character, function(x) as.numeric(x))

######### start working on joining everything so they line up ---------
# add rec group to rec summary
rec_sum <- rec_sum %>%
  mutate(rec_group = factor(rl_sum_sf$rec_group, 
                            levels = c("Head",
                                       "Neck",
                                       "Body",
                                       "Arm",
                                       "Legs")
                            ))


# create a to and from list using rec group names
from_to <- rec_sum %>%
  arrange(lon) %>%
  mutate(from = rec_group,
         to = rec_group) %>%
  expand(from, to) %>%
  mutate(from_to = paste0(from, "-", to)) %>%
  rownames_to_column("id") %>%
  mutate(id = as.numeric(id))

# rename lon and lat in rec_compare to from and to
rec_compare <- rec_compare %>%
  rename(from_lon = lon,
         from_lat = lat,
         to_lon = llon,
         to_lat = llat)

# join to and from to rec_compare
rec_compare_fr_to <- from_to %>%
  left_join(rec_compare, by = "id")


# join rec_compare_from_to to sf object
rec_dist_sf <- rec_dist_sf %>%
  left_join(rec_compare_fr_to, by = "id")

# add distances that were calculated
rec_dist_sf <-  rec_dist_sf %>%
  left_join(rec_dist, by = "id")


# covert sf objects from Lat Long to UTM ----
# You will need to replace zone with the correct UTM ZONE
lake_utm <- st_transform(lake, crs = 32615)
rl_sum_utm <- st_transform(rl_sum_sf, crs = 32615)

# plot -----
ggplot() +
  geom_sf(data = lake_utm) +
  geom_sf(data = rec_dist_sf, aes(colour = cost_dist), size = 1) +
  geom_sf(data = rl_sum_utm, size = 6, aes(shape = rec_group)) +
  scale_colour_viridis_c(name = "Cost Distance (m)", option = "B") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "Longitude",
       y = "Latitude")

