# ---- load packages ----
{
  library(crawl)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(here)
  library(lwgeom)
  library(pathroutr)
  library(purrr)
  library(readr)
  library(RcppAlgos)
  library(sf)
  library(tibble)
  library(tidyr)
  source(here::here("R", 
                    "Functions", 
                    "make_linestring.R"))
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

# ---- look at lake and rec structures -----
lake
rl_sum_sf

# ---- convert utms as distance calcualutations need to be metric ----
lake_utm <- lake %>% 
  st_transform(., crs = 32615)

rl_sum_sf_utm <- rl_sum_sf %>% 
  st_transform(., crs = 32615)

# both have the same CRS so we are good
# if CRS is different between body of water shape and receiver stations 
# then you will need to change CRS of one so they both match 

# ---- for pathroutr inland lakes/rivers need to inverted ----- 

# as pathrout looks at polygons as land barriers not water 
# grab the extent of the shapefile you're working with 
# and convert it into a sf object  
ext <- st_bbox(lake_utm, crs = st_crs(lake_utm)) %>%
  st_as_sfc() %>%
  st_sf()

# use st_difference to create sf object with body of water as empty 
inverse <- st_difference(ext, lake_utm)

# plot to make sure it looks proper 
ggplot() + 
  geom_sf(data = inverse)

# ---- create land region to use as a barrier -----
land_region <- rl_sum_sf_utm %>% 
  st_buffer(dist = 650) %>% # change dist to site specific buffer
  st_union() %>%
  st_convex_hull() %>% 
  st_intersection(inverse) %>% 
  st_sf()
ggplot() + 
  geom_sf(data = land_region)

# ---- prep path to determine shortest length ----
# convert receiver location sf object to table with each rec possibility 

prep_path <- rl_sum_sf %>%
  mutate(
    lon = st_coordinates(.)[,"X"],
    lat = st_coordinates(.)[,"Y"],
  ) %>% 
  st_drop_geometry() %>% 
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

# prep_path will be a dataframe of the lat/long combination of each possible
# to and from 
prep_path 


# ---- Create df that is each rec combo linked to lat long for combo ----

rec_order <- prep_path %>%
  left_join(
    rl_sum_sf %>% 
      mutate(
        lon = st_coordinates(.)[,"X"], 
        lat = st_coordinates(.)[,"Y"]
      ) %>% 
      st_drop_geometry() %>% 
      rename(from = rec_name) %>% 
      select(from, lon, lat), ., by = c("lon", "lat"), 
    multiple = "all"
  ) %>%  
  left_join(
    rl_sum_sf %>% 
      mutate(
        lon = st_coordinates(.)[,"X"]
      ) %>% 
      st_drop_geometry() %>% 
      rename(to = rec_name,
             llon = lon) %>% 
      select(to, llon), ., by = c("llon"), 
    multiple = "all"
  ) %>% 
  mutate(
    from_to = paste0(from, "-", to)
  ) %>% 
  select(from, to, from_to, lon, lat, llon, llat)

# rec_order is pre_path, with the metadata of which receivers go where  

rec_order

# ---- convert prep_path df to sf object with lat/long being linestrings ----
path <- prep_path %>%
  pmap(make_line) %>%
  st_as_sfc(crs = 4326) %>%
  st_sf() %>%  
  mutate(
    lon = st_startpoint(.) %>%
      st_coordinates(.) %>%
      as_tibble() %>%
      .$X,
    llon = st_endpoint(.) %>%
      st_coordinates(.) %>%
      as_tibble() %>%
      .$X
  ) %>% 
  left_join(., rec_order %>%
              select(from:lon, llon),
            by = c("lon", "llon")
  ) %>%
  select(from:from_to) %>% 
  st_transform(crs = 32615) %>% 
  arrange(from, to)

path

# ---- sample points along path for pathroutr and convert to multipoint -----
# change dfMaxLength to whatever sampling interval of interest, I 
# choose 5 m but this can be any metric 
# Without path_pts R only can look at receiver to receiver 
path_pts <- path %>% 
  st_segmentize(dfMaxLength = units::set_units(5, m), 
                # type = "regular"
  ) %>% 
  st_cast("MULTIPOINT")


# ---- plot land_region, rec_locs, and paths and path points ------
ggplot() + 
  geom_sf(data = land_region, fill = "cornsilk3") +
  geom_sf(data = rl_sum_sf, colour = "deepskyblue3", size = 2) +
  geom_sf(data = path, colour = "darkgrey") + 
  theme_void()

ggplot() + 
  geom_sf(data = rl_sum_sf, colour = "deepskyblue3", size = 3) +
  geom_sf(data = path, colour = "darkgrey", 
          linewidth = 1) + 
  theme_void()


ggplot() + 
  geom_sf(data = land_region, fill = "cornsilk3") +
  geom_sf_label(data = rl_sum_sf, aes(label = rec_name), 
                size = 4) +
  geom_sf(
    data = path %>% # look at just rec 1 to make sure 1 goes to each rec 
            filter(from == 1), aes(colour = from_to), 
          linewidth = 1) + 
  theme_void()

# ---- use pathroutr to id track points that intersect the shore of the lake ------
lake_bar_seg <- get_barrier_segments(trkpts = path_pts,
                                     barrier =  land_region) %>% 
  arrange(n_pts)

lake_bar_seg

# ---- create visgraph using land_regeion which is network analysis triangles -----
# pathroutr will use network analyses of triangles as paths 
vis <- prt_visgraph(barrier = land_region, 
                    # buffer = 50 
                    # adjust buffer, in this case buffer is 50 m 
                    # away from land
                    )
vis

# use sfnetworks to activate edge reroutes 
vis_graph_sf <- sfnetworks::activate(vis, "edges") %>% 
  sf::st_as_sf()


# view visgraph network of triangles that sfnetwork creates 
# sfnetwork is  powerful 
ggplot() + 
  geom_sf(data = vis_graph_sf)



# create what sections have been rerouted from the shore 
segs_tbl <- lake_bar_seg %>% 
  prt_shortpath(vis, blend = TRUE)


# view rerouted sections around land 
ggplot() + 
  geom_sf(data = land_region, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(segs_tbl$geometry, color = "deepskyblue3", 
                           linewidth = 1) +
  theme_void()


# ---- take striahtline paths and add in the reroutes ------  
track_pts_fix <- prt_reroute(path_pts, land_region, vis)

track_pts_fix <- prt_update_points(track_pts_fix, path_pts)


# convert in linestrings 
track_pts_fixed <- track_pts_fix %>% 
  group_by(from_to) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast('LINESTRING') %>%  
  ungroup() %>% 
  separate(from_to, into = c("from", "to"), sep = "-", 
           remove = FALSE) %>% 
  mutate(
    cost_dist_m = as.numeric(st_length(.))
  ) %>% 
  filter(from != to) %>% 
  dplyr::select(from, to, from_to, cost_dist_m, geometry)



# view one reroute to confimr pathroutr is reouting 
track_pts_fixed %>% 
  filter(from_to == "15-11") %>% 
ggplot() + 
  geom_sf(data = land_region, fill = "cornsilk3", size = 0) +
  geom_sf(color = "deepskyblue3", 
          linewidth = 1) +
  theme_void()

# ---- make distance graph between receviers ----- 
ggplot() + 
  geom_sf(data = lake_utm, colour = "black",
          # fill = "cornsilk3", 
         size = 1) +
  geom_sf(data = rl_sum_sf_utm, 
          size = 4, colour = "black") + 
  geom_sf(data = track_pts_fixed, 
          aes(color = cost_dist_m), 
          linewidth = 1) +
  scale_colour_viridis_c(option = "B", 
                         name = "Cost Distance (m)") + 
  theme_bw(base_size = 13) + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black")) + 
  labs(x = "Longitude", 
       y = "Latitude") 
ggsave(here("Plots", 
            "big_sissabagama_lake_cost_dist_pathroutr.png"), 
       height = 8.5, width = 8.5, plot = last_plot())

# ---- simulate dietections 

library(crawl)

rl_sum_sf


df <- data.frame(
  fish_id = "1234",
  Time = seq.POSIXt(as.POSIXct("2018-05-26 05:06:25"), 
                         as.POSIXct("2018-06-26 05:06:25"), 
                         length.out = 10), 
  rec_name = c(3, 1, 3, 5, 4, 6, 5, 5, 4, 3), 
  temp = c(5.5, 6.2, 8.2, 8.1, 8.5, 7.8, 7.4, 7.9, 8.5, 7.1), 
  lat = c(45.79588, 45.7989, 45.79588, 45.79373, 
          45.7918, 45.79378, 45.79373, 45.79373, 
          45.7918, 45.79588), 
  long = c(-91.5236, -91.52844, -91.5236, -91.5191, 
           -91.52147, -91.51294, -91.5191, -91.5191, 
           -91.52147, -91.5236)
  
  
) %>% 
  st_as_sf(., coords = c("long", "lat"), 
           crs = st_crs(rl_sum_sf)) %>% 
  st_transform(., crs = 32615)
df
unique(harborSeal_sf$DryTime)


fixPar <- c(log(250), log(500), log(1500), rep(NA,5), 0)
fixPar <- c(log(5), log(10), log(50))


displayPar(mov.model=~1, 
            err.model=list(x=~rec_name-1),
            data = df, 
            # activity= ~ I(1 - temp),
            fixPar=fixPar)
# 
constr=list(
  lower=c(rep(log(1500),3), rep(-Inf,2)),
  upper=rep(Inf,5)
)
# 
# fixPar <- c(log(250), log(500), log(1500), rep(NA,5), 0)
# 
m <- crwMLE(
  mov.model = ~1,
  err.model = list(x = ~rec_name - 1),
  # activity = ~I(1-DryTime),
  data = df,
  Time.name = "Time",
  fixPar = fixPar,
  # theta = c(rep(log(5000),1)),
  # constr = constr,
  method = "L-BFGS-B",
  control = list(maxit = 2000,
                 trace = 1,
                 REPORT = 1)
)
# 
# 
m

unique(harborSeal_sf$Argos_loc_class)

fixPar = c(log(250), log(500), log(1500), rep(NA,5), 0)
displayPar( mov.model=~1, err.model=list(x=~Argos_loc_class-1),data=harborSeal_sf, 
            activity=~I(1-DryTime),fixPar=fixPar)
#>                  ParNames   fixPar thetaIdx
#> 1 ln tau Argos_loc_class0 5.521461       NA
#> 2 ln tau Argos_loc_class1 6.214608       NA
#> 3 ln tau Argos_loc_class2 7.313220       NA
#> 4 ln tau Argos_loc_class3       NA        1
#> 5 ln tau Argos_loc_classA       NA        2
#> 6 ln tau Argos_loc_classB       NA        3
#> 7    ln sigma (Intercept)       NA        4
#> 8     ln beta (Intercept)       NA        5
#> 9                  ln phi 0.000000       NA
constr=list(
  lower=c(rep(log(1500),3), rep(-Inf,2)),
  upper=rep(Inf,5)
)

set.seed(123)
fit1 <- crwMLE(
  mov.model=~1, err.model=list(x=~Argos_loc_class-1), activity=~I(1-DryTime),
  data=harborSeal_sf,  Time.name="Time", 
  fixPar=fixPar, theta=c(rep(log(5000),3),log(3*3600), 0),
  constr=constr, method="L-BFGS-B",
  control=list(maxit=2000, trace=1, REPORT=1)
)
#> Beginning SANN initialization ...
#> Beginning likelihood optimization ...
#> iter    1 value 41202.609844
#> iter    2 value 41056.998394
#> iter    3 value 40756.499672
#> iter    4 value 40146.411888
#> iter    5 value 40066.268387
#> iter    6 value 40005.050127
#> iter    7 value 40001.092459
#> iter    8 value 40001.050325
#> iter    9 value 40001.048541
#> iter   10 value 40001.048534
#> final  value 40001.048534 
#> converged

pred1 = crwPredict(fit1, predTime = '15 min')
pred1_sf <- pred1 %>% crw_as_sf("POINT","p")
