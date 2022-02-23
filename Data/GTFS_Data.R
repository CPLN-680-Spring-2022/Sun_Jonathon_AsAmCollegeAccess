#GTFS script

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


# Test to make sure everything works --------------------------
GTFS_path <- c("Data\\OTP\\graphs\\default\\google_bus.zip")

GTFS <- read_gtfs(GTFS_path)
GTFS_Bus <- GTFS

validation_result <- attr(GTFS, "validation_result")
#head(validation_result)

GTFS_path <- c("Data\\OTP\\graphs\\default\\google_rail.zip")

GTFS <- read_gtfs(GTFS_path)
GTFS_rail <- GTFS

validation_result <- attr(GTFS, "validation_result")
#head(validation_result)

#GTFS geometry -------------------------

## Bus
GTFS_Bus_shp <- gtfs_as_sf(GTFS_Bus)
GTFS_Bus_route <- get_route_geometry(GTFS_Bus_shp) %>%
                    st_transform(st_crs(Universities))
GTFS_Bus_points <- st_as_sf(GTFS_Bus$stops,
                            coords = c("stop_lon","stop_lat"),
                            crs=4326) %>%
                   st_transform(st_crs(Universities))

GTFS_Bus_points <- st_intersection(GTFS_Bus_points, Philadelphia_School_District)
GTFS_Bus_route <- st_intersection(GTFS_Bus_route, Philadelphia_School_District)

#GTFS_Bus_points_Buffers <- st_union(st_buffer(GTFS_Bus_points, 2640)) %>%
#  st_sf() 

## Rail
GTFS_Rail_shp <- gtfs_as_sf(GTFS_rail)
GTFS_Rail_route <- get_route_geometry(GTFS_Rail_shp) %>%
                    st_transform(st_crs(Universities))
GTFS_Rail_points <- st_as_sf(GTFS_rail$stops,
                            coords = c("stop_lon","stop_lat"),
                            crs=4326) %>%
                    st_transform(st_crs(Universities))

GTFS_Rail_points <- st_intersection(GTFS_Rail_points, Philadelphia_School_District)
GTFS_Rail_route <- st_intersection(GTFS_Rail_route, Philadelphia_School_District)

#GTFS_Rail_points_Buffers <- st_union(st_buffer(GTFS_Rail_points, 2640)) %>%
#  st_sf() 

ggplot() +
  geom_sf(data = Philadelphia_School_District) +
  geom_sf(data = GTFS_Rail_points) +
  geom_sf(data = GTFS_Rail_route) +
  geom_sf(data = GTFS_Rail_points_Buffers,
          fill = "transparent") +
  mapTheme()

ggplot() +
  geom_sf(data = Philadelphia_School_District) +
  geom_sf(data = GTFS_Bus_points) +
  geom_sf(data = GTFS_Bus_route) +
  geom_sf(data = GTFS_Bus_points_Buffers) +
  mapTheme()
