library(tidyverse)
library(sf) #shapefile package
library(tmap)
library(hereR)
library(osmdata) # open streetmap packge
library(dplyr)
library(geojson_sf)
library(readxl)

# Set up ------------------------------------------------------------------


tmap_mode("view")
set_key("xrcYx5euwWciYdK-coeSXcu16kjdJAujw9INxZRAp_U")
crs <- 4326

# reading in shapefile from map server
rail <-read_sf("C:/Users/nanzawi/OneDrive - HDR, Inc/Data_Science/MODOT_RR/inputs/MNARail.shp") %>% st_transform(crs)
rail <- st_geometry(rail)

# read in Replica points
replica_points_path <- "C:/Users/nanzawi/OneDrive - HDR, Inc/Data_Science/MODOT_RR/Replica_APIs/replicabq/Data"

ReplicaCrossings_MNA <- read_excel("C:/Users/nanzawi/OneDrive - HDR, Inc/Data_Science/MODOT_RR/Replica_APIs/replicabq/Data/replica_crossing_trips_spring_2023_weekday/ReplicaCrossings_MNA.xlsx",
                                   sheet = "replica_crossing_trips_spring_2")



# Function to create points on each side of the track and find route ---------------------------------------------------------
# name of route, point is location for start of route, offset from point, bufffer, rail_simplified the actual dataset

xingCloseRoute <- function(name, point, offset, buffer, rail) {
  navPoints <- point %>% st_buffer(buffer) %>% st_intersection(rail) %>% st_cast("POINT") %>%
  transmute(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>% kmeans(center = 2, algorithm = "Lloyd") %>%
  .$centers %>% as_tibble() %>% mutate(name = row_number()) %>%
  mutate(
    xavg = mean(.$x),
    yavg = mean(.$y),
    xdiff = .$x[1] - .$x[2],
    ydiff = .$y[1] - .$y[2]
    ) %>%
  slice(1) %>%
  transmute(
    angle = atan(ydiff/xdiff) + pi/2,
    x1 = xavg + cos(angle)*offset,
    y1 = yavg + sin(angle)*offset,
    x2 = xavg - cos(angle)*offset,
    y2 = yavg - sin(angle)*offset
    ) %>%
  select(-angle) %>% add_row() %>%
  transmute(
    point = c("side1", "side2"),
    xnew = c(.$x1[1], .$x2[1]),
    ynew = c(.$y1[1], .$y2[1])
    ) %>%
    st_as_sf(coords = c("xnew", "ynew"), crs = crs)

  barrier <- point %>% st_buffer(buffer) %>% st_cast("LINESTRING") %>%
    st_intersection(rail) %>% st_cast("POINT") %>% summarize() %>%
    st_cast("LINESTRING") %>% st_buffer(10)

  routeTest1 <-
    route(
      origin = navPoints %>% filter(point == "side1"),
      destination = navPoints %>% filter(point == "side2"),
      transport_mode = "car",
      routing_mode = "fast",
      results = 1,
      avoid_area = barrier
    ) %>% mutate(side = "side1")

  routeTest2 <-
    route(
      origin = navPoints %>% filter(point == "side2"),
      destination = navPoints %>% filter(point == "side1"),
      transport_mode = "car",
      routing_mode = "fast",
      results = 1,
      avoid_area = barrier
    ) %>% mutate(side = "side2")

  return(bind_rows(routeTest1, routeTest2) %>% mutate(name = name, .before = geometry))
}


p <- tibble(id = "barrier", lat = 37.414583, lon = -94.28185) %>%
    st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326) %>% st_transform(crs)


test <- xingCloseRoute(name = "test run", point = p, offset = 500, buffer = 15, rail = rail)

tm_shape(test) + tm_lines(col = "side", palette = "Set1", lwd = 5)

# Map -----------------------------------------------------

tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
    tm_shape(routeTest1) + tm_lines(lwd = 5, col = "darkred") +
    tm_shape(routeTest2) + tm_lines(lwd = 5, col = "darkblue") +
    tm_shape(intBarrier) + tm_polygons(alpha = 0.5)






# Create Isolines ---------------------------------------------------------

# calculate isochrones by poit of interest (poi) which is the centroid of the barrier, checks to see if geos are valid, sets range to values in seconds

isoTest <-
    isoline(
        poi = barrier %>% st_centroid(),
        range_type = "time",
        range = c(5, 10, 15, 20) * 60,
        routing_mode = "fast",
        transport_mode = "car",
        traffic = FALSE
    ) %>%
    mutate(minutes = as.integer(range / 60))

st_is_valid(isoTest)

tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
    tm_shape(isoTest) + tm_polygons(col = "minutes", n = 4, palette = "-Reds", alpha = 0.5) +
    tm_shape(points) + tm_dots("purple")




# Junk --------------------------------------------------------------------


roads <-
    opq(as.numeric(st_bbox(barrier %>% st_transform(4326)))) %>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf() #%>% .$osm_lines #%>% st_as_sf() %>% st_transform(crs)
