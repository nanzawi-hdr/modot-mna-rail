library(tidyverse)
library(sf) #shapefile package
library(tmap)
library(hereR)
library(osmdata) # open streetmap packge
library(dplyr)
library(reticulate)


# Set up ------------------------------------------------------------------

tmap_mode("view")
set_key("xrcYx5euwWciYdK-coeSXcu16kjdJAujw9INxZRAp_U")
crs <- 4326

# reading in shapefile from map server
rail <-read_sf("C:/Users/nanzawi/OneDrive - HDR, Inc/Data_Science/MODOT_RR/inputs/MNARail.shp") %>% st_transform(crs)

rail <- st_geometry(rail)

# Function to create points on each side of the track and find route ---------------------------------------------------------
# name of route, point is location for start of route, offset from point, bufffer, rail_simplified the actual dataset

use_python("C:\\Users\\nanzawi\\AppData\\Local\\anaconda3\\python.exe")

# set wd
os <- import("os")
os$listdir(".")
print(os)
xingCloseRoute <- function(name, point, offset, buffer, rail) {
    # Perform intersection and cast to POINT
    navPoints <- point %>%
        st_buffer(buffer) %>%
        st_intersection(rail) %>%
        st_cast("POINT")

    # Convert to data frame with x, y coordinates
    coords_df <- st_coordinates(navPoints) %>%
        { as_tibble(.); setNames(., c("x", "y")) }


    # Use k-means to find two clusters among the points
    set.seed(123) # for reproducibility
    clusters <- kmeans(st_coordinates(navPoints), centers = 2)

    # Check if two clusters were found
    if (nrow(clusters$centers) < 2) {
        stop("k-means did not return enough centers.")
    }

    # Calculate the angle and create two new points on each side of the original point
    angle <- atan2(diff(clusters$centers[,2]), diff(clusters$centers[,1])) + pi/2
    points_df <- data.frame(
        point = c("side1", "side2"),
        x = mean(clusters$centers[,1]) + c(cos(angle), -cos(angle)) * offset,
        y = mean(clusters$centers[,2]) + c(sin(angle), -sin(angle)) * offset
    )
    sidePoints <- st_as_sf(points_df, coords = c("x", "y"), crs = st_crs(point))

    # Create a LINESTRING barrier
    barrier <- navPoints %>%
        st_union() %>%
        st_cast("LINESTRING") %>%
        st_buffer(10)

    # Calculate routes avoiding the barrier
    routeTest1 <- route(
        origin = sidePoints[1, ],
        destination = sidePoints[2, ],
        transport_mode = "car",
        routing_mode = "fast",
        results = 1,
        avoid_area = barrier
    ) %>% mutate(side = "side1")

    routeTest2 <- route(
        origin = sidePoints[2, ],
        destination = sidePoints[1, ],
        transport_mode = "car",
        routing_mode = "fast",
        results = 1,
        avoid_area = barrier
    ) %>% mutate(side = "side2")

    # Combine the routes and add metadata
    routes <- bind_rows(routeTest1, routeTest2) %>%
        mutate(name = name, .before = geometry)

    return(routes)
}



p <- tibble(id = "barrier", lat = 38.78329, lon = -94.61616) %>%
    st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326) %>% st_transform(crs)


test <- xingCloseRoute(name = "test run", point = p, offset = 200, buffer = 15, rail = rail)

tm_shape(test) + tm_lines(col = "side", palette = "Set1", lwd = 5)

# Other Test
xingCloseRoute <- function(name, point, offset, buffer, rail) {
    # Buffering the point and intersecting with rail
    bufferedPoints <- point %>% st_buffer(buffer) %>% st_intersection(rail)
    castedPoints <- bufferedPoints %>% st_cast("POINT")

    # Extracting coordinates and performing clustering
    coordinates <- castedPoints %>%
        transmute(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2]) %>%
        st_drop_geometry()
    clusteredPoints <- kmeans(coordinates, centers = 2, algorithm = "Lloyd")

    # Creating a tibble from cluster centers and calculating average and differences
    navPoints <- clusteredPoints$centers %>%
        as_tibble() %>%
        mutate(name = row_number(),
               xavg = mean(x),
               yavg = mean(y),
               xdiff = x[1] - x[2],
               ydiff = y[1] - y[2])

    # Calculating angles and new coordinates
    navPointsTransformed <- navPoints %>%
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
            xnew = c(x1[1], x2[1]),
            ynew = c(y1[1], y2[1])
        ) %>%
        st_as_sf(coords = c("xnew", "ynew"), crs = crs)

    # Creating a barrier
    barrier <- point %>%
        st_buffer(buffer) %>%
        st_cast("LINESTRING") %>%
        st_intersection(rail) %>%
        st_cast("POINT") %>%
        summarize() %>%
        st_cast("LINESTRING") %>%
        st_buffer(10)

    # Testing routes
    routeTest1 <- testRoute(navPoints, barrier, "side1")
    routeTest2 <- testRoute(navPoints, barrier, "side2")

    # Combining and returning the routes
    return(bind_rows(routeTest1, routeTest2) %>% mutate(name = name, .before = geometry))
}

# Function to test route
testRoute <- function(navPoints, barrier, side) {
    route(
        origin = navPoints %>% filter(point == side),
        destination = navPoints %>% filter(point != side),
        transport_mode = "car",
        routing_mode = "fast",
        results = 1,
        avoid_area = barrier
    ) %>% mutate(side = side)
}





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
