library(tidyverse)
library(sf)


#special note on the HERE R package.
#it defaults to add the calculation of tolls, which is makes the api cost $5 per 1,000
#With the package loaded run:

# trace("route", edit=TRUE)

#And remove the following part of the function in the editor window:

  # Add tolls (note: has to be added after &return=...)
 # if (transport_mode %in% c("car", "truck", "taxi", "bus")) {
#    url <- paste0(
#      url,
#      ifelse(
#        vignettes,
#        ",tolls&tolls[summaries]=total&currency=",
#        ",tolls&tolls[summaries]=total&tolls[vignettes]=all&currency="
#      ),
#      .get_currency()
#    )
#  }

#this way you can safely stay under the free tier for passenger vehicle routing

# Begin -----------------------------------------

library(hereR)

setwd("C:/Users/nanzawi/OneDrive - HDR, Inc/Data_Science/MODOT_RR/HERE_APIs/data/crossings_analysis")

#Here API key
hereR::set_key("xrcYx5euwWciYdK-coeSXcu16kjdJAujw9INxZRAp_U")

#clear environment
rm(list = ls())

# READ IN LINES OF BASE O-Ds -----------------------------------------

# read in moves and compare/contrast to where to remove there is not alt path/rail location is missing per annotations - 17

moves <- st_read("crossing_movements.shp") %>%
  #remove where there is no alternative path or rail location is missing per annotations
  filter(!Note %in% c("No Alt", "No rail", "No Routing")) %>%
  select(CrossingId)

view(moves)

# view
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
    tm_shape(moves) + tm_lines(lwd = 3, col = "darkred")

# calc from meters to feet by multiply .3048
moves$eu_distance <- st_length(moves) %>% as.numeric() * 0.3048
moves_straight_distance <- moves %>% st_drop_geometry()

moves <- moves %>%
  select(-eu_distance) %>%
  st_transform(4326)

# view
plot(moves)


### Create routeable points -----------------------------------------

# getting the number of rows in points
move_row <- nrow(moves)

#turning od lines into route able points
moves <- cbind(moves %>% st_coordinates() %>% as.data.frame(),
               CrossingId = map(1:move_row, function(x){rep(moves$CrossingId[x],2)}) %>% unlist()
               ) %>%
  mutate(L1 = rep(c("o","d"),move_row)) %>%
  select(CrossingId, part = L1, X, Y) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326)


#setting up avoid area polygons - buffered crossings that intsersect the moves routes
avoid <- st_read("avoid_areas.shp") %>%
  select(CrossingId) %>%
  filter(CrossingId %in% moves$CrossingId) %>%
  st_transform(4326)

# FUNCTION TO RUN ROUTES AND CREATE ALTERNATIVE PATH

crossing_analysis <- function(x){
 print(x)
  o <- moves %>% filter(part == "o" & CrossingId == x)
  d <- moves %>% filter(part == "d" & CrossingId == x)
  b <- avoid %>% filter(CrossingId == x)

  org <- route(origin = o,
               destination = d,
               datetime = Sys.time(),
               arrival = TRUE,
               results = 1,
               routing_mode = "fast",
               transport_mode = "car",
               traffic = FALSE)

  if (is.null(org)){print("no route")}else{org <- st_zm(org)}

  alt <- route(origin = o,
               destination = d,
               datetime = Sys.time(),
               arrival = TRUE,
               results = 1,
               routing_mode = "fast",
               transport_mode = "car",
               traffic =FALSE,
               avoid_area = b)



  if (is.null(alt)){print("no route")}else{alt <- st_zm(alt)}

  if (is.null(org)){
    data.frame(CrossingId =NA, scenario =NA, rank = NA, distance = NA, duration = NA)
  }else{
    rbind(
      org %>% transmute(CrossingId = x, scenario = "base", rank, distance, duration),
      alt %>% transmute(CrossingId = x, scenario = "closure", rank, distance, duration)
    )
  }


}


# RUN CROSSING ANALYSIS AND COLLECT RESULTS -----------------------------------------

# note - If no traffic is selected, the \code{datetime} is set to \code{"any"} and the request is processed independently from time.
df2 <- map_df(unique(moves$CrossingId), crossing_analysis)
view(df2)


df %>% st_write("data/results.shp")

df <- st_read("results.shp")
view(df)

metrics_df <- df %>%
  select(-rank) %>%
  st_drop_geometry() %>%
  group_by(CrossingId) %>%
  pivot_wider(names_from = "scenario", values_from = c("distance","duration")) %>%
  ungroup() %>%
  left_join(moves_straight_distance) %>%
  mutate(circuity_base = distance_base / eu_distance) %>%
  mutate(circuity_closure = distance_closure / eu_distance) %>%
  mutate(distance_change = distance_closure - distance_base) %>%
  mutate(duration_change = duration_closure - duration_base) %>%
  mutate(circuity_change = circuity_closure - circuity_base) %>%
  mutate(circuity_pctchg = circuity_change / circuity_base) %>%
  mutate(distance_pctchg = distance_change / distance_base) %>%
  mutate(duration_pctchg = duration_change / duration_base)


output_path <- file.path("C:", "Users", "nanzawi", "OneDrive - HDR, Inc", "Data_Science", "MODOT_RR", "HERE_APIs", "output")
metrics_df %>%
  write_csv(file.path(output_path, "metrics.csv"))


####
# calculate isochrones by poit of interest (poi) which is the centroid of the barrier, checks to see if geos are valid, sets range to values in seconds
trace("isoline", edit=TRUE)
isoTest <-
    isoline(
        poi = avoid %>% st_centroid(),
        range_type = "time",
        range = c(5, 10, 15, 20) * 60,
        routing_mode = "fast",
        transport_mode = "car",
        traffic = FALSE
    ) %>%
    mutate(minutes = as.integer(range / 60))

st_is_valid(avoid)

