# Final Routing Interpolation script
# Built off of 2.4 NJDOT Truck Counts, but moved here to make for a cleaner file

library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")
library(hereR)

library(lwgeom)
library(readxl)


# Data Import and Prep ----------------------------------------------------

setwd("C:\Users\nanzawi\OneDrive - HDR, Inc\Data_Science\MODOT_RR\HERE_APIs")
set_key("oSlYpCzDHTENyvJB0yWUT9NTr6q4oGWdzTRpAsz1Vy0")

# Load Freight Trip Generation (Block Level Data)
load("SJTPO_blocksFTG.RData")

# Load Pop and Other Base Data
load("baseData.RData")

# Load original classification count location file
load("countDataJoin.Rdata")
countDataJoin <-
  countDataJoin %>% select(si_station_num, adt, truckADT, comboADT) %>% mutate(type = "Original")

# Import the new points added through ArcGIS
countDataJoinExtra <-
  read_sf("ArcGIS/CountLocEdits/CountLocsForEdit2.shp") %>%
  filter(row_number() > 288) %>%
  mutate(
    si_station_num = paste0("extra", row_number()),
    type = if_else(row_number() < 22, "Collection", siteName)
    )

# Import the Cumberland County Truck Study Counts from Local Drive (already downloaded)
CCC_East <- read_sf("FromTeams/ClassificationCounts/ForGIS/CCC_East.shp") %>% st_transform(crs)
CCC_West <- read_sf("FromTeams/ClassificationCounts/ForGIS/CCC_West.shp") %>% st_transform(crs)

tm_shape(CCC_West) + tm_dots(col = "red") +
  tm_shape(CCC_East) + tm_dots(col = "blue")


CCC <- bind_rows(CCC_East, CCC_West) %>%
  transmute(
    si_station_num = site2,
    adt = trafAll,
    truckADT = trafTruck,
    comboADT = trafCombo,
    type = "CCC"
  )

# create esri basemap
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(CCC) + tm_dots()

# Import FAF Points
load("fafNetworkSJTPOdots.RData")
fafNetworkSJTPOdots <-
  fafNetworkSJTPOdots %>% st_cast("POINT") %>% st_transform(crs) %>%
  transmute(
    si_station_num = paste0("FAF_", row_number()),
    adt = tot_trips_22_all / 0.07, # ADT estimate based on higher truck volume roads being around 7% trucks
    truckADT = tot_trips_22_all,
    comboADT = tot_trips_22_cu,
    type = "FAF"
  )

fafNetworkSJTPOdots
tm_shape(fafNetworkSJTPOdots) + tm_dots() +
  tm_shape(fafNetworkSJTPOdots %>% filter(si_station_num == "FAF_299")) + tm_dots(col = "red")


# Chart of ADT vs. Truck Percentage
countDataJoin %>% st_drop_geometry() %>% transmute(perc = truckADT / adt, truckADT=truckADT, adt=adt) %>%
  ggplot(aes(x = adt, y = perc)) + geom_point()

# Combine all data together for analysis
countDataFinal <-
  bind_rows(countDataJoin, countDataJoinExtra, CCC, fafNetworkSJTPOdots) %>% select(-siteName) %>%
  mutate(
    si_station_num = replace_na(si_station_num, "none"),
    si_station_num = if_else(si_station_num == "none", paste0("ERN_", row_number()), si_station_num)
  ) %>%
  filter(
    si_station_num != "160605",
    type != "dummy"
  )

tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(countDataFinal) + tm_dots(col = "type", popup.vars = TRUE)

countDataFinal %>% st_drop_geometry() %>% count(type) %>% arrange(desc(n))

countDataFinal



# Incorporation of FTG Estimates to Create New Points -------------------------------
tmap_mode("view")
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(countDataFinal) + tm_dots() +
  tm_shape(SJTPO_blocksFTG) + tm_polygons(col = "FTG", alpha = 0.5) + tm_dots(col = "red") +
  tm_add_legend(
    type = "fill",
    labels = c("Classification Count", "FTG Location"),
    col = c("black", "red")
  )

# Process assumes that FTG block centroids can be treated as if they are actual
# point count locations. FTG estimates don't include combo ADT estimate, so that
# is estimated based on average percentages of surrounding count locations.

countDataFinal_FTG <-
  SJTPO_blocksFTG %>%  #Take the SJTPO_blocksFTG GeoDataFrame
  st_transform(crs) %>% # Transform its coordinates to the specified CRS
  st_centroid %>%  # Calculate the centroid of each geometry
  transmute(
    si_station_num = paste0("FTG_", row_number()), # Generate unique station numbers
    adt = 0, #initalize adt col
    truckADT = FTG, #copy values from FTG col
    comboADT = 0
    )
tm_shape(countDataFinal_FTG[1,] %>% st_buffer(5280*2)) + tm_borders() + tm_dots() +
  tm_shape(countDataFinal) + tm_dots(col = "red")

comboForJoin = tibble()
for(i in 1:nrow(countDataFinal_FTG)) {
  new <- countDataFinal_FTG[i,] %>% st_buffer(5280*2) %>% st_intersection(countDataFinal) %>% st_drop_geometry() %>%
    summarize(comboADT = sum(comboADT.1) / sum(truckADT.1) * countDataFinal_FTG[i,]$truckADT) %>% round()

  comboForJoin <- bind_rows(comboForJoin, new)
}

countDataFinal_FTG <- countDataFinal_FTG %>% mutate(comboADT = comboForJoin$comboADT)

countDataFinal <- bind_rows(countDataFinal, countDataFinal_FTG)

tm_shape(countDataFinal) + tm_dots(col = "type", popup.vars = TRUE)


# Start Here If Data Prepped ----------------------------------------------

save(countDataFinal, file = "countDataFinal.RData")
load("countDataFinal.RData")

# Actual Routing ----------------------------------------------------------

target <- "674-1"
X <- 20
RouteNearest <- function(target, X) {

  targetADT <- countDataFinal %>% st_drop_geometry() %>% filter(si_station_num == target) %>% select(adt) %>% pull()
  targetTruckADT <- countDataFinal %>% st_drop_geometry() %>% filter(si_station_num == target) %>% select(truckADT) %>% pull()
  targetComboADT <- countDataFinal %>% st_drop_geometry() %>% filter(si_station_num == target) %>% select(comboADT) %>% pull()

  # Find nearest X features
  NearestX <-
    countDataFinal %>%
    bind_cols(st_distance(countDataFinal, countDataFinal %>% filter(si_station_num == target))) %>%
    rename(dist = starts_with("...")) %>%
    mutate(rank = rank(dist)) %>%
    filter(between(rank, 2, X+1)) %>%
    mutate(id = row_number())

  # Create duplicate starting points to match number of X features
  StartingX <- countDataFinal %>% filter(si_station_num == target) %>% slice(rep(1, each = X))

  # Call HERE routing API, find endpoints, computes a route from the StartingX points to the NearestX points using the "car" transport mode, avoid toll roads
  routeAPI <- route(origin = StartingX, destination = NearestX,
                    transport_mode = "car", avoid_feature = "tollRoad", routing_mode = "fast") %>%
    mutate(circuity = distance * 3.28084 / NearestX$dist) %>%
    filter(circuity < 3) #, filter for routes with ciruitry less than 3
  routeAPIEnds <- routeAPI %>% st_drop_geometry() %>% bind_cols(routeAPI %>% st_endpoint()) %>% st_as_sf() #store in routeAPIEnds df

#create a FinalEnds dataframe from the route api dataframe with a buffer set at 20 and spatially join to routeAPIEnds dataframe. group by data id, calculate count of intersecting features
  FinalEnds <- routeAPI %>% st_buffer(20) %>% st_join(routeAPIEnds %>% select(rank), join = st_intersects) %>%
    group_by(id) %>% summarize(count = n()) %>% filter(count == 1) %>% st_drop_geometry()

#NearestX data frame created, filters rows where  id == id in FinalEnds df
NearestXFinal <-
    NearestX %>% filter(id %in% FinalEnds$id) %>%
    st_drop_geometry() %>% #drop geometry
    transmute( #create new columns with info from routeAPI df
      target = target,
      targetADT = targetADT,
      targetTruckADT = targetTruckADT,
      targetComboADT = targetComboADT,
      id = id,
      propTargetADT = round((adt / sum(adt)) * targetADT, 0), # Not actually a proportion
      propTargetTruck = paste0(round(truckADT / sum(truckADT) * 100, 1), "%"),
      propTargetCombo = paste0(round(comboADT / sum(comboADT) * 100, 1), "%"),
      estADT = round((adt / sum(adt)) * targetADT, 0),
      estTruckADT = round((truckADT / sum(truckADT)) * targetTruckADT, 0),
      estComboADT = round((comboADT / sum(comboADT)) * targetComboADT, 0)
    ) %>%
    left_join(routeAPI %>% select(id)) %>% st_as_sf() #conver to spatial dataframe as st_as_sf. NOTE: I don't know much about R, but similar to python someitmes aggregate functions inherent to packages/the programing language don't work onspatial dataframes so you hae to make the dataframe non-spatial, perform these functions, and then make it spatial again

  return(NearestXFinal)

}
#tm_shape(NearestXFinal) + tm_lines(col = "propTargetADT")


# Map check to confirm results
sta1 <- "8-4-431"
sta1 <- "8-6-116"
sta1 <- "552-1"
sta1 <- "552S-2"
sta1 <- "684-2"
sta1 <- "FTG_197"
sta1 <- "674-1"
sta1 <- "FAF_238"
sta1 <- "8-8-032"
sta1 <- "FAF_343"
routeTest <- RouteNearest(sta1, 50)
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(routeTest) + tm_lines(lwd = "estTruckADT", col = "estTruckADT", scale = 10, palette = "Reds", popup.vars = TRUE)+
  tm_shape(routeTest %>% st_endpoint()) + tm_dots(col = "red", size = 0.2) +
  tm_shape(countDataFinal %>% filter(si_station_num == sta1)) + tm_dots(col = "green", size = 1) +
  tm_shape(countDataFinal) + tm_dots() + tm_text("si_station_num", ymod = 1)


# Apply routing function to all Classification count locations
# Running this was resulting in errors: features with invalid spherical geometry
      # start <- Sys.time()
      # SJTPO_Truck_RoutingPart1 <- map_dfr(countDataFinal$si_station_num, ~RouteNearest(., 12))
      # Sys.time() - start

# Retry this as a loop with error handling
SJTPO_Truck_Routing01 <- map_dfr(countDataFinal$si_station_num[1:1], ~RouteNearest(., 20))
default <- SJTPO_Truck_Routing01[1,] %>% mutate(target = "error")

    # start <- Sys.time()
    # SJTPO_Truck_Routing <- default
    # for(i in 1:nrow(countDataFinal)) {
    #   t <- tryCatch((RouteNearest(countDataFinal$si_station_num[i], 20)), error = function(e) default)
    #   SJTPO_Truck_Routing <- bind_rows(SJTPO_Truck_Routing, t)
    #   print(i)
    # }
    # Sys.time() - start
    #
    # save(SJTPO_Truck_Routing, file = "SJTPO_Truck_Routing3.RData")
load("SJTPO_Truck_Routing3.RData")


# Figure of all the different points used for the routing interpolation by filtering on dataframe specifics
tmap_mode("plot")
tm_shape(basetile, unit = "imperial") + tm_rgb() +
  tm_shape(SJTPO_Truck_Routing) + tm_lines() +
    tm_shape(countDataFinal %>% mutate(
      type = case_when(
        type == "border" ~ "Dummy Points",
        type == "CCC" ~ "Cumberland County Truck Study",
        type == "Collection" ~ "Collected for Study",
        type == "new" ~ "NJDOT Counts",
        type == "Original" ~ "NJDOT Counts",
        type == "FAF" ~ "FAF Estimates",
        TRUE ~ "Freight Trip Generation Estimates"
      )
    )) + tm_dots(col = "type", size = 0.15, palette = c("gray50", "red", "black", "orange", "purple"), title = "Count Type") +
  DefMapSet
tmap_save(filename = "Plot/Draft/Tech Memo 1/2.3.X CountLocations4.png", width = 7, height = 6.3, dpi = 300, units = "in")



# Union and Process Final Network -----------------------------------------

# Create buffer of the routing shapes
SJTPO_Truck_Routing_Buff <-
  SJTPO_Truck_Routing %>% # from routing daataframe set crs and create a buffer of 90 (miles?km?_ into a shape file
  st_transform(crs) %>% st_buffer(20) %>% st_transform(4326)
tm_shape(SJTPO_Truck_Routing_Buff) + tm_polygons()

# Export to ArcGIS to complete full Union;
# Have not yet figured out how to duplicate this functionality in R
SJTPO_Truck_Routing_Buff %>% write_sf("Spatial/Vector/ForGIS/SJTPO_Truck_Routing_Buff_v3.shp")

SJTPO_Truck_Routing_Buff_Union <- read_sf("Spatial/Vector/ForGIS/SJTPO_Truck_Routing_Buff_v3_Union.shp") #upload from local drive

# Create single-feature version of the routing spatial file, then simplify
SJTPO_Truck_Routing_Summarize <- SJTPO_Truck_Routing %>% select(geometry) %>% st_union()
SJTPO_Truck_Routing_Summarize_SF <- SJTPO_Truck_Routing_Summarize %>% st_as_sf() #%>% rmapshaper::ms_simplify()

tm_shape(SJTPO_Truck_Routing_Summarize_SF) + tm_lines()

save(SJTPO_Truck_Routing_Summarize_SF, file = "SJTPO_Truck_Routing_Summarize.RData")
load("SJTPO_Truck_Routing_Summarize.RData")

      # # Intersect the simplified roadway spatial file with the Unioned buffer shapes
      # start <- Sys.time()
      # roadIntersect <- st_intersection(SJTPO_Truck_Routing_Summarize_SF, SJTPO_Truck_Routing_Buff_Union)
      # roadIntersect2 <-
      #   roadIntersect %>% st_as_sf() %>% st_transform(crs) %>%
      #   mutate(
      #     segID = row_number(roadIntersect),
      #     length = st_length(.)
      #   ) %>%
      #   filter(as.numeric(length) > 50)
      # Sys.time() - start

# Create id based on the shape and length of the union
# In theory, this should allow for summarizing across individual same shapes
# to generate a single shape with the volumes estimates
sf_use_s2(FALSE)
mergeUnion <-
  SJTPO_Truck_Routing_Buff_Union %>% mutate(id = paste0(Shape_Leng, "-", Shape_Area)) %>%
  #st_drop_geometry() %>%
  filter(Shape_Leng > 0.001) %>% group_by(id) %>%  @ grup by locations in union that are larger than .001
  summarize(
    count = n(),
    estADT = sum(estADT), # get summary stats for regular ADT, truck ADT, and call adt these next fe wlines
    estTADT = sum(estTADT),
    estCADT = sum(estCADT)
    )
save(mergeUnion, file = "mergeUnion.RData")
load("mergeUnion.RData")

# Intersect the merged union above with the roadway line network
mergeUnionLines <- SJTPO_Truck_Routing_Summarize_SF %>% st_intersection(mergeUnion) # perform a spatila intersction where only overlapping geoemetries are kept

# sho win es
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(mergeUnionLines) + tm_lines(col = "estTADT", style = "jenks", lwd = 3, popup.vars = TRUE) +
  tm_shape(countDataFinal %>% st_buffer(50)) + tm_borders() + tm_text("truckADT")

smoothPoints <-
  countDataFinal %>% st_buffer(50) %>%
  st_intersection(mergeUnionLines %>% st_transform(crs)) %>%
  st_cast("POINT") %>% st_drop_geometry() %>%
  mutate(
    adjADT = round(estADT + (adt - estADT)/2, 0),
    adjTADT = round(estTADT + (truckADT - estTADT)/2, 0),
    adjCADT = round(estCADT + (comboADT - estCADT)/2, 0)
    ) %>%
  group_by(id) %>% summarize(
    adjADT = mean(adjADT),
    adjTADT = mean(adjTADT),
    adjCADT = mean(adjCADT)
  )

mergeUnionLinesAdj <-
  mergeUnionLines %>% left_join(smoothPoints) %>%
  transmute(
    id,
    finADT = if_else(is.na(adjADT), estADT, adjADT),
    finTADT = if_else(is.na(adjTADT), estTADT, adjTADT),
    finCADT = if_else(is.na(adjCADT), estCADT, adjCADT),
    percTruck = finTADT/finADT,
    above10perc = if_else(percTruck >= 0.1, "Yes", "No")
  ) %>%
  st_intersection(counties %>% st_transform(4326))

tmap_mode("view")
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(mergeUnionLinesAdj) + tm_lines(col = "finTADT", style = "jenks", lwd = 3, popup.vars = TRUE) +
  tm_shape(countDataFinal) + tm_dots() + tm_text("truckADT")

mergeUnionLinesAdj %>% save(file = "mergeUnionLinesAdj.RData") # save tot R file
mergeUnionLinesAdj %>% write_sf_zip("Spatial/Vector/ForGIS", "FinalTruckEstimatesV2")  # save to ziopped shp file


mergeUnionLinesAdj %>% filter(finADT > 0) %>% select(above10perc) %>% write_sf_zip("Spatial/Vector/ForGIS", "Above10Perc")
countDataFinal %>% write_sf_zip("Spatial/Vector/ForGIS", "countDataFinal")
countDataFinal %>% mutate(truckPerc = truckADT / adt) %>% #save as a filtered county object to shp file
  filter(truckPerc >= 0.1, type %in% c("Original", "CCC", "Collection")) %>%
  write_sf_zip("Spatial/Vector/ForGIS", "countDataFinal_10plus")
counties %>% write_sf_zip("Spatial/Vector/ForGIS", "SJTPO_Counties")

load("mergeUnionLinesAdj.RData") #load rdata from a databse (not sure where)

read_sf_zipLoc <- function(path) {  # read in a zipped shape file from local drive and unzip
  temp <- tempfile()
  unzip(zipfile = path, exdir = temp)
  read_sf(temp)
}

mergeUnionLinesAdj <- read_sf_zipLoc("Spatial/Vector/ForGIS/FinalTruckEstimates.zip") # read in shape file from local drive
mergeUnionLinesAdj <-
  mergeUnionLinesAdj %>%
  mutate(prcTrck == if_else(prcTrck != Inf, prcTrck, NA))

# Final Maps --------------------------------------------------------------

# all maps being made below are using a basemap set with rgb coloring, and showing the estimated adt for trucks, combination, and ars
# each function follows the same formula: 4
## (1) take the merged cumberland county truck study counts from the local drive via teams, (2) use the basetile map in imperial units, (3) add county file and only show borders, (4) add lines to map and (5) display

tmap_mode("view")
tmap_mode("plot")
# Truck ADT
tm_shape(basetile, unit = "imperial") + tm_rgb() +
  tm_shape(counties) + tm_borders() +
  tm_shape(mergeUnionLinesAdj) + tm_lines(col = "finTADT", lwd = 3, style = "jenks", palette = "YlOrRd", title.col = "Estimated Truck ADT") +
  tm_shape(basetileLabels) + tm_rgb(alpha = 0.8) +
  DefMapSet
tmap_save(filename = "Plot/Draft/Tech Memo 1/2.X Final Estimated Truck ADT.png", width = 7, height = 6.3, dpi = 300, units = "in")

# Combination Truck ADT
tm_shape(basetile, unit = "imperial") + tm_rgb() +
  tm_shape(counties) + tm_borders() +
  tm_shape(mergeUnionLinesAdj) + tm_lines(col = "finCADT", lwd = 3, style = "jenks", palette = "YlOrRd", title.col = "Estimated Combination ADT") +
  tm_shape(basetileLabels) + tm_rgb(alpha = 0.8) +
  DefMapSet
tmap_save(filename = "Plot/Draft/Tech Memo 1/2.X Final Estimated Combination ADT.png", width = 7, height = 6.3, dpi = 300, units = "in")

# Percent Trucks
tm_shape(basetile, unit = "imperial") + tm_rgb() +
  tm_shape(counties) + tm_borders() +
  tm_shape(mergeUnionLinesAdj %>% filter(prcTrck <= 1)) + tm_lines(col = "prcTrck", lwd = 3, breaks = c(0, 0.1, 0.2, 0.3, 1), palette = "YlOrRd", title.col = "Estimated Truck Percentage") +
  tm_shape(basetileLabels) + tm_rgb(alpha = 0.8) +
  DefMapSet
tmap_save(filename = "Plot/Draft/Tech Memo 1/2.X Final Estimated Truck PercentV2.png", width = 7, height = 6.3, dpi = 300, units = "in")

# Percent Trucks with Dots
tm_shape(basetile, unit = "imperial") + tm_rgb() +
  tm_shape(counties) + tm_borders() +
  tm_shape(mergeUnionLinesAdj %>% filter(prcTrck <= 1)) + tm_lines(col = "prcTrck", lwd = 3, breaks = c(0, 0.1, 0.2, 0.3, 1), palette = "YlOrRd", title.col = "Estimated Truck Percentage") +
  tm_shape(basetileLabels) + tm_rgb(alpha = 0.8) +
  DefMapSet
tmap_save(filename = "Plot/Draft/Tech Memo 1/2.X Final Estimated Truck PercentV2.png", width = 7, height = 6.3, dpi = 300, units = "in")



# For illustration
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(mergeUnionLinesAdj) + tm_lines(col = "finCADT", lwd = 3, style = "jenks", palette = "YlOrRd", title.col = "Estimated Combination ADT") +
  tm_shape(countDataFinal) + tm_dots()


tmap_mode("view")
tm_shape(mergeUnionLinesAdj) + tm_lines(col = "prcTrck", lwd = 3) +
  tm_shape(countDataJoin) + tm_dots()
