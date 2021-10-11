###
# testing the spatial balance between other geographies 
# carverd@colostate.edu
# 20211006
### 

# voting districts 
votingDistricts <- tigris::voting_districts(state = "CO")
qtm(votingDistricts)

# school districts 
schoolDistricts <- tigris::school_districts(state = "CO")
qtm(schoolDistricts)


tmap_mode("view")
# geometries  
gCBG <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
gCT <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
gC <- sf::st_read("data/county/coloradoCounties.geojson")
gB1 <- tigris::blocks(state = "CO", year = 2019)


gB <- gB1 %>%
  dplyr::filter(COUNTYFP %in% c("109", "027", "059"))


# school districts -- do not match
tm_shape(schoolDistricts)+
  tm_polygons()+
  # tm_shape(gCBG)+
  # tm_polygons(col = "red",alpha = 0.2)
  # tm_shape(gCT)+
  # tm_polygons(col = "green",alpha = 0.2)
  # tm_shape(gC)+
  # tm_polygons(col = "blue", alpha = 0.2)+
  tm_shape(gB)+
  tm_polygons(col = "yellow", alpha = 0.2)

# voting districts -- do not match
tm_shape(votingDistricts)+
  tm_polygons()+
  # tm_shape(gCBG)+
  # tm_polygons(col = "red",alpha = 0.2)
  # tm_shape(gCT)+
  # tm_polygons(col = "green",alpha = 0.2)
# tm_shape(gC)+
# tm_polygons(col = "blue", alpha = 0.2)+
tm_shape(gB)+
  tm_polygons(col = "yellow", alpha = 0.2)

