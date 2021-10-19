



geom <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")


# districts 
f1 <- list.files("data/waterDistricts/SHP",pattern = ".shp", full.names = TRUE)
f1 <- f1[!grepl(pattern = ".xml", x = f1)]
d1 <-  lapply(X = f1, sf::st_read) 

# water and san districts 
d2 <- bind_rows(d1[[4]], d1[[5]], d1[[6]])

s1 <- d2 %>%
  sf::st_transform(crs = sf::st_crs(geom))%>%
  sf::st_make_valid()


geom$overLap <- st_intersects(geom, s1 )

g2 <- geom %>%
  as.data.frame()%>%
  dplyr::select(-geometry) %>%
  dplyr::mutate(
    ol2 = as.character(overLap),
    match = case_when(
      ol2 == "integer(0)" ~ TRUE,
      TRUE ~ FALSE
    )
  )
View(g2)

g2a <-g2 %>%
  dplyr::group_by(ol2)%>%
  dplyr::summarise(count = n())

write.csv(g2a, file = "oneOffContent/waterDistrictOverlap.csv", row.names = FALSE)


t2 <- st_intersection(geom, s1)



