###
# evaluate water district violations
# carverd@colostate.edu
# 20211007
###
pacman::p_load(dplyr, sf)

# districts 
f1 <- list.files("data/waterDistricts/SHP",pattern = ".shp", full.names = TRUE)
f1 <- f1[!grepl(pattern = ".xml", x = f1)]
d1 <-  lapply(X = f1, sf::st_read) 

# water and san districts 
d2 <- bind_rows(d1[[4]], d1[[5]], d1[[6]])%>%
  as.data.frame()%>%
  dplyr::select(-geometry)

# violations 
v1 <- read.csv("data/waterQuality/Drinking water violations open 092221.csv")

v1$match <- NA
for(i in seq_along(v1$Name)){
  if(grepl(v1$Name[i], x = d2$NAME)){
    v1$match[i] <- TRUE
  }else{
    v1$match[i] <- FALSE
  }
}



v2 <- v1 %>%
  dplyr::group_by(Violation.Name) %>%
  dplyr::summarise(count = n())
write.csv(v2, "data/waterQuality/DrinkingWater_summary.csv")
