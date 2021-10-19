###
# evaluate ecoli data for the state 
# carverd@colostate.edu
# 20211008
###
library(sf)
library(tmap)
library(dplyr)
library(plotly)

d1 <- st_read("E:/geoSpatialCentroid/coEnvrioScreen/src/Colorado-EnviroScreen/data/waterQuality/colorado_ecoli_wqp.csv", 
              options=c("X_POSSIBLE_NAMES=long","Y_POSSIBLE_NAMES=lat"))%>%
  st_set_crs(4326)
  

nSites <- length(unique(d1$SiteID))

uniqueSites <- d1 %>%
  dplyr::mutate(
    ecoli1 = as.numeric(value)
  )%>%
  dplyr::filter(!is.na(ecoli1))%>%
  dplyr::arrange(desc(Date))%>%
  dplyr::distinct(SiteID, .keep_all = TRUE)%>%
  dplyr::mutate(
    ecoliConcern = case_when(
      ecoli1 > 88 ~ TRUE,
      TRUE ~ FALSE
    )
  )

tmap_mode("view")

tm_shape(uniqueSites)+
  tm_dots(col = "ecoliConcern", popup.vars = c("SiteID","Date","value"))

hist1 <- uniqueSites %>%
  tidyr::separate(col = Date, sep = "-",into = c("year","month","day"))%>%
  dplyr::group_by(year)%>%
  dplyr::summarise(count = n())%>%
  as.data.frame()%>%
  dplyr::select(-geometry)

plot_ly(data = hist1$year,
         type = "histogram")
  