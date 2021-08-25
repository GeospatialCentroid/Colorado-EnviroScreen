###
# reviewing the haps data
# 20210819 
# carverd@colostate.edu
###
library(dplyr)
library(sp)
library(tmap)
tmap_mode("view")
haps <- read.csv("F:/geoSpatialCentroid/coEnvrioScreen/data/state/hapsData.csv")

top10 <- c(
  "formaldehyde",  "ethylene oxide",
  "chloroprene",  "1,3-butadiene",
  "acetaldehyde",  "benzene",
  "carbon tetrachloride",  "napthalene",
  "1,4-dichlorobenzene",  "arsenic compounds",
  "chromium VI compounds",  "coke oven emissions",
  "ethylbenzene",  "chlorine",
  "hexamethylene diisocyanate")
ethlylBenzene <- "site_100414_estim"
formaldehyde <- "site_50000_estim"
benzene <- "site_71432_estim"

haps2 <- haps %>%
  dplyr::select(1:11, "SITE_100414_ESTIM","SITE_50000_ESTIM","SITE_71432_ESTIM")%>%
  dplyr::rename(ethlylBenzene = SITE_100414_ESTIM,
                formaldehyde = SITE_50000_ESTIM,
                benzene = SITE_71432_ESTIM )
  
View(haps2)
names <- c("ethlylBenzene","formaldehyde","benzene")


t1 <- haps2 %>%
  dplyr::count(!is.na(ethlylBenzene))
t2 <- haps2 %>%
  dplyr::count(!is.na(formaldehyde))
t3 <- haps2 %>%
  dplyr::count(!is.na(benzene))


df<-data.frame(matrix(nrow = 3, ncol = 3))
names(df) <- c("contaniment", "recorded", "notRecorded" )
df[1,] <- c( "ethlylBenzene", t1$n[2], t1$n[1])
df[2,] <- c( "formaldehyde", t2$n[2], t2$n[1])
df[3,] <- c( "benzene", t3$n[2], t3$n[1])

coords <- haps2[,c("SITE_X_COORDINATE", "SITE_Y_COORDINATE")]
haps_SP <- sp::SpatialPointsDataFrame(
  coords = coords,
  data = haps2,
  proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
)
# drop day lat long 
haps_SP<- haps_SP[haps_SP$SITE_X_COORDINATE != 0.0000, ]


### create a map to show three examples 
# create three objects 
ethlylBenzene <-  haps_SP[,c("APCD_SITE_ID", "ethlylBenzene")]
ethlylBenzene <- ethlylBenzene[!is.na(ethlylBenzene$ethlylBenzene),]
formaldehyde <- haps_SP[,c("APCD_SITE_ID", "formaldehyde")]
formaldehyde <- formaldehyde[!is.na(formaldehyde$formaldehyde),]
benzene <- haps_SP[,c("APCD_SITE_ID", "benzene")]
benzene <- benzene[!is.na(benzene$benzene),]

map <- tm_shape(ethlylBenzene) +
  tm_dots(col = "ethlylBenzene", style = "quantile", n = 8)+
  tm_shape(formaldehyde) +
  tm_dots(col = "formaldehyde", style = "quantile", n = 8)+
  tm_shape(benzene) +
  tm_dots(col = "benzene", style = "quantile", n = 8)+
  tm_shape(haps_SP)+
  tm_dots(col = "blue")
map



