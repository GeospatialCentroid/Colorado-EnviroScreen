# Get superfund sites

library(arcpullr)
library(purrr)

# there are some invalid geometries that were not making buffers, st_make_valid works
npl <-
  arcpullr::get_spatial_layer(url = "https://services3.arcgis.com/66aUo8zsujfVXRIT/arcgis/rest/services/CDPHE_Colorado_Superfund_NPL_NRD/FeatureServer/0") %>% 
  st_transform(crs ="+proj=longlat +datum=WGS84" ) %>% 
  st_make_valid() %>% 
  dplyr::select(FID, POLLUTANTS, ID, NAME, SiteStatus)

npl_prj <- npl %>% 
  st_transform(crs = 5070)



# calculate values at census block level using cali enviroscreen methods

# but...sites were weighted on scale 0-12 by site type and status, higher weights
#assigned to superfund, state repsonse sites, and cleanups compared to evaluations, (as example)

#weights then adjusted based on distance from populated census blocks, Sites further
#than 1000m from any populated census block excluded

#site weights adjusted by multiplying weight by 1 sites <250m, 0.5 250-500m,
#0.25 500-750, 0.1 sites 750-1000 from nearest populated census blocks within a given tract.

#census tract scored based on sum of adjusted weights


#check for any un-populated census groups (NONE!)
getCensusAPIKey()
block_pop <- tidycensus::get_acs(
  geography = "block group", variables = "B01003_001", geometry = TRUE,
  state = "08", year = 2019)


##get block group geometry

blockGroups <- tigris::block_groups(state = "08") %>% 
  st_transform(crs ="+proj=longlat +datum=WGS84" ) %>% 
  dplyr::select(GEOID)


#create 250, 500, 750, and 1000 m

#buffer labels

buff_labs <- paste0(seq(250,1000, by = 250), "_km")

dist <- seq(250, 1000, by = 250)
weight <- c(1, 0.5, 0.2, 0.1)

buff_250 <- st_buffer(npl_prj, dist = 250) %>% st_transform(crs ="+proj=longlat +datum=WGS84")



buffers <- vector("list", length = length(dist))
intersect <- vector("list", length = length(dist)+1)

for (i in 1:length(dist)) {
  b <- st_buffer(npl_prj , dist[i]) %>% st_transform(crs ="+proj=longlat +datum=WGS84") %>% 
    mutate(dist = dist[i], weight = weight[i])
  
  buffers[[i]] <- b
  
  #buffers[[i]]$dist <- dist[i] #add the distance as attribute
  
  intersect[[i]] <- st_intersection(blockGroups, b)
}
#takes a while....

#also get direct intersections
intersect[[length(dist)+1]] <- st_intersection(blockGroups, npl) %>% 
  mutate(dist = 0, weight = 1)

#now combine into one df
intersect_all <- bind_rows(intersect)


# test out buffer distances, appear distance is correct but choppy
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = npl, popup = ~ID) %>% 
  addPolygons(data = buffers[[1]], color = "red") %>% 
  addPolygons(data = buffers[[2]], color = "yellow") %>% 
  #addPolygons(data =test, color = "yellow") %>% 
  addScaleBar()
  
# calculating in projected then plotting in wgs84 looks much better
leaflet() %>%
  addTiles() %>%
  addPolygons(data = blockGroups,
              color = "gray",
              popup = ~ GEOID) %>%
  addPolygons(data = npl, popup = ~ ID) %>%
  addPolygons(data = buffers[[1]], color = "red") %>%
  addPolygons(data = buffers[[2]], color = "yellow") %>%
  addScaleBar()




#now need to refine to distinct geoID/npl ID pairs, BUT if geoID overlaps
# npl multiple times, take the HIGHEST weight, so arrange by weight first (distinct keeps first row of duplicated columns)

npl_scores <- intersect_all %>% 
  arrange(desc(weight)) %>% 
  dplyr::distinct(GEOID, FID, .keep_all = TRUE) %>% 
  group_by(GEOID) %>% 
  mutate(npl_score = sum(weight)) %>% 
  distinct(GEOID, .keep_all = TRUE) %>% 
  dplyr::select(GEOID, npl_score)

st_geometry(npl_scores) <- NULL

#now tie back to block group geometry
blockGroups <- blockGroups %>% left_join(npl_scores, by = "GEOID")

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = blockGroups , color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.9,
              fillColor = ~colorNumeric("Blues", npl_score)(npl_score)) %>% 
  addPolygons(data = npl, color = "red")
