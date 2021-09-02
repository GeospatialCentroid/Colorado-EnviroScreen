# Get superfund sites

library(arcpullr)


npl <-
  arcpullr::get_spatial_layer(url = "https://services3.arcgis.com/66aUo8zsujfVXRIT/arcgis/rest/services/CDPHE_Colorado_Superfund_NPL_NRD/FeatureServer/0")


plot(npl[1])


# calculate values at census block level using cali enviroscreen methods

# but...sites were weighted on scale 0-12 by site type and status, higher weights
#assigned to superfund, state repsonse sites, and cleanups compared to evaluations, (as example)

#weights then adjusted based on distance from populated census blocks, Sites further
#than 1000m from any populated census block excluded

#site weights adjusted by multiplying weight by 1 sites <250m, 0.5 250-500m,
#0.25 500-750, 0.1 sites 750-1000 from nearest populated census blocks within a given tract.

#census tract scored based on sum of adjusted weights

##get block groups

blockGroups <- tigris::block_groups(state = "08") %>% 
  st_transform(crs ="+proj=longlat +datum=WGS84" )


# add multi-buffer to polygons

#buffer labels

buff_labs <- paste0(seq(250,1000, by = 250), "_km")


buff_250 <- st_buffer(npl, dist = 250) #%>% st_simplify(preserveTopology = FALSE,
                                                       #dTolerance = 250)


leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = npl) %>% 
  addPolygons(data = buff_250, color = "red") %>% 
  addPolygons(data =test, color = "yellow") %>% 
  addScaleBar()


#
test <- st_intersection(blockGroups, buff_250)

#then if geoid is in test, assign it (0.25) in block group and the associated
#site ID (so know if duplicates, take highest value per site ID)
                        
                        


#check for any un-populated census groups (NONE!)
getCensusAPIKey()
block_pop <- tidycensus::get_acs(
  geography = "block group", variables = "B01003_001", geometry = TRUE,
  state = "08", year = 2019)
  
