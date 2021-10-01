###
# buffer spatial features and summarize all weighted intersections 
# carverd@colostat.edu
# 20211001
###

### The distance and weight values need to be ranked relative to each other 
### as they are assigned based on index. 
# ex. 
# weight <- c(1,0.5,0.25,0.1)
# dist <- c(250,500,750,1000)
# all distances are in meters 


bufferObjects <- function(bufferFeature, geometry, dist, weight){
  ### Preforms and intersect across various distances and returns all intersections 
  # bufferFeatures : the spatial object you are looking to buffer
  # geometry : the goemetry object to test buffer intersections agains
  # dist : vector of number values for distance in meters. 
  # weight : vector of numeric values to apply at various distances 
  ###
  # test for correct CRS 
  if(sf::st_crs(bufferFeature) != st_crs(5070) |sf::st_crs(geometry) != st_crs(5070)){
    return(print("Please ensure that both spatial objects are project in ESPG 5070 before attempting this function"))
    }else{
        
      # create list to hold output results from buffering process 
      intersect <- vector("list", length = length(dist))
      
      #itorate over the buffer distances 
      for (i in 1:length(dist)) {
        # generate the buffer and asign relative weight 
        b <- st_buffer(bufferFeature, dist[i]) %>% 
          mutate(dist = dist[i], weight = weight[i])
        
        # test for the intersection between buffered object and geometry feature
        intersect[[i]] <- st_intersection(geometry, b)
      }
      
      
      #now combine into one df
      intersect_all <- bind_rows(intersect)

      return(intersect_all)
    }
}
