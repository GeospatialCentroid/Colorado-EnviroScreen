###
# generate map element the contains all the features of the shiny app
# 20211103
# carverd@colorstate.edu
### 

# spatial data 
geometry <- setSpatialData(dataFolder = "data/",scale = "county")%>%
  sf::st_transform(crs = 4326)


# enviroscreen data 
d1 <- vroom("data/envScreenScores/county.csv")%>%
  dplyr::mutate(GEOID = as.character(GEOID))

# join datasets 
geom <- geometry %>%
  dplyr::select("GEOID","NAME") %>%
  dplyr::left_join(y = d1, by = "GEOID")


# map parameters/inputs  
## purple low green high
colorRamp <- c(
  "#40004b","#762a83","#9970ab","#c2a5cf","#e7d4e8","#d9f0d3",
  "#a6dba0","#5aae61","#1b7837","#00441b"
)
# purple high
colorRamp2 <- c(
  "#00441b","#1b7837","#5aae61","#a6dba0","#d9f0d3","#e7d4e8",
  "#c2a5cf","#9970ab","#762a83","#40004b"
)


# example with hard coded indicator value 

pal <- colorNumeric(
  palette = colorRamp,
  domain = geom$pm25_pcntl
  #reverse = FALSE
)


# for the popups we need to construct the specific features as a color in the 
# data frame 
geom <- geom %>% 
  dplyr::mutate(
    popup = paste0(
      "<b>", paste0(NAME," County"),"</b>",
      "<br/><i>", "pm25","</i>",
      "<br/><b>Measured:</b> ", pm25,
      "<br/><b>Percentile:</b> ", pm25_pcntl
    )
  )

# leaftlet map 
m <- leaflet(data = geom)%>%
  addTiles()%>%
  addPolygons( 
    color = "#454547",
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1.0, fillOpacity = 0.5,
    fillColor =  ~pal(pm25_pcntl),
    highlightOptions = highlightOptions(color = "white",
                                        weight = 2,
                                        bringToFront = TRUE),
    popup = ~popup)%>%
  addLegend("bottomright",
            pal = pal,
            values = ~pm25_pcntl,
            title = "Est. Values",
            opacity = 1
            ### this flips legend values to desending which I want, but 
            ### having trouble getting the color palette to follow. 
            #labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  leaflet.extras::addSearchOSM()
m


#### in the app we need the variable and the 

mapMake <- function(inputVar, raw){
  ###
  # inputVar : a text value representing the varible to be displayed
  # raw : reactive element to change display visualization on map. Raw or pnctl
  ### 
  
  # define the indicator from user input
  # need these variables for legend creation
  indicator1 <- inputVar # basically a text value 
  indicator2 <- paste0(indicator1,"_pcntl")
  # condition for determining to visual raw data or percentile 
  if(raw == TRUE){
    indicator <- indicator1
  }else{
    indicator <- indicator2
  }
  # grab unique values
  vals <- geom %>%select(indicator)%>%sf::st_drop_geometry()
  
  pal <- colorNumeric(
    palette = colorRamp,
    domain = vals  # need something comparable to geom$varName
    #reverse = FALSE  # experiement for descending legend 
  )
  
  
  # for the popups we need to construct the specific features as a color in the 
  # data frame 
  
  
  geom <- geom %>% 
    dplyr::mutate(
      popup = paste0(
        "<b>", paste0(NAME," County"),"</b>",
        "<br/><i>", as.character(indicator),"</i>", # needs to be text
        "<br/><b>Measured:</b> ", `indicator1`, # needs to value
        "<br/><b>Percentile:</b> ", `indicator2` # needs to be value 
      )
    )
  # leaftlet map 
  m <- leaflet(data = geom)%>%
    addTiles()%>%
    addPolygons( 
      color = "#454547",
      weight = 1,
      smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = 0.5,
      fillColor =  ~pal(indicator),
      highlightOptions = highlightOptions(color = "white",
                                          weight = 2,
                                          bringToFront = TRUE),
      popup = ~popup) %>%
     addLegend("bottomright",
               pal = pal,
               values = ~indicator,
               title = "Est. Values",
               opacity = 1
               #labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
     )%>%
     leaflet.extras::addSearchOSM()
  return(m)
}



