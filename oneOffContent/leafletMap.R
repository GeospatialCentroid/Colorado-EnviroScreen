###
# generate map element the contains all the features of the shiny app
# 20211103
# carverd@colorstate.edu
### 


# read in datasets
d1 <- sfarrow::st_read_feather("data/envScreenScores/allScores.feather")

# export features as a csv 
d2 <- d1 %>% sf::st_drop_geometry()
readr::write_csv(d2, file = "data/envScreenScores/allScores.csv")

### temp features for testing 
d1$coalCommunity <- rbinom(nrow(d1), 1, 0.5)
d1$oilCommunity <- rbinom(nrow(d1), 1, 0.5)
d1$rural <- rbinom(nrow(d1), 1, 0.5)


# geom <- d1 
# inputVar <- "Colorado.Enviroscreen.Score"
# raw <- FALSE

# purple high
colorRamp1 <- c(
  "#00441b","#1b7837","#5aae61","#a6dba0","#d9f0d3","#e7d4e8",
  "#c2a5cf","#9970ab","#762a83","#40004b"
)
# blue high
colorRamp2 <- c(
  '#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0',
  '#225ea8','#253494','#081d58'
)
# Teal high
colorRamp3 <- c(
  "#543005","#8c510a","#bf812d","#dfc27d","#f6e8c3","#c7eae5",
  "#80cdc1","#35978f","#01665e","#003c30"
)

mapMake <- function(geom, inputVar, colorRamp, raw){
  ###
  # inputVar : a text value representing the varible to be displayed
  # raw : reactive element to change display visualization on map. Raw or pnctl
  ### 
  # # purple high
  # colorRamp <- c(
  #   "#00441b","#1b7837","#5aae61","#a6dba0","#d9f0d3","#e7d4e8",
  #   "#c2a5cf","#9970ab","#762a83","#40004b"
  # )
  # 
  
  
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
  vals <- geom %>%select(indicator)%>%sf::st_drop_geometry() %>% pull()
  
    # palette for the map 
    palMap <- colorNumeric(
      palette = colorRamp,
      domain = vals,  
      reverse = FALSE 
    )
    #palette for the legend 
    palLen <- colorNumeric(
      palette = colorRamp,
      domain = vals,  
      reverse = TRUE
    )
  # binary palette
  palBin <-  colorFactor(palette = c("black", NA), 
                         levels = c("1", "0"))  
  geom <- geom %>%
    dplyr::mutate(
      popup = paste0(
        "<b>", paste0(GEOID," County"),"</b>",
        "<br/><i>", as.character(indicator),"</i>", # needs to be text
        "<br/><b>Measured:</b> ", !!as.symbol(indicator1),
        "<br/><b>Percentile:</b> ", !!as.symbol(indicator2),
        "<br/><b>Coal Community:</b> ", coalCommunity,
        "<br/><b>Rural:</b> ", oilCommunity,
        "<br/><b>Oil Community:</b> ", rural# needs to be value
      )
    )
  
  # leaftlet map 
  m <- leaflet(data = geom)%>%
    addTiles()%>%
    addMapPane("index", zIndex = 420)%>%
    addMapPane("binary", zIndex = 410)%>%
    # addPolygons(
    #   color = NA,
    #   weight = 1,
    #   smoothFactor = 0.5,
    #   opacity = 0.7, 
    #   fillOpacity = 0.2,
    #   fillColor =  palBin(geom$coalCommunity),
    #   group = "Coal Community",
    #   options = pathOptions(pane = "binary")
    # )%>%
    # addPolygons(
    #   color = NA,
    #   weight = 1,
    #   smoothFactor = 0.5,
    #   opacity = 0.7, 
    #   fillOpacity = 0.2,
    #   fillColor =  palBin(geom$oilCommunity),
    #   group = "Oil Community",
    #   options = pathOptions(pane = "binary")
    # )%>%
    # addPolygons(
    #   color = NA,
    #   weight = 1,
    #   smoothFactor = 0.5,
    #   opacity = 0.7, 
    #   fillOpacity = 0.2,
    #   fillColor =  palBin(geom$rural),
    #   group = "Rural",
    #   options = pathOptions(pane = "binary")
    # )%>%
    addPolygons( 
      color = "#454547",
      weight = 1,
      smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = 0.5,
      fillColor =  ~palMap(vals),
      highlightOptions = highlightOptions(color = "white",
                                          weight = 2,
                                          bringToFront = TRUE),
      popup = ~popup,
      group = "Indicator Score",
      options = pathOptions(pane = "index"))%>%
    addLayersControl(overlayGroups = c("Indicator Score",
                                       "Coal Community",
                                       "Rural",
                                       "Oil Community"),
                     options = layersControlOptions(collapsed = FALSE))%>%
    # have layers as togglable features
    hideGroup(group = c("Coal Community","Rural", "Oil Community"))%>%
    leaflet.extras::addSearchOSM() %>% # add search funtion
     addLegend("bottomright",
               pal = palLen,
               values = ~vals,
               title = "Est. Values",
               opacity = 1,
               labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
     )
  return(m)
}
# Need '+proj=longlat +datum=WGS84' 


# select area of interest 
c1 <- d1[d1$area == "Census Block Group", ]

mapMake(geom =c1, inputVar = "Colorado.Enviroscreen.Score",colorRamp = colorRamp3,  raw = FALSE)

View(c1)
