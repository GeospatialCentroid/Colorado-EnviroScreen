###
# generate map element the contains all the features of the shiny app
# 20211103
# carverd@colorstate.edu
###


# read in datasets
d1 <- sfarrow::st_read_feather("data/envScreenScores/allScores.feather")
d2 <- sf::read_sf("F:/geoSpatialCentroid/coEnvrioScreen/src/Colorado-EnviroScreen/data/county/coloradoCounties.geojson")%>%
  select(
    countyName = NAME,
    geoid2 = GEOID
  )%>%
  st_drop_geometry()
# grab county name
d1 <- d1 %>%
  dplyr::mutate(geoid2 = str_sub(GEOID,start = 1, end = 5))%>%
  dplyr::left_join(d2, by="geoid2")%>%
  dplyr::select(-geoid2)


# export features as a csv
# d2 <- d1 %>% sf::st_drop_geometry()
# readr::write_csv(d2, file = "data/envScreenScores/allScores.csv")

### temp features for testing
d1$coalCommunity <- rbinom(nrow(d1), 1, 0.5)
d1$oilCommunity <- rbinom(nrow(d1), 1, 0.5)
d1$rural <- rbinom(nrow(d1), 1, 0.5)
d1$DI <- rbinom(nrow(d1), 1, 0.5)

# transform to TRUE False
d1 <- d1 %>%
  dplyr::mutate(
    coalCommunity = case_when(
      coalCommunity == 0 ~ "No",
      TRUE ~ "Yes"
    ),
    oilCommunity = case_when(
      oilCommunity == 0 ~ "No",
      TRUE ~ "Yes"
    ),
    rural = case_when(
      rural == 0 ~ "No",
      TRUE ~ "Yes"
    ),
    DI = case_when(
      DI == 0 ~ "No",
      TRUE ~ "Yes"
    )
  )

# create line features


# create line features ----------------------------------------------------
##county level
oil <- d1 %>%
  dplyr::filter(oilCommunity == "Yes", area == "County")%>%
  patternLayer(pattern = "horizontal",
               mode = "sfc",
               density = 4,
  )
coal <-d1 %>%
  dplyr::filter(coalCommunity == "Yes", area == "County")%>%
  patternLayer(pattern = "vertical",mode = "sfc", density = 4)
rural <- d1 %>%
  dplyr::filter(rural == "Yes", area == "County")%>%
  patternLayer(pattern = "left2right",mode = "sfc", density = 4)


# purple high 9 colors
colorRamp <- c(
  "#fcfbfd",
  "#efedf5",
  "#dadaeb",
  "#bcbddc",
  "#9e9ac8",
  "#807dba",
  "#6a51a3",
  "#54278f",
  "#3f007d")


mapMake <- function(geom,oil,coal,rural, inputVar, colorRamp, raw){
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
  vals <- geom%>%
    select(indicator)%>%
    sf::st_drop_geometry()%>%
    pull()

  # palette for the map
  palMap <- colorNumeric(
    palette = colorRamp,
    domain = vals,
    reverse = TRUE
  )

  # #palette for the legend
  # palLen <- colorNumeric(
  #   palette = colorRamp,
  #   domain = vals,
  #   reverse = FALSE
  #   )

# Detemine geom label -----------------------------------------------------
  if(geom$area[1] == "County"){
    featName <- paste0(geom$countyName," County")
  }else{
    if(geom$area[1] == "Census Tract"){
      featName <- paste0("Census Tract :",geom$GEOID, " in ", geom$countyName," County")
    }else{
      featName <- paste0("Census Block Group :",geom$GEOID, " in ", geom$countyName," County")
    }
  }

  # construct legend
  geom <- geom %>%
    dplyr::mutate(
      popup = paste0(
        "<br/><h4>", as.character(indicator1),"</h4>", # needs to be text
        "<br/><b>Disproportionally Impacted Community: </b>", DI,
        "<br/>", featName,
        paste0(if(inputVar %in% c("Colorado.Enviroscreen.Score",
                           "Pollution.Burden",
                           "Climate.Burden",
                           "Environmental.Exposures",
                           "Environmental.Effects",
                           "Climate",
                           "Sensitive.Populations",
                           "Socioeconomic"
                           )){
          paste0("<br/><b>Percentile:</b> ", round(!!as.symbol(indicator2), digits =  0))
        }else{
          paste0("<br/><b>Measured:</b> ", round(!!as.symbol(indicator1), digits = 2),
          "<br/><b>Percentile:</b> ", round(!!as.symbol(indicator2), digits =  0))
        }),
        "<br/><b>Coal Community:</b> ", coalCommunity,
        "<br/><b>Rural:</b> ", rural,
        "<br/><b>Oil Community:</b> ", oilCommunity# needs to be value
      )
    )

# construct Legend --------------------------------------------------------


  # leaftlet map
  m <- leaflet(data = geom)%>%
    addProviderTiles(
      "Stamen.Toner",
      group = "Stamen.Toner"
    ) %>%
    addProviderTiles(
      "OpenStreetMap",
      # give the layer a name
      group = "OpenStreetMap"
    ) %>%
    addMapPane("index", zIndex = 410)%>%
    addMapPane("binary", zIndex = 420)%>%
    addPolylines(data = oil,
                 stroke = TRUE,
                 color = "#54A800",
                 weight = 1,
                 layerId = "Oil and Gas",
                options = pathOptions(pane = "binary"),
                group = "Oil Community"
                 )%>%
    addPolylines(data = coal,
                 stroke = TRUE,
                 color = "#54A800",
                 weight = 1,
                 layerId = "Coal",
                 options = pathOptions(pane = "binary"),
                 group = "Coal Community"
    )%>%
    addPolylines(data = rural,
                 stroke = TRUE,
                 color = "#54A800",
                 weight = 1,
                 layerId = "Rural",
                 options = pathOptions(pane = "binary"),
                 group = "Rural"
    )%>%
    addPolygons(
      color = "#888A8F",  # light :#DDE1E8  Dark : #888A8F
      weight = 0.4,
      smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = 0.8,
      fillColor =  ~palMap(vals),
      highlightOptions = highlightOptions(color = "white",
                                          weight = 2,
                                          bringToFront = TRUE),
      popup = ~popup,
      group = "Indicator Score",
      options = pathOptions(pane = "index"))%>%
    addLayersControl(
      baseGroups = c("Stamen.Toner","OpenStreetMap"),
      overlayGroups = c("Indicator Score",
                                       "Coal Community",
                                       "Rural",
                                       "Oil Community"
                                       ),
                     options = layersControlOptions(collapsed = FALSE),
      position = "topleft")%>%
    leaflet.extras::addSearchOSM(
      options = leaflet.extras::searchOptions(
        autoCollapse = TRUE,
        hideMarkerOnCollapse = TRUE))%>%
        hideGroup(group = c("Coal Community","Rural", "Oil Community"))%>%
  addLegend("topright",
               colors = colorRamp,
               title = "Est. Values",
               labels = c("Most Burdened","","","","","","","","Least Burdened"),
               opacity = 1
               # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
     )%>%
    addResetMapButton()
  return(m)
}

  # Need '+proj=longlat +datum=WGS84'


# select area of interest
c1 <- d1[d1$area == "Census Block Group", ]

mapMake(geom =c1,
        inputVar = "Colorado.Enviroscreen.Score",
        colorRamp = colorRamp,
        oil = oil,
        coal = coal,
        rural = rural,
        raw = FALSE)

View(c1)
