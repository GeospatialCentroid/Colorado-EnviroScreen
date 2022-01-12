###
# generate temp line features for shiny app
# carverd@colostate.edu 
# 20211214 
###

genCommunityData <- function(){
  d1 <- readRDS("data/envScreenScores/allScores.rda")
  
  
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
  
  # create line features ----------------------------------------------------
  ##county level
  oil <- d1 %>%
    dplyr::filter(oilCommunity == "Yes", area == "County")%>%
    patternLayer(pattern = "horizontal",mode = "sfc",density = 4)
  saveRDS(object = oil, file = "data/communityMetrics/oil.rda")
  coal <-d1 %>%
    dplyr::filter(coalCommunity == "Yes", area == "County")%>%
    patternLayer(pattern = "vertical",mode = "sfc", density = 4)
  saveRDS(object = coal, file = "data/communityMetrics/coal.rda")
  rural <- d1 %>%
    dplyr::filter(rural == "Yes", area == "County")%>%
    patternLayer(pattern = "left2right",mode = "sfc", density = 4)
  saveRDS(object = rural, file = "data/communityMetrics/rural.rda")
  
}


