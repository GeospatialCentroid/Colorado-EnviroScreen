# get EJScreen ACS census variables

# testing
# library(tidycensus)
# library(dplyr)
# library(tidyr)
# processingLevel <- "tract"
# year <-
# tic()
# d2 <- getACS(filePath = filePath, geometry = geometry)
# toc()


#'geometry' is one of "block group", "tract", or "county"

getACS <- function(processingLevel, year ){
  ###
  # Workflow pulled from the EPA ejscreen methodology
  # geometry = character descirbing the spatial extent
  # year = numeric value define the year to pull data from
  ###
  x <- c("tidycensus","dplyr","tidyr")
  lapply(x, require, character.only = TRUE)
  # call census api key
  getCensusAPIKey()
  #### potential for some conditional testing here, but this is getting
  #### more complicated then it really needs to be... so something to
  #### come back too.

  # change the geometry character to match requirements in tidy census
  if(processingLevel == "censusBlockGroup"){
    processingLevel <- "block group"
  }
  if(processingLevel == "county"){
    processingLevel <- "county"
  }
  if(processingLevel == "censusTract"){
    processingLevel <- "tract"
  }

  # pull ACS data
  acs <- tidycensus::get_acs(
    geography = processingLevel,
    variables = c(
        # under 5
        "B01001_003",
        "B01001_027",
        # over 64
        paste0("B01001_0", 20:25),
        paste0("B01001_0", 44:49),
        #percent people of color
        "B03002_001",
        "B03002_003",
        #Percent low income
        "C17002_001",
        "C17002_008",
        #Percent linguistic isolation
        "C16002_001",
        "C16002_004",
        "C16002_007",
        "C16002_010",
        "C16002_013",
        #Percent less than high school education
        "B15002_001",
        paste0("B15002_00", 3:9),
        "B15002_010",
        paste0("B15002_0", 20:27)
      ),
    state = "08",
    year = year
  )



  # NOTE, for those where total pop/known pop is '0' I change to NA. EJ Screen
  # would set these to '0' but in some cases '0' was actually a meaningful
  # number (i.e., 0 people of color)


    acs %>% tidyr::spread(key = variable, value = estimate) %>%

      dplyr::group_by(GEOID) %>%
      dplyr::summarize(across(contains("_"), ~ sum(.x, na.rm = TRUE))) %>%
      dplyr::group_by(GEOID) %>% # not sure why.. but had to group_by a second time to get correct calculations
      dplyr::mutate(
        age_under5 = sum(B01001_003, B01001_027),
        age_over64 = sum(B01001_020, B01001_021),
        percent_minority = ifelse(B03002_001 == 0, NA, (B03002_001 - B03002_003) /
                                    B03002_001),
        percent_lowincome = ifelse(C17002_001 == 0, NA, (C17002_001 - C17002_008) / C17002_001),
        percent_lingiso = ifelse(
          C16002_001 == 0,
          NA,
          sum(C16002_004, C16002_007, C16002_010, C16002_013) / C16002_001
        ),
        percent_lths = ifelse(
          B15002_001 == 0,
          NA,
          sum(
            B15002_003,
            B15002_004,
            B15002_005,
            B15002_006,
            B15002_007,
            B15002_008,
            B15002_009,
            B15002_010,
            B15002_020,
            B15002_021,
            B15002_022,
            B15002_023,
            B15002_024,
            B15002_025,
            B15002_026,
            B15002_027
          ) / B15002_001
        )
      ) %>%
      dplyr::select(GEOID, age_under5, age_over64, percent_minority, percent_lowincome,
             percent_lingiso, percent_lths)

}
