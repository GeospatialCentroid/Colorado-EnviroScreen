# process EJScreen ACS census variables

library(tidycensus)
library(tidyverse)

census_api_key("0ab15d4d7d8a87694979e5d5667502b365ae96f9")

var <- load_variables(2019, "acs5", cache = T)


bg_co <- get_acs(geography = "block group",
                 variables = c("C17002_001",
                               "C17002_002",
                               "C17002_003",
                               "C17002_004",
                               "C17002_005",
                               "C17002_006",
                               "C17002_007",
                               "B25010_001",
                               "B02001_001",
                               "B02001_002"),
                 state = "08",
                 year = 2019,
                 geometry = T)


ct_co <- get_acs(geography = "tract",
                 variables = c("C17002_001",
                               "B02001_001",
                               "B25106_001",
                               "B25106_006",
                               "B25106_010",
                               "B25106_014",
                               "B25106_018",
                               "B25106_022",
                               "B25106_028",
                               "B25106_032",
                               "B25106_036",
                               "B25106_040",
                               "B25106_044"),
                 state = "08",
                 year = 2019)

#create a function where you input census variables, geometry, 
# and type of variable (this will change the equation)


getACS <- function(geometry){
  
  if (geometry == 'block group')
    
    bg <- get_acs(
      geography = geometry,
      variables =
        
        c(
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
      year = 2019,
      geometry = TRUE
    )
  
        
  # note, for those where total pop/known pop is '0' I change to NA. EJ Screen
  # would set these to '0' but in some cases '0' was actually a meaningful number (i.e., 0 people of color)
    bg %>% spread(key = variable, value = estimate) %>% 
      group_by(GEOID) %>% 
      summarize(across(contains("B0"), ~ sum(.x, na.rm = TRUE))) %>% 
      group_by(GEOID) %>% 
      mutate(age_under5 = sum(B01001_003,B01001_027),
             age_over64 = sum(B01001_020, B01001_021),
             percent_minority = ifelse(B03002_001 == 0, NA, (B03002_001 - B03002_003)/B03002_001),
             percent_lowincome = ifelse(C17002_001 == 0, NA, (C17002_001 - C17002_008)/ C17002_001),
             percent_lingiso = ifelse(C16002_001 == 0, NA, (C16002_004 + C16002_007 + C16002_010 + C16002_013)/ C16002_001),
             percent_lths = )
    
}




#over64 = age65to66m + age6769m + age7074m + age7579m + age8084m + age85upm +
#age65to66f + age6769f + age7074f + age7579f + age8084f + age85upf

#pop3002 = B03002.001
#nhwa = B03002.003 
#mins (minorities) = pop - nhwa (total pop - white only)
#pctmin (% minorities) = ifelse (pop==0,0, as.numeric(mins) / pop) 



# povknownratio = C17002.001
# pov50 = C17002.002 (below 0.50 times poverty threshold)
# pov99 = C17002.003 (0.5 to 0.99 times poverty threshold)
# pov124 = C17002.004
# pov149 = C17002.005
# pov184 = C17002.006
# pov199 = C17002.007
# pov2plus = C17002.008

#QUESTION: FOR % LOW INCOME, USE 2X POVERTY THRESHOLD (EJ SCREEN AND cedv) OR 1X??


# lths (less than high school) = m0 + m4 + m6 + m8 + m9 + m10 + m11 + m12 + f0 + f4 + f6 + f8 + f9 + f10 + f11 + f12 
# m12 and f12 are those that made it to 12th grade, but no diploma 

# pctlths (% less than high school) = ifelse (age25up==0,0, as.numeric(lths) / age25up) 
# 
