# Lauren ACS request
# 4/7/2021

# CENSUS BLOCK GROUPS WITH 
# 40% OR MORE OF HOUSEHOLDS ARE LOW INCOME (200% OF FEDERAL POVERTY GUIDELINE OR LESS)
# 40% OR MORE OF HOUSEHOLDS ARE MINORITY (LAUREN'S NOTE - I DON'T KNOW HOW A HOUSEHOLD CAN BE CONSIDERED MINORITY, SO I ASSUME HOUSEHOLDS WITH INDIVIDUALS WHO ARE MINORITIES)
# 40% OR MORE OF HOUSEHOLDS THAT ARE "HOUSING COST-BURDENED" (30% OR MORE OF HOUSEHOLD INCOME SPENT ON HOUSING)

library(tidycensus)
library(dplyr)
library(tidyr)

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



# Things I need: 
# - total block group population -- C17002_1, B02001_001
# - average household size per block group  --  B25010
# - combine those two for total HH per block group
# - people living below 200% FPL per block group  -- sum C17002_002-C17002_007 
# - % Minority per block group 1-%white alone  -- B02001_002 (count)
# - % housing cost burdened  -- B25106

# used this resource for the low-income housing calcultion: https://www.oregon.gov/odot/RPTD/RPTD%20Committee%20Meeting%20Documents/STIF-Low-Income-Methods-Guidance.pdf


bg <- bg_co %>%
  select(-moe) %>%
  spread(key = variable, value = estimate)%>%
  mutate(TotalPop = C17002_001,
         AvgHHSize = B25010_001,
         HHperBG = TotalPop/AvgHHSize,
         WhitePop = B02001_002,
         MinPop = TotalPop-WhitePop,
         MinHH = MinPop/AvgHHSize,
         LT200FLP_Pop = C17002_002+C17002_003+C17002_004+C17002_005+C17002_006+C17002_007,
         FLPHH = LT200FLP_Pop/AvgHHSize,
         FLPHH_PCT = FLPHH/HHperBG,
         Min_PCT = MinHH/HHperBG,
         FLP_FLAG = ifelse(FLPHH_PCT >= .4, 1, 0),
         Min_FLAG = ifelse(Min_PCT >= .4, 1, 0),
         CT = substr(GEOID, 1, 11))

FLP_HH_count <- sum(bg$FLP_FLAG, na.rm = T)
Min_HH_count <- sum(bg$Min_FLAG, na.rm = T)
FLP_HH_percent <- sum(bg$FLP_FLAG, na.rm = T)/nrow(bg)
Min_HH_percent <- sum(bg$Min_FLAG, na.rm = T)/nrow(bg)
FLP_HH_pop <- sum(bg$TotalPop[bg$FLP_FLAG==1], na.rm = T)
Min_HH_pop <- sum(bg$TotalPop[bg$Min_FLAG==1], na.rm = T)

minhh <- bg %>%
  filter(Min_FLAG==1)%>%
  summarise(TotalPop = sum(TotalPop),
            AffectedBGs = sum(Min_FLAG))


flphh <- bg %>%
  filter(FLP_FLAG==1)%>%
  summarise(TotalPop = sum(TotalPop),
            AffectedBGs = sum(FLP_FLAG))

ct <- ct_co %>%
  select(-moe) %>%
  spread(key = variable, value = estimate)%>%
  mutate(TotalPop = C17002_001,
         HHUnits = B25106_001,
         HH_Burdened = B25106_006+B25106_010+B25106_014+B25106_018+B25106_022+B25106_028+B25106_032+B25106_036+B25106_040+B25106_044,
         HH_Burdened_Pct = HH_Burdened/HHUnits,
         Burdened_FLAG = ifelse(HH_Burdened_Pct >= .4, 1, 0))

Burdened_HH_CT_count <- sum(ct$Burdened_FLAG, na.rm = T)
Burdened_HH_CT_percent <- sum(ct$Burdened_FLAG, na.rm = T)/nrow(ct)
Burdened_HH_pop <- sum(ct$TotalPop[ct$Burdened_FLAG==1], na.rm = T)



# creating data layer

DI <- merge(bg, ct, by.x = "CT", by.y = "GEOID", all.x = T)

DI <- DI %>%
  filter(FLP_FLAG == 1 | Min_FLAG == 1 | Burdened_FLAG == 1) %>%
  mutate(FLPHH_PCT = FLPHH_PCT * 100,
         Min_PCT = Min_PCT * 100,
         HH_Burdened_Pct = HH_Burdened_Pct * 100) %>%
  select(CTFIPS = CT,
    BGFIPS = GEOID,
    TotalPop = TotalPop.x,
    LowIncome_PCT = FLPHH_PCT,
    PeopleofColor_PCT = Min_PCT,
    HousingBurdened_PCT = HH_Burdened_Pct)

bgsf <- 
