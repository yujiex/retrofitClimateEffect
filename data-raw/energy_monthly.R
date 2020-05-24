## code to prepare `energy_monthly` dataset goes here

library("readxl")
library("dplyr")
library("readr")

setwd("EUASweb")

dfallweb <- readxl::read_excel("mar.energyutilization.xlsx")

## by comparing the EUAS we received and the web downloaded, we can confirm what units are used
## KWHRAMT: electricity consumption in KWH
## KWDMDAMT: electricity consumption in KW
## STEAMAMT: steam consumption in Thou. lbs
## GASAMT: natural gas consumption in Cubic Ft
## OILAMT: oil consumption in Gallon
## COALAMT: coal consumption in ??
## CHILLWTRAMT: chilled water consumption in Ton Hour
## WTRAMT: water consumption in gallon

## KWHRCOST: electricity cost
## KWDMDCOST: electricity demand cost
## STEAMCOST: steam cost
## GASCOST: natural gas cost
## OILCOST: oil cost
## COALCOST: coal cost
## CHILLWTRCOST: chilled water cost
## WTRCOST: water cost

## following are unknown fields
## *ASRC
## *CSRC

## not sure whether the renewables are consumption

clean.result.check <- function (df) {
  nbuilding = df %>%
    distinct(BLDGNUM) %>%
    nrow() %>%
    {.}
  nrec = df %>%
    nrow() %>%
    {.}
  sprintf("number of building: %d, number of record: %d", nbuilding, nrec)
}

df.amt = dfallweb %>%
  dplyr::select(BLDGNUM, FYR, FMONTH, ends_with("AMT")) %>%
  dplyr::select(-starts_with("RE")) %>%
  tidyr::gather(variable, amt, KWHRAMT:WTRAMT) %>%
  dplyr::mutate(variable = gsub("AMT", "", variable)) %>%
  na.omit() %>%
  {.}

df.cost = dfallweb %>%
  dplyr::select(BLDGNUM, FYR, FMONTH, ends_with("COST")) %>%
  dplyr::select(-starts_with("RE")) %>%
  tidyr::gather(variable, cost, KWHRCOST:WTRCOST) %>%
  dplyr::mutate(variable = gsub("COST", "", variable)) %>%
  na.omit() %>%
  {.}

df = df.amt %>%
  ## checked and saw the non-available cost corresponds to 0 usage amount
  dplyr::left_join(df.cost, by=c("BLDGNUM", "FYR", "FMONTH", "variable")) %>%
  {.}

clean.result.check(df)
## [1] "number of building: 3494, number of record: 6166413"

energy.raw = df

usethis::use_data(energy.raw, overwrite = T)

load("../data/energy.raw.rda")

energy.raw %>%
  dplyr::group_by(FYR, BLDGNUM, variable) %>%
  dplyr::summarise(amt = sum(amt)) %>%
  dplyr::filter(amt > 0) %>%
  dplyr::filter(FYR == 2018) %>%
  dplyr::distinct(BLDGNUM) %>%
  {.}

## get the building size for all buildings in the data set
area.cat <- readxl::read_excel("mar.fyrbuilding.xlsx") %>%
  {.}

building_sqft_category_region_raw <- df %>%
  dplyr::distinct(BLDGNUM, FYR) %>%
  dplyr::arrange(BLDGNUM, FYR) %>%
  dplyr::left_join(area.cat, by=c("BLDGNUM", "FYR")) %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::arrange(FYR) %>%
  tidyr::fill(GROSSSQFT, BLDGCAT, REGNNUM) %>%
  tidyr::fill(GROSSSQFT, BLDGCAT, REGNNUM, .direction = "up") %>%
  dplyr::ungroup() %>%
  dplyr::select(BLDGNUM, FYR, GROSSSQFT, BLDGCAT, REGNNUM) %>%
  na.omit() %>%
  {.}

usethis::use_data(building_sqft_category_region_raw, overwrite = T)

## remove negative consumption
df <- df %>%
  dplyr::filter(amt >= 0) %>%
  {.}

clean.result.check(df)
## [1] "number of building: 3494, number of record: 6166411"

## For each fuel type of a building in each fiscal year, if the total consumption of
## the fiscal year is 0, remove the whole year of data
df <- df %>%
  dplyr::group_by(FYR, BLDGNUM, variable) %>%
  dplyr::filter(sum(amt) > 0) %>%
  dplyr::ungroup() %>%
  {.}

clean.result.check(df)
## [1] "number of building: 3274, number of record: 1181741"

## remove water data
df <- df %>%
  ## not analyzing water data
  dplyr::filter(variable != "WTR") %>%
  {.}

clean.result.check(df)
## [1] "number of building: 3271, number of record: 1062104"

## Has non-zero energy consumption of in a fiscal year (this is an older version, less strict)
## df <- df %>%
##   dplyr::group_by(FYR, BLDGNUM) %>%
##   dplyr::filter(sum(amt[!(variable %in% c("WTR", "KWDMD"))]) > 0) %>%
##   dplyr::ungroup() %>%
##   {.}

## clean.result.check(df)
## [1] "number of building: 3271, number of record: 3790044"

## according to mar.unitconversions.csv
multiplier.to.kbtu =
  tibble::tibble(variable=c("KWHR", "KWDMD", "STEAM", "GAS", "OIL", "COAL", "CHILLWTR"),
                 mult = c(3412.142, 1, 1000000, 1031, 24580000, 138700, 12000) / 1000)

df <- df %>%
  dplyr::left_join(multiplier.to.kbtu, by="variable") %>%
  dplyr::mutate(amt.kbtu = amt * mult) %>%
  dplyr::select(-mult) %>%
  {.}

df <- df %>%
  dplyr::mutate(`Year`=ifelse(`FMONTH` < 4, `FYR` - 1, `FYR`)) %>%
  dplyr::mutate(`Month`=(`FMONTH` - 3) %% 12) %>%
  dplyr::mutate(`Month`=ifelse(`Month` == 0, 12, `Month`)) %>%
  {.}

building.state = readxl::read_excel("mar.facilitybuildings.xlsx") %>%
  dplyr::select(BLDGNUM, STATE) %>%
  {.}

## there are 23 buildings whose first two letters in the id are different from their state abbreviation
## building.state %>%
##   dplyr::mutate(firsttwo = substr(BLDGNUM, 1, 2)) %>%
##   dplyr::filter(firsttwo != STATE) %>%
##   nrow()

df <- df %>%
  dplyr::inner_join(building.state, by=c("BLDGNUM")) %>%
  {.}

clean.result.check(df)
## [1] "number of building: 3253, number of record: 1060939"

area.cat <- readxl::read_excel("mar.fyrbuilding.xlsx") %>%
  {.}

building_sqft_category_region <- df %>%
  dplyr::distinct(BLDGNUM, FYR) %>%
  dplyr::arrange(BLDGNUM, FYR) %>%
  dplyr::left_join(area.cat, by=c("BLDGNUM", "FYR")) %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::arrange(FYR) %>%
  tidyr::fill(GROSSSQFT, BLDGCAT, REGNNUM) %>%
  tidyr::fill(GROSSSQFT, BLDGCAT, REGNNUM, .direction = "up") %>%
  dplyr::ungroup() %>%
  dplyr::select(BLDGNUM, FYR, GROSSSQFT, BLDGCAT, REGNNUM) %>%
  {.}

usethis::use_data(building_sqft_category_region, overwrite = T)

df <- df %>%
  dplyr::inner_join(building_sqft_category_region, by=c("BLDGNUM", "FYR")) %>%
  {.}

clean.result.check(df)
## [1] "number of building: 3253, number of record: 1060939"

df <- df %>%
  dplyr::filter(GROSSSQFT > 1) %>%
  {.}

clean.result.check(df)
## [1] "number of building: 3182, number of record: 1171637"

energy_monthly_web <- df

usethis::use_data(energy_monthly_web, overwrite = T)

load("../../data/building_location.rda")

energy_monthly_web_withloc <- energy_monthly_web %>%
  dplyr::filter(BLDGNUM %in% unique(building_location$BLDGNUM)) %>%
  {.}

clean.result.check(energy_monthly_web_withloc)
## [1] "number of building: 3167, number of record: 1050388"

usethis::use_data(energy_monthly_web_withloc, overwrite = T)

energy_monthly_web_continental <- energy_monthly_web_withloc %>%
  dplyr::filter(!STATE %in% c("AK", "HI", "VI", "GU", "PR", "VI", "MP")) %>%
  {.}

clean.result.check(energy_monthly_web_continental)
## [1] "number of building: 3058, number of record: 1027178"

usethis::use_data(energy_monthly_web_continental, overwrite = T)

energy_90_to_18 =
  energy_monthly_web_continental %>%
  dplyr::distinct(BLDGNUM, FYR, FMONTH) %>%
  dplyr::arrange(BLDGNUM, FYR, FMONTH) %>%
  dplyr::filter(FYR >= 1990, FYR <=2018) %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::filter(n() == 348) %>%
  dplyr::ungroup() %>%
  {.}

clean.result.check(energy_90_to_18)
## [1] "number of building: 499, number of record: 173652"

usethis::use_data(energy_90_to_18, overwrite = T)
