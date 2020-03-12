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

clean.result.check(dfallweb)
## [1] "number of building: 3494, 

## remove records with negative consumption (only electricity consumption has negatives)
dfallweb <- dfallweb %>%
  dplyr::filter(is.na(KWHRAMT) | KWHRAMT >=0) %>%
  {.}

clean.result.check(dfallweb)
## [1] "number of building: 3494, number of record: 856841"

## remove buildings with neither electricity nor gas consumption
dfallweb.has.data <- dfallweb %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total = sum(KWHRAMT, STEAMAMT, GASAMT, OILAMT, COALAMT, CHILLWTRAMT, na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(FYR, BLDGNUM) %>%
  dplyr::filter(sum(total) > 0) %>%
  dplyr::ungroup() %>%
  {.}

clean.result.check(dfallweb.has.data)
## [1] "number of building: 3271, number of record: 556110"

dfallweb.has.elec.gas <- dfallweb %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total = sum(KWHRAMT, GASAMT, na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(FYR, BLDGNUM) %>%
  dplyr::filter(sum(total) > 0) %>%
  dplyr::ungroup() %>%
  {.}

clean.result.check(dfallweb.has.elec.gas)
## [1] "number of building: 3262, number of record: 554710"

building.state = readxl::read_excel("mar.facilitybuildings.xlsx") %>%
  dplyr::select(BLDGNUM, STATE) %>%
  {.}

## unit conversion is based on "mar.unitconversions.xlsx"
energy_monthly_web <- dfallweb.has.data %>%
  dplyr::select(BLDGNUM, FYR, FMONTH, ends_with("AMT"), ends_with("COST")) %>%
  dplyr::select(-starts_with("RE")) %>%
  dplyr::mutate(`Year`=ifelse(`FMONTH` < 4, `FYR` - 1, `FYR`)) %>%
  dplyr::mutate(`Month`=(`FMONTH` - 3) %% 12) %>%
  dplyr::mutate(`Month`=ifelse(`Month` == 0, 12, `Month`)) %>%
  dplyr::mutate(Electricity.kbtu=KWHRAMT * 3412.142 / 1000,
                Demand.kw=KWDMDAMT,
                Steam.kbtu=STEAMAMT * 1000000 / 1000,
                Gas.kbtu=GASAMT * 1031 / 1000,
                Coal.kbtu=COALAMT * 24580000 / 1000,
                Oil.kbtu=OILAMT * 138700 / 1000,
                Chilledwater.kbtu=CHILLWTRAMT * 12000 / 1000,
                Water.gallon=WTRAMT) %>%
  dplyr::select(-ends_with("AMT")) %>%
  {.}

clean.result.check(energy_monthly_web)

energy_monthly_web <- energy_monthly_web %>%
  dplyr::left_join(building.state, by=c("BLDGNUM")) %>%
  dplyr::filter(!is.na(STATE)) %>%
  {.}

clean.result.check(energy_monthly_web)

load("../data/energy_monthly_web.rda")

usethis::use_data(energy_monthly_web, overwrite = T)

area.cat <- readxl::read_excel("mar.fyrbuilding.xlsx") %>%
  {.}

building_sqft_category_region <- energy_monthly_web %>%
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

energy_monthly_web <- energy_monthly_web %>%
  dplyr::left_join(building_sqft_category_region, by=c("BLDGNUM", "FYR")) %>%
  dplyr::filter(!is.na(GROSSSQFT)) %>%
  {.}

clean.result.check(energy_monthly_web)
## [1] "number of building: 3248, number of record: 555437"

energy_monthly_web <- energy_monthly_web %>%
  dplyr::filter(GROSSSQFT > 1) %>%
  {.}

clean.result.check(energy_monthly_web)
## [1] "number of building: 3181, number of record: 549023"

usethis::use_data(energy_monthly_web, overwrite = T)

load("../../data/building_location.rda")

energy_monthly_web_withloc <- energy_monthly_web %>%
  dplyr::filter(BLDGNUM %in% unique(building_location$BLDGNUM)) %>%
  {.}

clean.result.check(energy_monthly_web_withloc)

usethis::use_data(energy_monthly_web_withloc, overwrite = T)

energy_monthly_web_continental <- energy_monthly_web_withloc %>%
  dplyr::filter(!STATE %in% c("AK", "HI", "VI", "GU", "PR", "VI", "MP")) %>%
  {.}

clean.result.check(energy_monthly_web_continental)

load("../data/energy_monthly_web_withloc.rda")

energy_90_to_18 = energy_monthly_web_withloc %>%
  dplyr::arrange(BLDGNUM, FYR, FMONTH) %>%
  dplyr::filter(FYR >= 1990, FYR <=2018) %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::filter(n() == 348) %>%
  dplyr::ungroup() %>%
  {.}

usethis::use_data(energy_90_to_18)

load("../data/ghcnd_data_full.rda")
