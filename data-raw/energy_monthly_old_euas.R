## code to prepare `energy_monthly` dataset goes here

library("readxl")
library("dplyr")
library("readr")

setwd("EUAS")

files = list.files(pattern = "^Region")

out <- lapply(files, function(f) {
  df <- readxl::read_excel(f) %>%
    dplyr::mutate_at(vars(`Region No.`, `Fiscal Month`, `Fiscal Year`, `Area Field Office`), as.numeric) %>%
    dplyr::mutate_at(vars(`Service Center`), as.character) %>%
    {.}
})

recode.state.lookup <- readr::read_csv("../state_abbr.csv")

out1617 <- lapply(c("2016", "2017"), function(s) {
  df <- readxl::read_excel("EUAS_AllRegions_2016-2017.xlsx", s) %>%
    dplyr::mutate_at(vars(`Region No.`, `Fiscal Month`, `Fiscal Year`, `Area Field Office`), as.numeric) %>%
    dplyr::mutate_at(vars(`Service Center`), as.character) %>%
    dplyr::rename(`Abbr`=`State`) %>%
    dplyr::left_join(recode.state.lookup, by="Abbr") %>%
    dplyr::select(-`Abbr`) %>%
    {.}
})

dfall.before15 <- dplyr::bind_rows(out) %>%
  dplyr::filter(!is.na(State)) %>%
  dplyr::filter(`Fiscal Year` != 2016) %>%
  {.}

dfall.1617 <- dplyr::bind_rows(out1617) %>%
  dplyr::filter(!is.na(State)) %>%
  {.}

dfall <- dplyr::bind_rows(dfall.before15, dfall.1617)

## 3 buildings with ****** as their state
## `Building Number`
## <chr>
## 1 DC0017ZZ, the white house
## 2 DC1105ZZ, the white house garage (DC1105ZZ-01)
## 3 TX0000HF, 1 JUSTICE PARK DRIVE FB
dfall <- dfall %>%
  dplyr::mutate(State=ifelse(`Building Number` %in% c("DC0017ZZ", "DC1105ZZ"), "Washington DC", State)) %>%
  dplyr::mutate(State=ifelse(`Building Number` == "TX0000HF", "Texas", State)) %>%
  {.}

## add calendar year and month
dfall <- dfall %>%
  dplyr::mutate(`Year`=ifelse(`Fiscal Month` < 4, `Fiscal Year` - 1, `Fiscal Year`)) %>%
  dplyr::mutate(`Month`=(`Fiscal Month` - 3) %% 12) %>%
  dplyr::mutate(`Month`=ifelse(`Month` == 0, 12, `Month`)) %>%
  {.}

names(dfall)

## fixme: summary data
dfall %>%
  dplyr::group_by(`Building Number`, `Fiscal Year`, `Gross Sq.Ft`) %>%
  dplyr::summarise(first) %>%
  dplyr::ungroup() %>%
  ## dplyr::group_by(`Building Number`, `Fiscal Year`) %>%
  ## dplyr::summarise(`GSF`=n(`Gross Sq.Ft`)) %>%
  ## dplyr::ungroup() %>%
  {.}

energy_monthly_euas <- dfall

usethis::use_data(energy_monthly_euas)

