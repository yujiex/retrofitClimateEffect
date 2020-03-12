library("dplyr")
library("readr")
library("usethis")

setwd("ion download")

read.interval <- function (varname) {
  files = list.files(pattern=sprintf("^%s*", varname))
  dflist <- lapply(files, function(f) {
    readr::read_csv(f, col_types = cols()) %>%
    dplyr::select(-starts_with("X")) %>%
    tidyr::gather(`building`, !!rlang::sym(varname), -`Timestamp`) %>%
    na.omit() %>%
    dplyr::mutate(!!rlang::sym(varname) := as.numeric(!!rlang::sym(varname))) %>%
    {.}
  })
  dfuse <- dplyr::bind_rows(dflist)
  return(dfuse)
}

## electricity received from the grid
kwh.del <- read.interval(varname="kWh Del Int")

length(unique(kwh.rec$building))

## electricity send to the grid
kwh.rec <- read.interval(varname="kWh Rec Int")

kwh <- kwh.del %>%
  dplyr::left_join(kwh.rec, by=c("Timestamp", "building")) %>%
  {.}

buildings = unique(kwh$building)

lapply(kwh %>%
  dplyr::slice(1:200000) %>%
  dplyr::group_by(building) %>%
  dplyr::group_split() %>%
  {.}, function(x) {
})


usethis::use_data(energy_15_min_as_is)

library("readxl")

location = readxl::read_excel("../EUASweb/mar.facilitybuildings.xlsx") %>%
  dplyr::select(`BLDGNUM`, `ZIP`, `CITY`, `STATE`) %>%
  {.}

## fixme: need time zone of each building
kwh <- kwh %>%
  dplyr::mutate(Timestamp=as.POSIXct(Timestamp, format="%m/%d/%Y %I:%M:%S %p", tz=tz)) %>>%

for (f in files) {
  print(sprintf("--------------%s---------------", f))
  if (is.na(varname)) {
    varname = substr(f, 1, regexpr("_", f)[[1]] - 1)
  }
  df =
    readr::read_csv(f) %>%
    dplyr::select(-starts_with("X")) %>%
    ## readr::problems() %>%
    ## print()
    tidyr::gather(`building`, !!rlang::sym(varname), -`Timestamp`) %>%
    na.omit() %>%
    {.}
  buildings = unique(df$building)
  for (b in buildings) {
    print(b)
    df %>%
      dplyr::filter(building==b) %>%
      dplyr::select(-`building`) %>%
      readr::write_csv(sprintf("../building_energy/%s_%s.csv", b, varname))
  }
}

use_data(energy_15_min)
