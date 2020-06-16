library("dplyr")

load("../data/cmip5.bin.period.rda")

total.days = cmip5.bin.period %>%
  tidyr::gather(temperaturre.bin, value, `<10`:`>90`) %>%
  dplyr::group_by(BLDGNUM, Latitude, Longitude, period, scenario ,model) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  {.}

total.days %>%
  dplyr::group_by(period, scenario) %>%
  dplyr::summarise_at(vars(value), tibble::lst(min, median, mean, max)) %>%
  dplyr::ungroup() %>%
  {.}
