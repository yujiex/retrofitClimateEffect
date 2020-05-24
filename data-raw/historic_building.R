## library("readr")
library("readxl")
library("dplyr")

## historic.building = readr::read_csv("historic_building.csv") %>%
##   {.}

historic.building = readxl::read_excel("historic_building_web.xlsx") %>%
  dplyr::mutate(`Built year`=as.numeric(substr(`Construction Date`, 1, 4))) %>%
  dplyr::select(-`Architect(s)`) %>%
  dplyr::rename(BLDGNUM=Number) %>%
  {.}

usethis::use_data(historic.building, overwrite = T)
