## only very few building has this info
library("readxl")
library("dplyr")

building_built_year = readxl::read_excel("List-of-Battle-of-the-Buildings-Competitors-Energy.xlsx", skip = 3) %>%
  dplyr::filter(`Submitting Organization`=="The U.S. General Services Administration") %>%
  dplyr::mutate(`Building_Number`=substr(`Property Name`, 1, 8)) %>%
  dplyr::select(`Building_Number`, `Year Built`) %>%
  {.}

usethis::use_data(building_built_year)
