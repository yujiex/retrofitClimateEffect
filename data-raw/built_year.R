library("dplyr")
library("readxl")

built.year = readxl::read_excel("Entire GSA Building Portfolio.xls") %>%
  dplyr::select(`Building ID`, `Year Built`) %>%
  dplyr::mutate(decade = substr(`Year Built`, nchar(`Year Built`) - 1,
                                nchar(`Year Built`))) %>%
  ## dplyr::filter(decade < 17, nchar(`Year Built`) != 4) %>%
  dplyr::mutate(know.century = ((decade < 17) & (nchar(`Year Built`) != 4))) %>%
  {.}
