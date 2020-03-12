library("dplyr")
library("readr")

pathname = "weather_data/ghcnd_/"

files = list.files(pathname, "good_*")

i = 1
filename = files[[i]]

lapply(files, function(filename) {
  readr::read_csv(paste0(pathname, filename)) %>%
    distinct(good) %>%
    readr::write_csv(paste0(pathname, filename))
})
