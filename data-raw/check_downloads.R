library("readr")
library("dplyr")

pathname = "weather_data/ghcnd_/"

files = list.files(pathname, "building_station_distance_TMIN*")

for(f in files) {
  df = readr::read_csv(paste0(pathname, f), col_types = cols()) %>%
    dplyr::distinct(BLDGNUM) %>%
    {.}
  if (nrow(df) < 503) {
    print(sprintf("%s: %s buildings", f, nrow(df)))
  }
}
