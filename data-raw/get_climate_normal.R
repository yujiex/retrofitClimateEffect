library("rnoaa")
library("dplyr")

load("../data/energy_90_to_18.rda")
load("../data/building_location.rda")
load("../data/ghcnd_data_full.rda")

longterm = energy_90_to_18 %>%
  distinct(BLDGNUM) %>%
  dplyr::left_join(building_location, by="BLDGNUM") %>%
  dplyr::select(BLDGNUM, Latitude, Longitude) %>%
  dplyr::rename(latitude=Latitude, longitude=Longitude) %>%
  {.}

stations = ghcnd_data_full %>%
  dplyr::filter(element == "TMAX") %>%
  dplyr::select(id, latitude, longitude) %>%
  {.}

matched.stations = lapply(1:nrow(longterm), function(i) {
  results = rnoaa::meteo_distance(stations, longterm$latitude[[i]],
                                  longterm$longitude[[i]], radius = 32.2) %>%
    dplyr::mutate(BLDGNUM=longterm$BLDGNUM[[i]]) %>%
    {.}
})

df.station = dplyr::bind_rows(matched.stations) %>%
  dplyr::select(BLDGNUM, everything()) %>%
  {.}

## according to "readme" in ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/
cddnormal = readr::read_fwf("weather_data/climate_normal/ann-cldd-normal.txt", col_positions = readr::fwf_positions(c(1, 19, 24), c(11, 23, 24), c("id", "value", "flag"))) %>%
  {.}

## inverse dist weight the above
building_ann_cldd_normal = df.station %>%
  dplyr::left_join(cddnormal, by="id") %>%
  dplyr::mutate(value=as.double(value)) %>%
  na.omit() %>%
  dplyr::mutate(wt=1/distance) %>%
  dplyr::group_by(BLDGNUM, latitude, longitude) %>%
  dplyr::summarise(`cldd`=weighted.mean(x=value, w=wt),
                    `dist.min`=min(distance), `dist.max`=max(distance), `num.station`=n()) %>%
  dplyr::ungroup() %>%
  {.}

cuts = quantile(building_ann_cldd_normal$cldd, probs=seq(0, 1, 1/3))

building_ann_cldd_normal <- building_ann_cldd_normal %>%
  dplyr::mutate(climate=ifelse(cldd < cuts[[2]], "cold",
                        ifelse(cldd < cuts[[3]], "mild", "hot"))) %>%
  dplyr::mutate(climate.verbose=ifelse(cldd < cuts[[2]], "cold:[5,745)",
                        ifelse(cldd < cuts[[3]], "mild:[745,1322)", "hot:[1322,4838]"))) %>%
  {.}

usethis::use_data(building_ann_cldd_normal, overwrite = T)
