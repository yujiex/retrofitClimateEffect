library("rnoaa")
library("dplyr")

load("../data/energy_90_to_18.rda")
load("../data/building_location.rda")
load("../data/ghcnd_data_full.rda")

## variable: cldd or htdd
## buildings: a data frame containing building id's, in a column "BLDGNUM"
## variable: cldd or htdd
get_normal <- function(buildings, variable="cldd") {
  if (variable == "cldd") {
    file.fwf = "weather_data/climate_normal/ann-cldd-normal.txt"
  } else if (variable == "htdd") {
    file.fwf = "weather_data/climate_normal/ann-htdd-normal.txt"
  }
  df.loc = buildings %>%
    dplyr::left_join(building_location, by="BLDGNUM") %>%
    dplyr::select(BLDGNUM, Latitude, Longitude) %>%
    dplyr::rename(latitude=Latitude, longitude=Longitude) %>%
  {.}
  print(df.loc)
  stations = ghcnd_data_full %>%
    dplyr::filter(element == "TMAX") %>%
    dplyr::select(id, latitude, longitude) %>%
    {.}
  matched.stations = lapply(1:nrow(df.loc), function(i) {
    results = rnoaa::meteo_distance(stations, df.loc$latitude[[i]],
                                    df.loc$longitude[[i]], radius = 32.2) %>%
      dplyr::mutate(BLDGNUM=df.loc$BLDGNUM[[i]]) %>%
      {.}
  })
  df.station = dplyr::bind_rows(matched.stations) %>%
    dplyr::select(BLDGNUM, everything()) %>%
    {.}
  print("stations")
  print(df.station)
  ## according to "readme" in ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/
  normal = readr::read_fwf(file.fwf, col_positions = readr::fwf_positions(c(1, 19, 24), c(11, 23, 24), c("id", "value", "flag"))) %>%
    {.}
  ## inverse dist weight the above
  joint = df.station %>%
    dplyr::left_join(normal, by="id") %>%
    dplyr::mutate(value=as.double(value)) %>%
    na.omit() %>%
    ## remove special values of -5555, -6666, ..., -9999
    dplyr::filter(value > -5000) %>%
    {.}
  ## print(joint)
  building_ann_normal = joint %>%
    dplyr::mutate(wt=1/distance) %>%
    dplyr::group_by(BLDGNUM) %>%
    dplyr::summarise(!!rlang::sym(variable):=weighted.mean(x=value, w=wt),
                      `dist.min`=min(distance), `dist.max`=max(distance), `num.station`=n()) %>%
    dplyr::ungroup() %>%
    {.}
  return(building_ann_normal)
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## for long term analysis
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
longterm = energy_90_to_18 %>%
  distinct(BLDGNUM) %>%
  {.}

building_ann_cldd_normal = get_normal(longterm, "cldd")

cuts = quantile(building_ann_cldd_normal$cldd, probs=seq(0, 1, 1/3))

building_ann_cldd_normal <- building_ann_cldd_normal %>%
  dplyr::mutate(climate=ifelse(cldd < cuts[[2]], "cold",
                        ifelse(cldd < cuts[[3]], "mild", "hot"))) %>%
  dplyr::mutate(climate.verbose=ifelse(cldd < cuts[[2]],
                                       sprintf("cold:[%.1f,%.1f)", cuts[[1]],
                                               cuts[[2]]),
                                ifelse(cldd < cuts[[3]],
                                       sprintf("mild:[%.1f,%.1f)", cuts[[2]],
                                               cuts[[3]]),
                                       sprintf("hot:[%.1f,%.1f)", cuts[[3]],
                                               cuts[[4]])))) %>%
  {.}

usethis::use_data(building_ann_cldd_normal, overwrite = T)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## for retrofit analysis
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

load("../data/retrofit.avg.energy.enough.data.rda")

retrofit.building = retrofit.avg.energy.enough.data %>%
  distinct(BLDGNUM) %>%
  {.}

retrofit.ann.cldd.normal = get_normal(retrofit.building, "cldd")

retrofit.ann.htdd.normal = get_normal(retrofit.building, "htdd")

## retrofit.ann.cldd.normal = get_normal(tibble::tibble(BLDGNUM="HI0011ZZ"), "cldd")

usethis::use_data(retrofit.ann.cldd.normal, overwrite = T)
usethis::use_data(retrofit.ann.htdd.normal, overwrite = T)
