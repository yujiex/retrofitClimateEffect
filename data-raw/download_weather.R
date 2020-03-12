library("dplyr")

load("../data/energy_monthly_web_withloc.rda")
load("../data/building_location.rda")
load("../data/energy_90_to_18.rda")

load("../data/ghcnd_data_full.rda")

devtools::load_all("~/Dropbox/gsa_2017/get.noaa.weather")

longterm = energy_90_to_18 %>%
  distinct(BLDGNUM) %>%
  dplyr::left_join(building_location, by="BLDGNUM") %>%
  dplyr::select(BLDGNUM, Latitude, Longitude) %>%
  dplyr::rename(latitude=Latitude, longitude=Longitude) %>%
  {.}

## variable to download
v = "TMAX"
## v = "TMIN"

buildings = longterm$BLDGNUM

## startdates = seq(as.Date("1990-01-01"), as.Date("1999-01-01"), "year")
## enddates = seq(as.Date("1991-01-01"), as.Date("2000-01-01"), "year") - 1
startdates = seq(as.Date("2000-01-01"), as.Date("2008-01-01"), "year")
enddates = seq(as.Date("2001-01-01"), as.Date("2009-01-01"), "year") - 1
## startdates = seq(as.Date("2009-01-01"), as.Date("2018-01-01"), "year")
## enddates = seq(as.Date("2010-01-01"), as.Date("2019-01-01"), "year") - 1

for (j in seq_along(startdates)) {
  buildings = longterm$BLDGNUM
  date_min = startdates[[j]]
  date_max = enddates[[j]]
  print(sprintf("start date: %s-------------------", date_min))
  ghcnd_data_var = ghcnd_data_full %>%
    dplyr::filter(`element` == v) %>%
    {.}
  building_station_file =sprintf("weather_data/ghcnd_/building_station_distance_%s_%s.csv",
                                 v, date_min)
  bad_station_file = sprintf("weather_data/ghcnd_/bad_%s_%s.csv", v, date_min)
  good_station_file = sprintf("weather_data/ghcnd_/good_%s_%s.csv", v, date_min)
  start = 1
  counter = start
  if (file.exists(building_station_file)) {
    result_acc = readr::read_csv(building_station_file)
    buildings = setdiff(buildings, result_acc$BLDGNUM)
  } else {
    result_acc = NULL
  }
  if (file.exists(bad_station_file)) {
    bad_acc = readr::read_csv(bad_station_file)$bad
  } else {
    bad_acc = NULL
  }
  if (file.exists(good_station_file)) {
    good_acc = readr::read_csv(good_station_file)$good
  } else {
    good_acc = NULL
  }
  good_ghcnd <- ghcnd_data_var %>%
    dplyr::filter(!(`id` %in% bad_acc))
  for (idx in seq_along(buildings)) {
    b = buildings[[idx]]
    print(b)
    ## this part only downloads data
    print(sprintf("downloading %s --------------%s -----------", counter, b))
    b_loc = longterm %>%
      dplyr::filter(`BLDGNUM`==b) %>%
      {.}
    acc <- NULL
    good_ghcnd <- good_ghcnd %>%
      dplyr::filter(!(`id` %in% bad_acc)) %>%
      {.}
    ## print(sprintf("size of search space: %s", nrow(good_ghcnd)))
    result = get_nearby_ghcnd_stations_one_loc(lat_lon_df=b_loc,
                                               id_col_name="BLDGNUM",
                                               ghcnd_data=good_ghcnd,
                                               good_acc=good_acc, v=v,
                                               radius=100, limit=5,
                                               date_min=date_min,
                                               date_max=date_max, year=NULL,
                                               exclude_missing_percent=25)
    ## when not enough good stations are found, move to the next
    if (nrow(result$df) == 0) {
      bad_acc <- c(bad_acc, result$bad)
      next
    }
    ## print("final result---------")
    ## print(result$df)
    bad_acc <- c(bad_acc, result$bad)
    good_acc <- c(good_acc, result$good)
    result_acc <- result$df %>%
      dplyr::mutate(BLDGNUM=b) %>%
      dplyr::bind_rows(result_acc)
    if ((idx %% 100 == 0) || idx == length(buildings)) {
      data.frame(bad=bad_acc) %>%
        readr::write_csv(bad_station_file)
      data.frame(good=good_acc) %>%
        readr::write_csv(good_station_file)
      result_acc %>%
        readr::write_csv(building_station_file)
    }
    counter = counter + 1
  }
}
