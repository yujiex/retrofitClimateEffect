library("dplyr")
library("readr")

load("../data/ghcnd_data_full.rda")

devtools::load_all("~/Dropbox/gsa_2017/get.noaa.weather")

with.weather = readr::read_csv("weather_data/ghcnd_/building_TMIN_2006-01-01.csv") %>%
  distinct(building) %>%
  {.}

data.files = list.files("weather_data/ghcnd_by_building/", "*_TMAX.csv")

downloaded = substr(data.files, 1, 8)

load("../data/retrofit.avg.energy.enough.data.rda")

retrofit.to.download = retrofit.avg.energy.enough.data %>%
  dplyr::distinct(BLDGNUM, `Substantial_Completion_Date`, start, end, retro.status) %>%
  ## dplyr::filter(!(BLDGNUM %in% downloaded)) %>%
  ## dplyr::filter((BLDGNUM %in% with.weather$building)) %>%
  ## dplyr::filter(!(BLDGNUM %in% with.weather$building)) %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, retro.status) %>%
  dplyr::summarise(start=min(start), end=max(end)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(start.year = lubridate::year(start)) %>%
  dplyr::mutate(end.year = lubridate::year(end)) %>%
  {.}

load("../data/building_location.rda")

for (download.year in 2005:2017) {
  df.retro.year = retrofit.to.download %>%
    dplyr::filter(start.year <= download.year,
                  download.year <= end.year) %>%
    dplyr::distinct(BLDGNUM) %>%
    dplyr::left_join(building_location, by="BLDGNUM") %>%
    dplyr::select(BLDGNUM, Latitude, Longitude) %>%
    dplyr::rename(latitude=Latitude, longitude=Longitude) %>%
    {.}
  buildings = df.retro.year$BLDGNUM
  print(length(buildings))
  ## for (v in c("PRCP")) {
    for (v in c("TMAX")) {
    date_min = as.Date(sprintf("%d-01-01", download.year))
    date_max = as.Date(sprintf("%d-12-31", download.year))
    print(sprintf("start date: %s-------------------", date_min))
    ghcnd_data_var = ghcnd_data_full %>%
      dplyr::filter(`element` == v) %>%
      {.}
    ## building_station_file =sprintf("weather_data/ghcnd_retrofit/building_station_distance_%s_%s.csv",
    ##                                v, date_min)
    ## bad_station_file = sprintf("weather_data/ghcnd_retrofit/bad_%s_%s.csv", v, date_min)
    ## good_station_file = sprintf("weather_data/ghcnd_retrofit/good_%s_%s.csv", v, date_min)
    building_station_file =sprintf("weather_data/ghcnd_retrofit_4/building_station_distance_%s_%s.csv",
                                    v, date_min)
    bad_station_file = sprintf("weather_data/ghcnd_retrofit_4/bad_%s_%s.csv", v, date_min)
    good_station_file = sprintf("weather_data/ghcnd_retrofit_4/good_%s_%s.csv", v, date_min)
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
      b_loc = df.retro.year %>%
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
}

dfs = readr::read_csv("retrofit_redownload_weather.csv") %>%
  dplyr::group_by(Year) %>%
  dplyr::group_split() %>%
  {.}

for (df.year in dfs) {
  df.retro.year = df.year %>%
    dplyr::left_join(building_location, by="BLDGNUM") %>%
    dplyr::select(BLDGNUM, Latitude, Longitude) %>%
    dplyr::rename(latitude=Latitude, longitude=Longitude) %>%
    {.}
  buildings = df.retro.year$BLDGNUM
  download.year = df.year$Year[[1]]
  print(length(buildings))
  for (v in c("TMAX", "TMIN")) {
    date_min = as.Date(sprintf("%d-01-01", download.year))
    date_max = as.Date(sprintf("%d-12-31", download.year))
    print(sprintf("start date: %s-------------------", date_min))
    ghcnd_data_var = ghcnd_data_full %>%
      dplyr::filter(`element` == v) %>%
      {.}
    ## building_station_file =sprintf("weather_data/ghcnd_retrofit/building_station_distance_%s_%s.csv",
    ##                                v, date_min)
    ## bad_station_file = sprintf("weather_data/ghcnd_retrofit/bad_%s_%s.csv", v, date_min)
    ## good_station_file = sprintf("weather_data/ghcnd_retrofit/good_%s_%s.csv", v, date_min)
    building_station_file =sprintf("weather_data/ghcnd_retrofit_3/building_station_distance_%s_%s.csv",
                                    v, date_min)
    bad_station_file = sprintf("weather_data/ghcnd_retrofit_3/bad_%s_%s.csv", v, date_min)
    good_station_file = sprintf("weather_data/ghcnd_retrofit_3/good_%s_%s.csv", v, date_min)
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
      b_loc = df.retro.year %>%
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
}

## redownload missing data
dfs = readr::read_csv("need_to_download.csv") %>%
  dplyr::arrange(Year, BLDGNUM) %>%
  dplyr::left_join(building_location, by="BLDGNUM") %>%
  dplyr::select(BLDGNUM, Latitude, Longitude, Year) %>%
  dplyr::rename(latitude=Latitude, longitude=Longitude) %>%
  dplyr::group_by(Year) %>%
  dplyr::group_split()

for (df.retro.year in dfs) {
  buildings = df.retro.year$BLDGNUM
  print(length(buildings))
  download.year = df.retro.year$Year[[1]]
  ## for (v in c("PRCP")) {
  for (v in c("TMAX", "TMIN")) {
    date_min = as.Date(sprintf("%d-01-01", download.year))
    date_max = as.Date(sprintf("%d-12-31", download.year))
    print(sprintf("start date: %s-------------------", date_min))
    ghcnd_data_var = ghcnd_data_full %>%
      dplyr::filter(`element` == v) %>%
      {.}
    building_station_file =sprintf("weather_data/ghcnd_retrofit_6/building_station_distance_%s_%s.csv",
                                    v, date_min)
    bad_station_file = sprintf("weather_data/ghcnd_retrofit_6/bad_%s_%s.csv", v, date_min)
    good_station_file = sprintf("weather_data/ghcnd_retrofit_6/good_%s_%s.csv", v, date_min)
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
      b_loc = df.retro.year %>%
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
}
