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

## startdates = seq(as.Date("1990-01-01"), as.Date("2018-12-01"), "month")
## enddates = seq(as.Date("1990-02-01"), as.Date("2019-01-01"), "month") - 1

## try getting all stations
## date_min = startdates[[1]]
## date_max = enddates[[1]]
## ghcnd_data_var = ghcnd_data_full %>%
##   dplyr::filter(`element` == v) %>%
##   {.}

startdates = seq(as.Date("1990-01-01"), as.Date("1999-01-01"), "year")
enddates = seq(as.Date("1991-01-01"), as.Date("2000-01-01"), "year") - 1
## startdates = seq(as.Date("2000-01-01"), as.Date("2008-01-01"), "year")
## enddates = seq(as.Date("2001-01-01"), as.Date("2009-01-01"), "year") - 1
## startdates = seq(as.Date("2009-01-01"), as.Date("2018-01-01"), "year")
## enddates = seq(as.Date("2010-01-01"), as.Date("2019-01-01"), "year") - 1

library("tictoc")

## devtools::load_all("~/Dropbox/gsa_2017/get.noaa.weather")
## tic("download")
## result = get_nearby_ghcnd_stations_all_loc(lat_lon_df=slice(longterm, 1:2),
##                                            id_col_name="BLDGNUM",
##                                            ghcnd_data=ghcnd_data_var, v=v,
##                                            radius=50, limit=5,
##                                            date_min=date_min,
##                                            date_max=date_max, year=NULL,
##                                            resultfile_path="weather_data/ghcnd_/")
## toc()

for (j in seq_along(startdates)) {
  buildings = longterm$BLDGNUM
  date_min = startdates[[j]]
  date_max = enddates[[j]]
  print("date_min")
  print(date_min)
  ghcnd_data_var = ghcnd_data_full %>%
    dplyr::filter(`element` == v) %>%
    {.}
  ## bad = NULL
  building_station_file =sprintf("weather_data/ghcnd_/building_station_distance_%s_%s.csv",
                                 v, date_min)
  bad_station_file = sprintf("weather_data/ghcnd_/bad_%s_%s.csv", v, date_min)
  good_station_file = sprintf("weather_data/ghcnd_/good_%s_%s.csv", v, date_min)
  start = 1
  counter = start
  if (file.exists(building_station_file)) {
    result_acc = readr::read_csv(building_station_file)
    buildings = setdiff(buildings, result_acc$BLDGNUM)
    print(sprintf("length of buildings %d", length(buildings)))
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
    ## weatheri = compile_weather_ghcnd_main(building=b, station_df=result$df,
    ##                           date_min=date_min, date_max=date_max,
    ##                           var=v,
    ##                           format_fun=get.noaa.weather::format_noaa_temperature)
    ## print(weatheri)
    ## weatheri %>%
      ## readr::write_csv(sprintf("weather_data/ghcnd_/building_%s/%s_%s_%s.csv", v, b, v, date_min))
    if ((idx %% 100 == 0) || idx == length(buildings)) {
      data.frame(bad=bad_acc) %>%
        readr::write_csv(bad_station_file)
      data.frame(good=good_acc) %>%
        readr::write_csv(good_station_file)
      result_acc %>%
        readr::write_csv(building_station_file)
    }
    ## when weather file is already downloaded
    ## } else if (!file.exists(sprintf("weather_data/ghcnd_/building_%s/%s_%s_%s.csv",
    ##                                 v, b, v, date_min))){
    ##   print("compile weather for building")
    ##   print(sprintf("%s --------------%s -----------", counter, b))
    ##   df <- readr::read_csv(sprintf("weather_data/ghcnd_/building_%s/%s_station_distance_%s_%s.csv", v, b, v, date_min))
    ##   weatheri = compile_weather_ghcnd_main(building=b, station_df=df,
    ##                             date_min=date_min, date_max=date_max,
    ##                             var=v,
    ##                             format_fun=get.noaa.weather::format_noaa_temperature)
    ##   print(weatheri)
    ##   weatheri %>%
    ##     readr::write_csv(sprintf("weather_data/ghcnd_/building_%s/%s_%s_%s.csv", v, b, v, date_min))
    ## ## when weather output is compiled
    ## } else {
    ##   print(sprintf("%s file exists for %s", v, b))
    ## }
    counter = counter + 1
  }
}

## acc %>%
##   readr::write_csv(sprintf("%s_%s.csv", b, v))

#################################################################

library("dplyr")
library("DBI")
library("readr")
library("rlang")
library("pipeR")
library("feather")
library("ggplot2")

load("~/Dropbox/thesis/code/pubPriCmp/data/buildings.rda")
load("~/Dropbox/thesis/code/pubPriCmp/data/buildingData.rda")
buildingLatlng = buildingData %>%
  dplyr::select(-`Electric_(kBtu)`, -`Gas_(kBtu)`) %>%
  dplyr::group_by(`Name`) %>%
  slice(1) %>%
  ungroup() %>%
  {.}

## #### ## ## ## ## ## ## ## ## ##
## a slow version of downloading weather data building by building (with retry
## when stations has NA or no data) start
## #### ## ## ## ## ## ## ## ## ##
duration = "years"
## all_times = seq(as.Date("2011-01-01"), as.Date("2014-01-01"), duration)
## start_times = all_times[1:(length(all_times) - 1)]
## end_times = all_times[2:length(all_times)]
start_times = seq(as.Date("2011-01-01"), as.Date("2013-01-01"), duration)
end_times = seq(as.Date("2011-12-31"), as.Date("2013-12-31"), duration)

## change this to TMIN or TMAX
## v = "TMIN"
v = "TMAX"
## only search regarding the variables interested
ghcnd_data_var = ghcnd_data_full %>%
  dplyr::filter(`element` == v) %>%
  {.}
devtools::load_all("~/Dropbox/gsa_2017/get.noaa.weather")
## fixme: add error handling for
## Error in curl::curl_fetch_memory(url, handle = handle) :
##            Recv failure: Operation timed out
for (j in 1:3) {
  ## for (j in 1:length(start_times)) {
  date_min = start_times[j]
  date_max = end_times[j]
  bad = NULL
  if (file.exists(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/noaa/bad_%s_%s.csv", v, date_min))) {
    bad_acc = readr::read_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/noaa/bad_%s_%s.csv", v, date_min))$bad
  } else {
    bad_acc = NULL
  }
  good_ghcnd <- ghcnd_data_var %>%
    dplyr::filter(!(`id` %in% bad_acc))
  start = 1
  counter = start
  for (b in buildings[start:length(buildings)]) {
    print(b)
    ## this part only downloads data
    if (!file.exists(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_%s/%s_station_distance_%s_%s.csv",
                            v, b, v, date_min))) {
      print(sprintf("downloading %s --------------%s -----------", counter, b))
      b_loc = buildingLatlng %>%
        dplyr::filter(`Name`==b) %>%
        {.}
      print(b_loc)
      acc <- NULL
      good_ghcnd <- good_ghcnd %>%
        dplyr::filter(!(`id` %in% bad)) %>%
        {.}
      print(sprintf("size of search space: %s", nrow(good_ghcnd)))
      result = get_nearby_ghcnd_stations_one_loc(lat_lon_df=b_loc, id_col_name="Name", ghcnd_data=good_ghcnd, v=v,
                                        radius=100, limit=3,
                                        date_min=date_min, date_max=date_max,
                                        year=NULL)
      print("final result---------")
      print(result$df)
      bad <- result$bad
      bad_acc <- c(bad_acc, bad)
      data.frame(bad=bad_acc) %>%
        readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/noaa/bad_%s_%s.csv", v, date_min))
      result$df %>%
        readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_%s/%s_station_distance_%s_%s.csv",
                                v, b, v, date_min))
    ## when weather file is already downloaded
    } else if (!file.exists(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_%s/%s_%s_%s.csv",
                                    v, b, v, date_min))){
      print("compile weather for building")
      print(sprintf("%s --------------%s -----------", counter, b))
      df <- readr::read_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_%s/%s_station_distance_%s_%s.csv", v, b, v, date_min))
      weatheri = compile_weather_ghcnd_main(building=b, station_df=df,
                                date_min=date_min, date_max=date_max,
                                var=v,
                                format_fun=get.noaa.weather::format_noaa_temperature)
      print(weatheri)
      weatheri %>%
        readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_%s/%s_%s_%s.csv", v, b, v, date_min))
    ## when weather output is compiled
    } else {
      print(sprintf("%s file exists for %s", v, b))
    }
    counter = counter + 1
  }
  ## acc %>%
  ##   readr::write_csv(sprintf("%s_%s.csv", b, v))
}
## #### ## ## ## ## ## ## ## ## ##
## a slow version of downloading weather data end
## #### ## ## ## ## ## ## ## ## ##

## compile individual building's weather data to one data frame
for (b in buildings) {
  ## for (b in buildings[start:length(buildings)]) {
  print(b)
  if (file.exists(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_AVGMINMAX/%s.feather", b))) {
    print("building AVGMINNMAX already created -----")
    next
  }
  acc = NULL
  for (v in c("TMIN", "TMAX")) {
    for (j in 1:3) {
      date_min = start_times[j]
      filename = sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_%s/%s_%s_%s.csv", v, b, v,
                         date_min)
      if (file.exists(filename)) {
        dfvar <- readr::read_csv(filename, col_types=cols()) %>%
          dplyr::mutate(`varname`=v) %>%
          tibble::as_data_frame() %>%
          {.}
        acc <- rbind(acc, dfvar)
      } else {
        print(sprintf("file not exist %s_%s_%s.csv", b, v, date_min))
      }
    }
  }
  acc %>%
    dplyr::arrange(`date`, `varname`) %>%
    dplyr::group_by(`date`, `varname`) %>%
    slice(n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(varname, weighted) %>%
    dplyr::mutate(`AVGMINMAX` = (`TMIN` + `TMAX`) / 2) %>%
    dplyr::mutate(`HDD` = ifelse(`AVGMINMAX` <= 65, 65 - `AVGMINMAX`, 0)) %>%
    dplyr::mutate(`CDD` = ifelse(`AVGMINMAX` > 65, `AVGMINMAX` - 65, 0)) %>%
    feather::write_feather(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_AVGMINMAX/%s.feather", b))
  ## readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_AVGMINMAX/%s.csv", b))
}

## compile individual building's weather station distance data to one data frame
acc_distance = NULL
for (b in buildings) {
  ## for (b in buildings[start:length(buildings)]) {
  print(b)
  for (v in c("TMIN", "TMAX")) {
    for (j in 1:3) {
      date_min = start_times[j]
      filename = sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_%s/%s_station_distance_%s_%s.csv",
                         v, b, v, date_min)
      if (file.exists(filename)) {
        dfvar <- readr::read_csv(filename, col_types=cols()) %>%
          dplyr::mutate(`varname`=v) %>%
          dplyr::mutate(`Name`=b) %>%
          dplyr::mutate(`start_time`=date_min) %>%
          tibble::as_data_frame() %>%
          {.}
        acc_distance <- rbind(acc_distance, dfvar)
      } else {
        print(sprintf("file not exist %s_%s_%s.csv", b, v, date_min))
      }
    }
  }
}

acc_distance %>% feather::write_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/station_distance.feather")

## combine all buildings' weather data into one file
acc = NULL
for (b in buildings) {
  print(b)
  df <- feather::read_feather(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_AVGMINMAX/%s.feather", b)) %>%
    dplyr::mutate(`Name`=b) %>%
    {.}
  acc <- rbind(acc, df)
}
acc %>% feather::write_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/all_weather.feather")

## produce table with columns of AVGMINMAX bins
lowerbound = min(acc$AVGMINMAX)
upperbound = max(acc$AVGMINMAX)
breaks = c(lowerbound, seq(10, 90, by=10), upperbound)
break_labels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90")

avgminmax_bin = acc %>%
  dplyr::select(`date`, `Name`, `AVGMINMAX`) %>%
  dplyr::mutate(`value_label`=dplyr::case_when(`AVGMINMAX` < 10 ~ "<10",
                                               `AVGMINMAX` < 20 ~ break_labels[2],
                                               `AVGMINMAX` < 30 ~ break_labels[3],
                                               `AVGMINMAX` < 40 ~ break_labels[4],
                                               `AVGMINMAX` < 50 ~ break_labels[5],
                                               `AVGMINMAX` < 60 ~ break_labels[6],
                                               `AVGMINMAX` < 70 ~ break_labels[7],
                                               `AVGMINMAX` < 80 ~ break_labels[8],
                                               `AVGMINMAX` < 90 ~ break_labels[9],
                                               TRUE ~ break_labels[10]
                                               )) %>%
  dplyr::mutate(`value_label`=factor(`value_label`, levels=break_labels)) %>%
  dplyr::mutate(`year`=format(date, "%Y")) %>%
  dplyr::mutate(`month`=format(date, "%m")) %>%
  dplyr::select(-`date`) %>%
  dplyr::group_by(`Name`, `year`, `month`, `value_label`) %>%
  dplyr::summarise(`value_count`=n()) %>%
  dplyr::ungroup() %>%
  tidyr::spread(key=`value_label`, value=`value_count`) %>%
  dplyr::mutate_all(funs(ifelse(is.na(.), 0L, .))) %>%
  {.}

avgminmax_bin %>%
  feather::write_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/avgminmax_bin.feather")

## compile monthly degree day
hddcdd = acc %>%
  dplyr::select(`date`, `Name`, `HDD`, `CDD`) %>%
  dplyr::mutate(`year`=format(date, "%Y")) %>%
  dplyr::mutate(`month`=format(date, "%m")) %>%
  dplyr::select(-`date`) %>%
  dplyr::group_by(`Name`, `year`, `month`) %>%
  dplyr::summarise(`HDD`=sum(`HDD`), `CDD`=sum(`CDD`)) %>%
  dplyr::ungroup() %>%
  {.}

hddcdd %>%
  feather::write_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/hddcdd.feather")

building_weather = avgminmax_bin %>%
  dplyr::left_join(hddcdd, by=c("Name", "year", "month")) %>%
  {.}

## get all temperature bin and building weather variables
building_weather %>%
  feather::write_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_weather.feather")

## #### ## ## ## ## ## ## ## ## ##
## This section is for downloading for a set of buildings, haven't finished/tested, neglect for now, START
## #### ## ## ## ## ## ## ## ## ##
## retry for all buildings
v = "TMIN"
## only search regarding the variables interested
ghcnd_data_var = ghcnd_data_full %>%
  dplyr::filter(`element` == v) %>%
  {.}

devtools::load_all("~/Dropbox/gsa_2017/get.noaa.weather")
acc <- NULL
startClock = Sys.time()
for (j in 1:1) {
  ## for (j in 1:length(start_times)) {
  date_min = start_times[j]
  date_max = end_times[j]
  result = get_nearby_ghcnd_stations_all_loc(lat_lon_df=buildingLatlng,
                                             id_col_name="Name",
                                             ghcnd_data=ghcnd_data_full, v=v,
                                             radius=100, limit=5,
                                             date_min=date_min,
                                             date_max=date_max, year=NULL, testing=TRUE)
##   result %>%
##     readr::write_csv(sprintf("%s_station_distance_%s_%s.csv", b, v, date_min))
##   weatheri = compile_weather_ghcnd_main(building=b, station_df=result,
##                             date_min=date_min, date_max=date_max,
##                             var=v,
##                             format_fun=get.noaa.weather::format_noaa_temperature)
##   print(weatheri)
##   acc <- rbind(acc, weatheri)
}
print("Takes time: %s min", Sys.time() - startClock)
## acc %>%
##   readr::write_csv(sprintf("%s_%s.csv", b, v))
## #### ## ## ## ## ## ## ## ## ##
## This section is for downloading for a set of buildings, haven't finished/tested, neglect for now, END
## #### ## ## ## ## ## ## ## ## ##

## get the average of daily mean per month
files = list.files("building_AVGMINMAX")
acc = NULL
for (f in files) {
  print(f)
  building = gsub(".feather", "", f)
  df <- feather::read_feather(paste0("building_AVGMINMAX/", f)) %>%
    dplyr::mutate(`year`=format(`date`, "%Y"),
                  `month`=format(`date`, "%m")) %>%
    dplyr::group_by(`year`, `month`) %>%
    dplyr::summarise(`mean_avgminmax`=mean(`AVGMINMAX`)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(`Name`=building) %>%
    {.}
  acc = rbind(acc, df)
}

acc %>%
  feather::write_feather("building_mean_avgminmax.feather")
