library("dplyr")

devtools::load_all("~/Dropbox/gsa_2017/get.noaa.weather")

pathname = "weather_data/ghcnd_/"
## startdates = seq(as.Date("1990-01-01"), as.Date("1999-01-01"), "year")
## enddates = seq(as.Date("1991-01-01"), as.Date("2000-01-01"), "year") - 1
startdates = seq(as.Date("2000-01-01"), as.Date("2008-01-01"), "year")
enddates = seq(as.Date("2001-01-01"), as.Date("2009-01-01"), "year") - 1
## startdates = seq(as.Date("2009-01-01"), as.Date("2018-01-01"), "year")
## enddates = seq(as.Date("2010-01-01"), as.Date("2019-01-01"), "year") - 1
v = "TMAX"

for (i in seq_along(startdates)) {
  date_min = startdates[[i]]
  date_max = enddates[[i]]
  filename = sprintf("building_station_distance_%s_%s.csv", v, date_min)
  output.filename = sprintf("building_%s_%s_with_distance.csv", v, date_min)
  print(sprintf("compile weather for %s", filename))
  df <- readr::read_csv(paste0(pathname, filename)) %>%
    dplyr::select(BLDGNUM, id, distance) %>%
    {.}
  good = NULL
  bad = NULL
  if(file.exists(paste0(pathname, output.filename))) {
    result_acc = readr::read_csv(paste0(pathname, output.filename))
    bad = result_acc %>%
      dplyr::filter(is.na(weighted)) %>%
      dplyr::distinct(building)
      {.}
    good = result_acc %>%
      dplyr::filter(!(building %in% bad$building)) %>%
      {.}
    print(sprintf("recompile %d buildings---------", nrow(bad)))
  }
  if (!is.null(bad)) {
    df <- df %>%
      dplyr::filter(BLDGNUM %in% unique(bad$building)) %>%
      {.}
  }
  df_list <- df %>%
    dplyr::group_by(BLDGNUM) %>%
    dplyr::group_split()
  result = lapply(seq_along(df_list), function(j) {
    x = df_list[[j]]
    b = x$BLDGNUM[[1]]
    print(sprintf("%d --------- building %s", j, b))
    output = get.noaa.weather::compile_weather_ghcnd_main(building=b, station_df=x,
                                                 date_min=date_min,
                                                 date_max=date_max, var=v,
                                                 format_fun=get.noaa.weather::format_noaa_temperature)
    output
  })
  dplyr::bind_rows(result) %>%
    dplyr::bind_rows(good) %>%
    readr::write_csv(paste0(pathname, output.filename))
}
