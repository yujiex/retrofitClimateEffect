library("dplyr")

## This file compiles the tasavg into different climate change scenarios

compiled.data = compiled.data = lapply (c("cmip5/bcca_now/", "cmip5/bcca/"), function(parent.folder) {
  folders = list.files(parent.folder, "bcca5*")
  result = lapply(folders, function(f) {
    lines = readLines(paste0(parent.folder, f, "/MetaData.txt"))
    period.line = lines[[5]]
    period = gsub("Period:                ", "", period.line)
    model.lines <- readLines(paste0(parent.folder, f, "/Projections5.txt"))
    model = sapply(model.lines, function (m) {substr(m, 1, nchar(m) - 6)})
    scenario = sapply(model.lines, function (m) {substr(m, nchar(m) - 4, nchar(m))})
    return(tibble::tibble("model"=model, "period"=period, "scenario"=scenario, "folder"=f))
  })
  ## download summary
  df.cmip5 = dplyr::bind_rows(result) %>%
    dplyr::arrange(scenario, period, model) %>%
    {.}
  dfs = df.cmip5 %>%
    dplyr::arrange(scenario, period, model) %>%
    dplyr::mutate(filename = paste0(model, ".", scenario, ".csv")) %>%
    dplyr::group_by(period, scenario) %>%
    dplyr::group_split() %>%
    {.}
  data = lapply(dfs, function(df) {
    print(paste(df$scenario[[1]], df$period[[1]]))
    result.one.group = lapply(1:nrow(df), function(i) {
      foldername = df$folder[[i]]
      filename = df$filename[[i]]
      df.data = readr::read_csv(sprintf("%s%s/%s", parent.folder, foldername, filename)) %>%
        ## dplyr::filter(Missing == 0) %>%
        dplyr::mutate(model = substr(proj, 1, nchar(proj) - 6)) %>%
        dplyr::mutate(scenario = substr(proj, nchar(proj) - 4, nchar(proj))) %>%
        dplyr::mutate(folder = foldername, file = filename) %>%
        dplyr::select(-proj) %>%
        {.}
    })
    df.one.group = dplyr::bind_rows(result.one.group) %>%
      dplyr::mutate(period = df$period[[1]]) %>%
      {.}
  }) %>%
  dplyr::bind_rows() %>%
  {.}
  return(data)
}) %>%
dplyr::bind_rows() %>%
{.}

compiled.data %>%
  dplyr::left_join(building_location,
                   by=c("lat.pts"="Latitude", "lon.pts"="Longitude360")) %>%
  dplyr::filter(BLDGNUM == "AL0000AB", scenario == "rcp45", model == "access1-0.1", year==2014) %>%
  {.}

compile.data.relabel = compiled.data %>%
  dplyr::filter(!(year %in% c(2059, 2099))) %>%
  dplyr::mutate(period = ifelse(stringr::str_detect(period, c("2005Jan")) |
                                stringr::str_detect(period, "2014Feb") |
                                stringr::str_detect(period, "2011Jan"),
                                "2005Jan through 2019Jan", period)) %>%
  dplyr::distinct() %>%
  {.}

compile.data.relabel %>%
  dplyr::distinct(model, scenario, period) %>%
  readr::write_csv("cmip5_download.csv")

compile.data.relabel %>%
  feather::write_feather("../data/cmip5_concat.feather")

df.summary.all = compile.data.relabel %>%
  dplyr::group_by(scenario, period, model, lat.pts, lon.pts) %>%
  dplyr::summarise(count=n(), missing=sum(Missing)) %>%
  dplyr::ungroup() %>%
  {.}

load("../data/energy_monthly_web_continental.rda")
load("../data/retrofit.avg.energy.enough.data.rda")
load("../data/retrofit.alldata.rda")
load("../data/building_location.rda")

retrofit.buildings = unique(retrofit.alldata$BLDGNUM)

building_location <- building_location %>%
  dplyr::select(BLDGNUM, Latitude, Longitude) %>%
  dplyr::mutate(Longitude360=Longitude %% 360) %>%
  {.}

building_location %>%
  dplyr::group_by(Latitude, Longitude) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Latitude, Longitude, BLDGNUM) %>%
  {.}

df.summary.all <- df.summary.all %>%
  dplyr::mutate(`cmip5 availability`=ifelse(missing > 0, "no data", "with data")) %>%
  dplyr::left_join(building_location,
                   by=c("lat.pts"="Latitude", "lon.pts"="Longitude360")) %>%
  dplyr::rename(Latitude = lat.pts) %>%
  dplyr::select(-lon.pts) %>%
  {.}

df.summary.all %>%
  readr::write_csv("cmip5_summary_count.csv")

library("sf")

to.plot = df.summary.all %>%
  distinct(BLDGNUM, Latitude, Longitude, `cmip5 availability`) %>%
  {.}

## for retrofit data set
cnt = to.plot %>%
  dplyr::filter(BLDGNUM %in% retrofit.buildings) %>%
  dplyr::group_by(`cmip5 availability`) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup() %>%
  {.}

## to.plot %>%
##   dplyr::filter(BLDGNUM %in% retrofit.buildings) %>%
##   dplyr::mutate(`cmip5 availability` =
##                   ifelse(`cmip5 availability` == "with data",
##                          paste0("with data (n=", cnt$`n()`[[2]], ")"),
##                          paste0("no data (n=", cnt$`n()`[[1]], ")"))) %>%
##   ggplot2::ggplot(ggplot2::aes(x=Longitude, y=Latitude,
##                                color=`cmip5 availability`)) +
##   ggplot2::borders("state", colour="black", fill=NA) +
##   ggplot2::coord_quickmap() +
##   ggplot2::geom_point(size=0.5) +
##   ggplot2::ggtitle("retrofit data set cmip5 availability") +
##   ggplot2::theme()
## ggplot2::ggsave("../images/retrofit_cmip5_loc.png", width=5, height=3)

## for long-term data set
load("../data/energy_90_to_18.rda")

longterm.buildings = energy_90_to_18 %>%
  distinct(BLDGNUM) %>%
  .$BLDGNUM

cnt = to.plot %>%
  dplyr::filter(BLDGNUM %in% longterm.buildings) %>%
  dplyr::group_by(`cmip5 availability`) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup() %>%
  {.}

to.plot %>%
  dplyr::filter(BLDGNUM %in% longterm.buildings) %>%
  dplyr::mutate(`cmip5 availability` =
                  ifelse(`cmip5 availability` == "with data",
                         paste0("with data (n=", cnt$`n()`[[2]], ")"),
                         paste0("no data (n=", cnt$`n()`[[1]], ")"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=Longitude, y=Latitude,
                               color=`cmip5 availability`)) +
  ggplot2::ggtitle("long-term energy data set cmip5 availability") +
  ggplot2::borders("state", colour="black", fill=NA) +
  ggplot2::coord_quickmap() +
  ggplot2::geom_point(size=0.5) +
  ggplot2::theme()
ggplot2::ggsave("../images/longterm_cmip5_loc.png", width=5, height=3)

bin.result = lapply(result, function(df) {
  df.bin = df %>%
    dplyr::group_by(lat.pts, lon.pts, year, model, scenario, period) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::ungroup() %>%
    {.}
})

options(tibble.width=Inf)

cmip5.bin.year = compile.data.relabel %>%
  dplyr::group_by(lat.pts, lon.pts, year, model, scenario, period) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(building_location,
                   by=c("lat.pts"="Latitude", "lon.pts"="Longitude360")) %>%
  dplyr::select(-lon.pts) %>%
  dplyr::rename(Latitude=lat.pts) %>%
  dplyr::select(BLDGNUM, Latitude, Longitude, period, year, scenario, model, everything()) %>%
  {.}

load("../data/cmip5.bin.year.rda")

## check how whether all bins summ to 365
cmip5.bin.year %>%
  ## dplyr::filter(Missing == 0) %>%
  dplyr::select(-Missing) %>%
  tidyr::gather(temperature.bin, value, `<10`:`>90`) %>%
  dplyr::group_by(BLDGNUM, period, scenario, year, model) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(value) %>%
  {.}

usethis::use_data(cmip5.bin.year, overwrite = T)

cmip5.bin.period = cmip5.bin.year %>%
  dplyr::filter(!(year %in% c(2014, 2059, 2099))) %>%
  dplyr::select(-year) %>%
  dplyr::group_by(BLDGNUM, Latitude, Longitude, period, scenario, model) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
  dplyr::ungroup() %>%
  ## filter the model who don't appear in all scenario
  dplyr::group_by(BLDGNUM, model) %>%
  dplyr::filter(n() == 6) %>%
  dplyr::ungroup() %>%
  {.}

usethis::use_data(cmip5.bin.period, overwrite = T)

cmip5.bin.period.threeyear = cmip5.bin.year %>%
  dplyr::filter((2054 <= year & year <= 2056) | (2094 <= year & year <= 2096)) %>%
  dplyr::select(-year) %>%
  dplyr::group_by(BLDGNUM, Latitude, Longitude, period, scenario, model) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
  dplyr::ungroup() %>%
  ## filter the model who don't appear in all scenario
  dplyr::group_by(BLDGNUM, model) %>%
  dplyr::filter(n() == 4) %>%
  dplyr::ungroup() %>%
  {.}

usethis::use_data(cmip5.bin.period.threeyear, overwrite = T)
