library("dplyr")
library("ncdf4")
library("ncdf4.helpers")
library("weathermetrics")
library("raster")

## load("~/Dropbox/thesis/code/pubPriCmp/data/buildingLatlng.rda")
load("../data/building_location.rda")
load("../data/energy_monthly_web_continental.rda")
load("../data/retrofit.avg.energy.enough.data.rda")

## locations to get weather for
uniqueLocation <- building_location %>%
  dplyr::select(BLDGNUM, Latitude, Longitude) %>%
  dplyr::filter(BLDGNUM %in% unique(energy_monthly_web_continental$BLDGNUM)) %>%
  dplyr::rename(latitude=Latitude, longitude=Longitude) %>%
  distinct(latitude, longitude) %>%
  dplyr::mutate(longitude360=longitude %% 360) %>%
  {.}

## locations to get weather for
lon.pts <- uniqueLocation$longitude360
lat.pts <- uniqueLocation$latitude
extract.pts <- cbind(lon.pts,lat.pts)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## global settings
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## 3 years data for "now"
## setwd("cmip5/bcca/now_rcp45/") ## for rcp now
## start.year = "2011"
## end.year = "2013"
## nsteps = 1096

## 10 years data for "future"

setwd("../future_rcp45/") ## for rcp future
start.year = "2090"
end.year = "2099"
nsteps = 3652

startdate = as.Date(sprintf("%s-01-01", start.year))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## process each download folder
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## for (folder in as.character(0:5)) {
for (folder in as.character(1:21)) {

  ncdfData = ncdf4::nc_open(sprintf("bcca5_%s/Extraction_tasmax.nc", folder))

  proj = ncdf4::ncatt_get(ncdfData, varid=0, attname="Projections")
  projections = unlist(strsplit(proj$value, split=", "))
  print(projections)
  lon = ncdf4::ncvar_get(ncdfData, varid = "longitude")
  ncdf4::nc_close(ncdfData)

  break_labels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90", "Missing")

  for (i in 1:length(projections)) {
    proj = projections[i]
    tasmax = raster::brick(sprintf("bcca5_%s/Extraction_tasmax.nc", folder), lvar=4, level=i)
    tasmin = raster::brick(sprintf("bcca5_%s/Extraction_tasmin.nc", folder), lvar=4, level=i)
    tasavg = (tasmax + tasmin) / 2
    acc = NULL
    ## spatial interpolation at each step
    for (j in 1:nsteps) {
      if (j %% 100 == 0) {
        print(j)
      }
      ext <- raster::extract(tasavg[[j]], extract.pts, method="bilinear")
      df <- data.frame(lat.pts, lon.pts, ext, step=j, proj) %>%
        {.}
      acc <- rbind(acc, df)
    }
    acc %>%
      tibble::as_tibble() %>%
      dplyr::mutate(ext=weathermetrics::celsius.to.fahrenheit(ext)) %>%
      dplyr::mutate(date=startdate + step - 1) %>%
      dplyr::mutate(`value_label`=dplyr::case_when(`ext` < 10 ~ break_labels[1],
                                                    `ext` < 20 ~ break_labels[2],
                                                    `ext` < 30 ~ break_labels[3],
                                                    `ext` < 40 ~ break_labels[4],
                                                    `ext` < 50 ~ break_labels[5],
                                                    `ext` < 60 ~ break_labels[6],
                                                    `ext` < 70 ~ break_labels[7],
                                                    `ext` < 80 ~ break_labels[8],
                                                    `ext` < 90 ~ break_labels[9],
                                                   `ext` < 1000 ~ break_labels[10],
                                                    TRUE ~ break_labels[11])) %>%
      dplyr::mutate(`value_label`=factor(`value_label`, levels=break_labels)) %>%
      dplyr::mutate(`year`=format(date, "%Y")) %>%
      dplyr::mutate(`month`=format(date, "%m")) %>%
      dplyr::select(-date, -step) %>%
      dplyr::group_by(`proj`, lat.pts, lon.pts, `year`, `month`, `value_label`) %>%
      dplyr::summarise(`value_count`=n()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key=`value_label`, value=`value_count`, fill=0) %>%
      dplyr::mutate_at(vars(ends_with(")"), ends_with("0")), funs(ifelse(is.na(.), 0L, .))) %>%
      readr::write_csv(sprintf("monthly_bin_%s_%s_%s.csv", start.year, end.year, proj))
    print(sprintf("write to monthly_bin_%s_%s_%s.csv", start.year, end.year, proj))
  }
}
