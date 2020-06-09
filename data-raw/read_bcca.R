library("dplyr")
library("ncdf4")
## library("ncdf4.helpers")
library("weathermetrics")
library("raster")

## load("~/Dropbox/thesis/code/pubPriCmp/data/buildingLatlng.rda")
load("../data/building_location.rda")
load("../data/energy_monthly_web_continental.rda")
load("../data/retrofit.avg.energy.enough.data.rda")

folders = list.files("cmip5/bcca_now/", "bcca5*")

## ## check downloads
## result = lapply(folders, function(f) {
##   lines = readLines(paste0("cmip5/bcca_now/", f, "/MetaData.txt"))
##   period.line = lines[[5]]
##   period = gsub("Period:                ", "", period.line)
##   model.lines <- readLines(paste0("cmip5/bcca_now/", f, "/Projections5.txt"))
##   model = sapply(model.lines, function (m) {substr(m, 1, nchar(m) - 6)})
##   scenario = sapply(model.lines, function (m) {substr(m, nchar(m) - 4, nchar(m))})
##   return(tibble::tibble("model"=model, "period"=period, "scenario"=scenario, "folder"=f))
## })

## result %>%
##   dplyr::bind_rows() %>%
##   dplyr::distinct(model, period, scenario) %>%
##   dplyr::group_by(period, scenario) %>%
##   dplyr::count() %>%
##   dplyr::ungroup() %>%
##   {.}

exist.location = readr::read_csv("cmip5/bcca/bcca5/canesm2.1.rcp45.csv") %>%
  dplyr::distinct(lat.pts, lon.pts) %>%
  dplyr::mutate(has.data=T) %>%
  {.}

## locations to get weather for
uniqueLocation <- building_location %>%
  dplyr::select(BLDGNUM, Latitude, Longitude) %>%
  dplyr::filter(BLDGNUM %in% unique(energy_monthly_web_continental$BLDGNUM)) %>%
  dplyr::rename(latitude=Latitude, longitude=Longitude) %>%
  distinct(latitude, longitude) %>%
  dplyr::mutate(longitude360=longitude %% 360) %>%
  ## ## only process locations not already with data
  ## dplyr::left_join(exist.location, by=c("latitude"="lat.pts", "longitude360"="lon.pts")) %>%
  ## dplyr::filter(is.na(has.data)) %>%
  {.}

## locations to get weather for
lon.pts <- uniqueLocation$longitude360
lat.pts <- uniqueLocation$latitude
extract.pts <- cbind(lon.pts,lat.pts)

folders = list.files("cmip5/bcca_now/", "bcca5*")
## folders = list.files("cmip5/bcca/", "bcca5*")

for (f in folders[10:34]) {
  ## lines = readLines(paste0("cmip5/bcca/", f, "/MetaData.txt"))
  lines = readLines(paste0("cmip5/bcca_now/", f, "/MetaData.txt"))
  period.line = lines[[5]]
  period = gsub("Period:                ", "", period.line)
  startdate = as.Date(sprintf("%s-01", substr(period, 1, 7)), format='%Y%b-%d')
  if (!stringr::str_detect(period, "2014Feb")) {
    print(startdate)
    next
  }
  print(startdate)
  ## ncdfData = ncdf4::nc_open(paste0("cmip5/bcca/", f, "/Extraction_tasmax.nc"))
  ncdfData = ncdf4::nc_open(paste0("cmip5/bcca_now/", f, "/Extraction_tasmax.nc"))
  proj = ncdf4::ncatt_get(ncdfData, varid=0, attname="Projections")
  projections = unlist(strsplit(proj$value, split=", "))
  nsteps = dim(ncdf4::ncvar_get(ncdfData, varid = "time"))
  print(projections)
  ncdf4::nc_close(ncdfData)
  break_labels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90", "Missing")
  for (i in 1:length(projections)) {
    proj = projections[[i]]
    ## tasmax = raster::brick(sprintf("cmip5/bcca/%s/Extraction_tasmax.nc", f), lvar=4, level=i)
    ## tasmin = raster::brick(sprintf("cmip5/bcca/%s/Extraction_tasmin.nc", f), lvar=4, level=i)
    tasmax = raster::brick(sprintf("cmip5/bcca_now/%s/Extraction_tasmax.nc", f), lvar=4, level=i)
    tasmin = raster::brick(sprintf("cmip5/bcca_now/%s/Extraction_tasmin.nc", f), lvar=4, level=i)
    tasavg = (tasmax + tasmin) / 2
    ## spatial interpolation at each step
    result = lapply(1:nsteps, function(j) {
      if (j %% 100 == 0) {
        print(j)
      }
      ext <- raster::extract(tasavg[[j]], extract.pts, method="bilinear")
      df <- data.frame(lat.pts, lon.pts, ext, step=j, proj=proj) %>%
        {.}
      return(df)
    })
    df.output = dplyr::bind_rows(result) %>%
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
      {.}
    ## output.filename = sprintf("cmip5/bcca/%s/%s.csv", f, proj)
    output.filename = sprintf("cmip5/bcca_now/%s/%s.csv", f, proj)
    if (FALSE) {
      ## if (file.exists(output.filename)) {
      readr::read_csv(output.filename) %>%
        dplyr::bind_rows(df.output) %>%
        readr::write_csv(output.filename)
      print(sprintf("append to %s", output.filename))
    } else {
      dplyr::bind_rows(df.output) %>%
        readr::write_csv(output.filename)
      print(sprintf("write to %s", output.filename))
    }
  }
}
