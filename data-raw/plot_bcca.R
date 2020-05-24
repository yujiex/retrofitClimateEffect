library("dplyr")
library("readr")
library("ggplot2")

## files = list.files(path="cmip5/bcca/now_rcp85", pattern = "^monthly_bin*")

## length(files)

folder = "2"
ncdfData = ncdf4::nc_open(sprintf("cmip5/bcca/bcca5 %s/Extraction_tasmax.nc", folder))

proj = ncdf4::ncatt_get(ncdfData, varid=0, attname="Projections")
projections = unlist(strsplit(proj$value, split=", "))

print(projections)

lon = ncdf4::ncvar_get(ncdfData, varid = "longitude")
lat = ncdf4::ncvar_get(ncdfData, varid = "latitude")
time = ncdf4::ncvar_get(ncdfData, varid = "time")

ncdfData

tasmax = ncdf4::ncvar_get(ncdfData, varid = "tasmax")

ncdf4::nc_close(ncdfData)

dim(lat)
dim(lon)
dim(time)
dim(tasmax)

length(tasmax[,,1])

tasmax1 = tasmax[,,1]

## first convert to 0-180 longitude
lon180 = (lon + 180) %% 360 - 180

## plot for the whole US
expand.grid(lon180, lat) %>%
  dplyr::rename(lon180 = Var1, lat = Var2) %>%
  dplyr::mutate(lon180 = ifelse(lon180 > 180, -(360 - lon180), lon180),
                tasmax1 = as.vector(tasmax1)) %>%
  ## dplyr::mutate(tasmax1 = convert_temperature(tasmax1, "k", "c")) %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(aes(x = lon180, y = lat, color = tasmax1),
                      size = 0.01) +
  borders("usa", colour="black", fill=NA) +
  viridis::scale_color_viridis(option = "inferno", name = "Temperature (C)") +
  ggplot2::theme_void() +
  ggplot2::coord_quickmap() +
  ggplot2::ggtitle("Modeled max daily surface air temperature on 2011-01-01",
                   subtitle = "cnrm-cm5.1 model, RCP8.5") +
  ggplot2::theme()
ggsave("../images/climate_scenario_grid_us.png", width=8, height=5)

## plot zoomed-in view for Pennsylvania
expand.grid(lon180, lat) %>%
  dplyr::rename(lon180 = Var1, lat = Var2) %>%
  dplyr::mutate(lon180 = ifelse(lon180 > 180, -(360 - lon180), lon180),
                tasmax1 = as.vector(tasmax1)) %>%
  ## dplyr::mutate(tasmax1 = convert_temperature(tasmax1, "k", "c")) %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(aes(x = lon180, y = lat, color = tasmax1),
                      size = 0.01) +
  borders("county", "pennsylvania", colour="black", fill=NA) +
  viridis::scale_color_viridis(option = "inferno", name = "Temperature (C)") +
  ggplot2::theme_void() +
  ggplot2::xlim(-80.56, -74.5) +
  ggplot2::ylim(39.6, 42.3) +
  ggplot2::coord_quickmap() +
  ggplot2::ggtitle("Modeled max daily surface air temperature on 2011-01-01",
                   subtitle = "cnrm-cm5.1 model, RCP8.5, Pennsylvania") +
  ggplot2::theme()
ggsave("../images/zoomed_climate_scenario_grid_us.png", width=8, height=5)
