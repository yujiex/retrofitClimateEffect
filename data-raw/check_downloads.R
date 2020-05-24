library("readr")
library("dplyr")

pathname = "weather_data/ghcnd_/"

files = list.files(pathname, "building_station_distance_TMIN*")

for(f in files) {
  df = readr::read_csv(paste0(pathname, f), col_types = cols()) %>%
    dplyr::distinct(BLDGNUM) %>%
    {.}
  if (nrow(df) < 499) {
    print(sprintf("%s: %s buildings", f, nrow(df)))
  }
}

## climate scenario files
files = list.files("cmip5/bcca/", "bcca5*")

result = lapply(files, function(f) {
  lines = readLines(paste0("cmip5/bcca/", f, "/MetaData.txt"))
  period.line = lines[[5]]
  period = gsub("Period:                ", "", period.line)
  model.lines <- readLines(paste0("cmip5/bcca/", f, "/Projections5.txt"))
  model = sapply(model.lines, function (m) {substr(m, 1, nchar(m) - 6)})
  scenario = sapply(model.lines, function (m) {substr(m, nchar(m) - 4, nchar(m))})
  return(tibble::tibble("model"=model, "period"=period, "scenario"=scenario, "folder"=f))
})

df.cmip5 = dplyr::bind_rows(result)

df.cmip5 %>%
  dplyr::select(scenario, period, model) %>%
  dplyr::arrange(scenario, period, model) %>%
  readr::write_csv("cmip5_download.csv")

df.cmip5 %>%
  dplyr::group_by(period, scenario) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(period, scenario) %>%
  {.}

files = list.files("cmip5_prcp/", "bcca5*")

result = lapply(files, function(f) {
  lines = readLines(paste0("cmip5_prcp/", f, "/MetaData.txt"))
  period.line = lines[[5]]
  period = gsub("Period:                ", "", period.line)
  model.lines <- readLines(paste0("cmip5_prcp/", f, "/Projections5.txt"))
  model = sapply(model.lines, function (m) {substr(m, 1, nchar(m) - 6)})
  scenario = sapply(model.lines, function (m) {substr(m, nchar(m) - 4, nchar(m))})
  return(tibble::tibble("model"=model, "period"=period, "scenario"=scenario, "folder"=f))
})

df.cmip5.pr = dplyr::bind_rows(result)

df.cmip5.pr %>%
  dplyr::group_by(period, scenario) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(period, scenario) %>%
  {.}
