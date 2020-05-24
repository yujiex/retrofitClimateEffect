library("dplyr")

## fixme: need to process TMIN
## variable = "TMAX"
variable = "TMIN"
inputdir = "ghcnd_"
files = list.files(sprintf("weather_data/%s/", inputdir),
                    pattern = sprintf("^building_%s_[0-9]{4}-01-01_with_distance.csv", variable))

result = lapply(files, function(f) {
  df = readr::read_csv(sprintf("weather_data/%s/%s", inputdir, f))
})

dfs = dplyr::bind_rows(result) %>%
  dplyr::group_by(building) %>%
  dplyr::group_split() %>%
  {.}

lapply(dfs, function(df) {
  name = df$building[[1]]
  df %>%
    readr::write_csv(sprintf("weather_data/ghcnd_longterm_by_building/%s_%s.csv", name,
                             variable))
  return(NULL)
})

## variable = "TMIN"
variable = "TMAX"
dir.suffixes = c("", "_1", "_2", "_3", "_4", "_5", "_6")
outer.result = lapply(dir.suffixes, function(suffix) {
  inputdir = paste0("ghcnd_retrofit", suffix)
  print(inputdir)
  files = list.files(sprintf("weather_data/%s/", inputdir),
                      pattern = sprintf("^building_%s_[0-9]{4}-01-01_with_distance.csv", variable))
  result = lapply(files, function(f) {
    df = readr::read_csv(sprintf("weather_data/%s/%s", inputdir, f))
  })
  return(dplyr::bind_rows(result))
})

dfs = dplyr::bind_rows(outer.result) %>%
  dplyr::group_by(building) %>%
  dplyr::group_split()

lapply(dfs, function(df) {
  name = df$building[[1]]
  df %>%
    dplyr::distinct() %>%
    readr::write_csv(sprintf("weather_data/ghcnd_by_building_retrofit/%s_%s.csv", name,
                             variable))
  return(NULL)
})

## process precipitation for retrofit buildings
variable = "PRCP"
inputdir = "ghcnd_retrofit_2"
files = list.files(sprintf("weather_data/%s/", inputdir),
                   pattern = sprintf("^building_%s_[0-9]{4}-01-01_with_distance.csv", variable))

result = lapply(files, function(f) {
  df = readr::read_csv(sprintf("weather_data/%s/%s", inputdir, f))
})

dfs = dplyr::bind_rows(result) %>%
  dplyr::group_by(building) %>%
  dplyr::group_split() %>%
  {.}

lapply(dfs, function(df) {
  name = df$building[[1]]
  df %>%
    readr::write_csv(sprintf("weather_data/ghcnd_by_building/%s_%s.csv", name,
                             variable))
  return(NULL)
})
