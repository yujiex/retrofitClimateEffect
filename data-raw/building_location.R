## get location data for buildings in energy_monthly_web.rda

library("dplyr")
library("readxl")

load("../data/energy_monthly_web.rda")

library("USAboundaries")

## get locatoin
geo.lookup = readr::read_delim("geographical/us-zip-code-latitude-and-longitude.csv", delim=";") %>%
  {.}

building.loc = readxl::read_excel("EUASweb/mar.facilitybuildings.xlsx") %>%
  dplyr::filter(BLDGNUM %in% unique(energy_monthly_web$BLDGNUM)) %>%
  dplyr::select(BLDGNUM, CITY, ZIP, STATE) %>%
  dplyr::rename(State=STATE) %>%
  dplyr::mutate(Zip=substr(ZIP, 1, 5)) %>%
  {.}

result = building.loc %>%
  dplyr::left_join(geo.lookup, by=c("Zip", "State")) %>%
  dplyr::select(-City, -Timezone, -`Daylight savings time flag`, -`geopoint`) %>%
  {.}

got.result = result %>%
  dplyr::filter(!is.na(Latitude)) %>%
  dplyr::mutate(datasource="us-zip-code-latitude-and-longitude.csv", resolution="5 digit zipcode") %>%
  {.}

no.result = result %>%
  dplyr::filter(is.na(Latitude)) %>%
  {.}

second.geo = USAboundaries::us_zipcodes()

second.geo <- second.geo %>%
  tibble::as_tibble() %>%
  dplyr::select(zipcode, geometry) %>%
  tidyr::unnest(geometry) %>%
  dplyr::group_by(zipcode) %>%
  dplyr::mutate(col=c("Longitude", "Latitude")) %>%
  tidyr::spread(key=col, value=geometry) %>%
  dplyr::ungroup() %>%
  dplyr::rename(Zip=zipcode) %>%
  {.}

second.result <- no.result %>%
  dplyr::select(BLDGNUM, CITY, ZIP, State, Zip) %>%
  dplyr::left_join(second.geo, by=c("Zip")) %>%
  {.}

second.got.result <- second.result %>%
  dplyr::filter(!is.na(Latitude)) %>%
  dplyr::mutate(datasource="USAboundaries::us_zipcodes()", resolution="5 digit zipcode") %>%
  {.}

second.no.result <- second.result %>%
  dplyr::filter(is.na(Latitude)) %>%
  dplyr::select(-Latitude, -Longitude) %>%
  {.}

city.geo <- USAboundaries::us_cities() %>%
  tibble::as_tibble() %>%
  dplyr::select(city, state_abbr, geometry) %>%
  dplyr::mutate(stringversion=as.character(geometry)) %>%
  dplyr::select(-geometry) %>%
  dplyr::mutate(stringversion=gsub("c(", "", stringversion, fixed=T)) %>%
  dplyr::mutate(stringversion=gsub(")", "", stringversion, fixed=T)) %>%
  tidyr::separate(stringversion, c("Longitude", "Latitude"), sep=", ") %>%
  dplyr::mutate(CITY=toupper(city)) %>%
  dplyr::rename(State=state_abbr) %>%
  {.}

## ## get the list of city to look up lat lon
## second.no.result %>%
##   dplyr::distinct(CITY, State) %>%
##   dplyr::left_join(city.geo, by=c("CITY", "State")) %>%
##   dplyr::filter(is.na(city)) %>%
##   select(CITY, State) %>%
##   dplyr::filter(CITY != "NOT AVAILABLE") %>%
##   na.omit() %>%
##   readr::write_csv("geographical/manual_city_latlon.csv")

city.latlon.manual <- readr::read_csv("geographical/manual_city_latlon.csv") %>%
  dplyr::mutate_if(is.numeric, as.character) %>%
  dplyr::mutate(datasource="google", resolution="city") %>%
  {.}

city.latlon <- second.no.result %>%
  dplyr::distinct(CITY, State) %>%
  dplyr::left_join(city.geo, by=c("CITY", "State")) %>%
  dplyr::filter(!is.na(city)) %>%
  dplyr::select(-city) %>%
  dplyr::mutate(datasource="USAboundaries::us_cities()", resolution="city") %>%
  dplyr::bind_rows(city.latlon.manual) %>%
  {.}

## everything has an address now
third.result <- second.no.result %>%
  dplyr::left_join(city.latlon, by=c("CITY", "State")) %>%
  dplyr::filter(!is.na(Latitude)) %>%
  dplyr::mutate_at(vars(Latitude, Longitude), as.numeric) %>%
  {.}

building.latlon <- got.result %>%
  dplyr::bind_rows(second.got.result) %>%
  dplyr::bind_rows(third.result) %>%
  {.}

building.latlon %>%
  dplyr::group_by(datasource) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()

building_location <- building.latlon

usethis::use_data(building_location, overwrite = T)
