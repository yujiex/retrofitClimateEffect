library("dplyr")
library("readr")

devtools::load_all("~/Dropbox/gsa_2017/db.interface")

state.abbr.lookup = readr::read_csv("state_abbr.csv") %>%
  {.}

load("../data/address.lookup.rda")

name.lookup = readr::read_csv("gsa-datathon-cost.csv") %>%
  dplyr::select(`Building`) %>%
  tidyr::separate(Building, c("BLDGNUM", "name"), sep="-") %>%
  {.}

address.lookup %>%
  dplyr::left_join(name.lookup, by=c("Building_Number"="BLDGNUM")) %>%
  readr::write_csv("address_lookup.csv")

result = readr::read_csv("leed_manual.csv") %>%
  dplyr::mutate(City = toupper(City), Address = toupper(Address)) %>%
  dplyr::distinct(Address, City, State, Action, Page) %>%
  dplyr::left_join(address.lookup,
                   by=c("Address"="Street_Address", "City"="City",
                        "State"="State")) %>%
  {.}

## 58
matched = result %>%
  dplyr::filter(!is.na(`Building_Number`)) %>%
  dplyr::distinct(Address, City, State, `Building_Number`, Action) %>%
  dplyr::group_by(Address, City, State, Action) %>%
  dplyr::filter(n()==1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(status="found match") %>%
  {.}

## 104
conflict = result %>%
  dplyr::filter(!is.na(`Building_Number`)) %>%
  dplyr::distinct(Address, City, State, `Building_Number`, Action, Page) %>%
  dplyr::group_by(Address, City, State, Action) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(name.lookup, by=c("Building_Number"="BLDGNUM")) %>%
  {.}

conflict %>%
  readr::write_csv("leed_matched_to_many.csv") %>%
  {.}

resolve.conflict = readr::read_csv("leed_matched_to_many_correction.csv") %>%
  dplyr::filter(!is.na(Correct)) %>%
  dplyr::select(-`Building_Number`, -Note) %>%
  dplyr::rename(`Building_Number`=Correct) %>%
  dplyr::mutate(status="manual resolve conflict") %>%
  {.}

cannot.resolve.conflict =
  readr::read_csv("leed_matched_to_many_correction.csv") %>%
  dplyr::group_by(Address, City, State, Action) %>%
  dplyr::filter(sum(!is.na(Correct))==0) %>%
  dplyr::distinct(Address, City, State, Action) %>%
  dplyr::mutate(status="cannot resolve conflict") %>%
  {.}

with.result = resolve.conflict %>%
  dplyr::select(-Page, -name) %>%
  dplyr::bind_rows(matched) %>%
  dplyr::bind_rows(cannot.resolve.conflict) %>%
  {.}

non.matched = result %>%
  dplyr::select(Address:Page) %>%
  dplyr::left_join(with.result, by=c("Address", "City", "State", "Action")) %>%
  dplyr::filter(is.na(`Building_Number`), is.na(status)) %>%
  {.}

non.matched %>%
  readr::write_csv("non_matched_leed_address.csv")

manual.match = readr::read_csv("non_matched_leed_address_fill.csv") %>%
  dplyr::mutate(status=ifelse(is.na(`Building_Number`), "cannot find address",
                                    "manual match address name")) %>%
  {.}

leed.info = readr::read_csv("leed_manual.csv") %>%
  dplyr::mutate(City = toupper(City), Address = toupper(Address)) %>%
  dplyr::select(-Country) %>%
  dplyr::mutate(time = as.POSIXct(time, format="%m/%d/%Y")) %>%
  {.}

leed.lookup = dplyr::bind_rows(matched, resolve.conflict,
                               cannot.resolve.conflict, manual.match) %>%
  dplyr::select(-Page, -name) %>%
  dplyr::left_join(leed.info, by=c("Address", "City", "State", "Action")) %>%
  {.}

sapply(leed.lookup, function(x) {sum(is.na(x))})

usethis::use_data(leed.lookup)
