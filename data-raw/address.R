library("dplyr")

con <- DBI::dbConnect(RSQLite::SQLite(), "other_input.db")

df1 = DBI::dbGetQuery(con, "SELECT DISTINCT [Building Number], [Street Address], City, State, [Zip Code] FROM Entire_GSA_Building_Portfolio_input") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(`data_source`="Entire_GSA_Building_Portfolio_input") %>%
  dplyr::mutate(`Zip Code`=substr(`Zip Code`, 1, 5)) %>%
  dplyr::rename(`Zip_Code`=`Zip Code`,
                `Building_Number`=`Building Number`,
                `Street_Address`=`Street Address`) %>%
  {.}

state.abbr.lookup = readr::read_csv("state_abbr.csv") %>%
  {.}

df2 = DBI::dbGetQuery(con, "SELECT DISTINCT Building_Number, Street_Address, City, [State/Province], Zip_Code FROM PortfolioManager_sheet0_input") %>%
  tibble::as_tibble() %>%
  dplyr::left_join(state.abbr.lookup, by=c("State/Province"="State")) %>%
  dplyr::select(-`State/Province`) %>%
  dplyr::rename(State=Abbr) %>%
  dplyr::mutate(`data_source`="PortfolioManager_sheet0_input") %>%
  dplyr::mutate(`Zip_Code`=substr(`Zip_Code`, 1, 5)) %>%
  {.}

df3 = DBI::dbGetQuery(con, "SELECT DISTINCT Building_Number, Street_Address, City, State FROM euas_database_of_buildings_cmu") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(`data_source`="euas_database_of_buildings_cmu") %>%
  {.}

address.lookup =
  dplyr::bind_rows(df1, df2, df3) %>%
  dplyr::select(`Building_Number`, `Street_Address`, `City`, `State`, `data_source`) %>%
  dplyr::mutate_at(vars(`Street_Address`, `City`, `State`), toupper) %>%
  dplyr::group_by(`Building_Number`, `Street_Address`, `City`, `State`) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  {.}

usethis::use_data(address.lookup, overwrite = T)
