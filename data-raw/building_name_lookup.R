library("dplyr")

con <- DBI::dbConnect(RSQLite::SQLite(), "other_input.db")

df1 = DBI::dbGetQuery(con,
                     "SELECT [Building Number] AS BLDGNUM, [Building Name] AS name FROM Entire_GSA_Building_Portfolio_input") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(`data.source` = "Entire_GSA_Building_Portfolio_input") %>%
  {.}

df2 = DBI::dbGetQuery(con,
                      "SELECT [Building_Number] AS BLDGNUM, [Building_Name] AS name FROM Covered_Facilities_All_Energy_mmBTUs_sheetF13") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(`data.source` = "Covered_Facilities_All_Energy_mmBTUs_sheetF13") %>%
  {.}

df3 = DBI::dbGetQuery(con,
                      "SELECT [Building_Number] AS BLDGNUM, [Building_Name] AS name FROM Covered_Facilities_All_Energy_mmBTUs_sheetF14") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(`data.source` = "Covered_Facilities_All_Energy_mmBTUs_sheetF14") %>%
  {.}

df4 = DBI::dbGetQuery(con, "SELECT [US Agency Designated Covered Facility ID] AS BLDGNUM, [Property Name] AS rawname FROM FY17WeatherNorm_CMU_BldgsOnly_sheet_FY16") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(name = substr(rawname, 10, nchar(rawname)),
                BLDGNUM = substr(rawname, 1, 8)) %>%
  dplyr::mutate(name = ifelse(grepl("^- ", name), substr(name, 3, nchar(name)),
                              name)) %>%
  dplyr::select(-rawname) %>%
  dplyr::mutate(data.source = "FY17WeatherNorm_CMU_BldgsOnly_sheet_FY16") %>%
  {.}

df5 = DBI::dbGetQuery(con, "SELECT [US Agency Designated Covered Facility ID] AS BLDGNUM, [Property Name] AS rawname FROM FY17WeatherNorm_CMU_BldgsOnly_sheet_FY17") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(name = substr(rawname, 10, nchar(rawname)),
                BLDGNUM = substr(rawname, 1, 8)) %>%
  dplyr::mutate(name = ifelse(grepl("^- ", name), substr(name, 3, nchar(name)),
                              name)) %>%
  dplyr::select(-rawname) %>%
  dplyr::mutate(data.source = "FY17WeatherNorm_CMU_BldgsOnly_sheet_FY17") %>%
  {.}

df6 = DBI::dbGetQuery(con, "SELECT [US Agency Designated Covered Facility ID] AS BLDGNUM, [Property Name] AS rawname FROM FY17WeatherNorm_CMU_BldgsOnly_sheet_FY18") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(name = substr(rawname, 10, nchar(rawname)),
                BLDGNUM = substr(rawname, 1, 8)) %>%
  dplyr::mutate(name = ifelse(grepl("^- ", name), substr(name, 3, nchar(name)),
                              name)) %>%
  dplyr::select(-rawname) %>%
  dplyr::mutate(data.source = "FY17WeatherNorm_CMU_BldgsOnly_sheet_FY18") %>%
  {.}

df7 = DBI::dbGetQuery(con, "SELECT [Building_ID] AS BLDGNUM, [Building_Name] as name FROM GSAlink_Buildings_First_55_Opiton_26_Start_Stop_Dates") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(data.source = "GSAlink_Buildings_First_55_Opiton_26_Start_Stop_Dates") %>%
  {.}

df8 = DBI::dbGetQuery(con, "SELECT [Building_Number] AS BLDGNUM, [Building_Name] as name FROM LEED_EB_Building_totals_and_years") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(data.source = "LEED_EB_Building_totals_and_years") %>%
  {.}

df9 = DBI::dbGetQuery(con, "SELECT [Building_Number] AS BLDGNUM, [Building Name] as name FROM euas_database_of_buildings_cmu") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(data.source = "euas_database_of_buildings_cmu") %>%
  {.}

df10 = readr::read_csv("gsa-datathon-cost.csv") %>%
  distinct(Building) %>%
  dplyr::mutate(BLDGNUM = substr(Building, 1, 8)) %>%
  dplyr::mutate(name = substr(Building, 10, nchar(Building))) %>%
  dplyr::select(-Building) %>%
  dplyr::mutate(data.source = "gsa-datathon-cost.csv") %>%
  {.}

building_name = dplyr::bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10) %>%
  dplyr::arrange(BLDGNUM, name, data.source) %>%
  dplyr::group_by(BLDGNUM, name) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  {.}

usethis::use_data(building_name)
