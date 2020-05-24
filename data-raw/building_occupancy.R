library("dplyr")

con <- DBI::dbConnect(RSQLite::SQLite(), "other_input.db")

building_occupancy = DBI::dbGetQuery(con, "SELECT Building_Number, Occupancy FROM GSA_National_Energy_Reduction_Target_Workbook_FY17_sheet10") %>%
  tibble::as_tibble() %>%
  {.}

DBI::dbDisconnect(con)

usethis::use_data(building_occupancy)

