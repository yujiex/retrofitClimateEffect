library("dplyr")

con <- DBI::dbConnect(RSQLite::SQLite(), "other_input.db")

df1 = DBI::dbGetQuery(con, "SELECT DISTINCT Building_Number, [Self-Selected_Primary_Function] AS Building_Type FROM PortfolioManager_sheet0_input") %>%
  tibble::as_data_frame() %>%
  dplyr::mutate(`data_source`="PortfolioManager_sheet0_input::Self-Selected_Primary_Function") %>%
  {.}

df2 = DBI::dbGetQuery(con, "SELECT Building_Number, [GSA Property Type] AS Building_Type FROM euas_database_of_buildings_cmu") %>%
  tibble::as_data_frame() %>%
  dplyr::mutate(`data_source`="euas_database_of_buildings_cmu::[GSA Property Type]") %>%
  {.}

DBI::dbDisconnect(con)

## http://www.dmschools.org/wp-content/uploads/2014/07/List-of-Battle-of-the-Buildings-Competitors-Energy.xlsx
df3 = readxl::read_excel("List-of-Battle-of-the-Buildings-Competitors-Energy.xlsx", skip = 3) %>%
  dplyr::filter(`Submitting Organization`=="The U.S. General Services Administration") %>%
  dplyr::mutate(`Building_Number`=substr(`Property Name`, 1, 8)) %>%
  dplyr::mutate(`name`=substr(`Property Name`, 12, nchar(`Property Name`))) %>%
  dplyr::select(`Building_Number`, `Property Type`) %>%
  dplyr::rename(`Building_Type`=`Property Type`) %>%
  dplyr::mutate(`data_source`="List-of-Battle-of-the-Buildings-Competitors-Energy.xlsx::[Property Type]") %>%
  {.}

## http://www.energystar.gov/sites/default/files/buildings/tools/2012%20ENERGY%20STAR%20Building%20Competition%20Competitor%20List.xls
df4 = readxl::read_excel("2012 ENERGY STAR Building Competition Competitor List.xls", skip=3) %>%
  dplyr::filter(Organization=="GSA") %>%
  dplyr::mutate(`Building_Number`=substr(`Building Name`, 1, 8)) %>%
  dplyr::mutate(`name`=substr(`Building Name`, 12, nchar(`Building Name`))) %>%
  dplyr::select(`Building_Number`, `Building Type`) %>%
  dplyr::rename(`Building_Type`=`Building Type`) %>%
  dplyr::mutate(`data_source`="2012 ENERGY STAR Building Competition Competitor List.xls::[Building Type]") %>%
  {.}

load("../data/building_name.rda")

building_name_unique <- building_name %>%
  dplyr::mutate(name.len=nchar(name)) %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::arrange(desc(nchar(name))) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-name.len, -data.source) %>%
  {.}

dftype = dplyr::bind_rows(df1, df2, df3, df4) %>%
  dplyr::group_by(`Building_Number`, `Building_Type`) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(`Building_Number`, `Building_Type`) %>%
  {.}
  ## dplyr::mutate(`data_source` = factor(`data_source`,
  ##                                       levels = c("euas_database_of_buildings_cmu::[GSA Property Type]",
  ##                                                 "PortfolioManager_sheet0_input::Self-Selected_Primary_Function"))) %>%
  ## dplyr::arrange(`Building_Number`, `data_source`) %>%

## recode type
## "CT/Office"="Courthouse",
## "Mailing Center/Post Office"="Office",
## "All Other"="Other",
## "Non-Refrigerated Warehouse"="Warehouse",
## "Other - Services"="Service",
dftype.recode = dftype %>%
  dplyr::mutate_at(dplyr::vars(`Building_Type`), dplyr::recode,
                   "Office Building"="Office",
                   "Mailing Center/Post Office"="Office",
                   "Non-Refrigerated Warehouse"="Warehouse",
                   "Storage Other than Bldg (OTB)"="Warehouse",
                   "Other - Services"="Service",
                   "Other - Education"="Education",
                   "CT/Office"="Courthouse",
                   "All Other"="Other",
                   "Other - Public Services"="Public services") %>%
  dplyr::group_by(`Building_Number`, `Building_Type`) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  ## "other" does not encode any information, it's equivalent to NA
  dplyr::filter(`Building_Type` != "Other") %>%
  dplyr::arrange(`Building_Number`, `Building_Type`) %>%
  {.}

## only one source has information about the building type
type.unique.1 = dftype.recode %>%
  dplyr::group_by(`Building_Number`) %>%
  dplyr::filter(n()==1) %>%
  dplyr::ungroup() %>%
  {.}

## multiple source has conflicting type information
type.collide = dftype.recode %>%
  dplyr::group_by(`Building_Number`) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  {.}

## office seems to be a place-holder default type, when there are other types
## available, use that instead of office
type.unique.2 <- type.collide %>%
  dplyr::group_by(`Building_Number`) %>%
  dplyr::filter("Office" %in% `Building_Type`) %>%
  dplyr::ungroup() %>%
  dplyr::filter(Building_Type != "Office") %>%
  dplyr::group_by(Building_Number) %>%
  dplyr::filter(n()==1) %>%
  dplyr::ungroup() %>%
  {.}

type.unique.3 <- type.collide %>%
  dplyr::group_by(`Building_Number`) %>%
  dplyr::filter("Office" %in% `Building_Type`) %>%
  dplyr::ungroup() %>%
  dplyr::filter(Building_Type != "Office") %>%
  dplyr::group_by(Building_Number) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(`Building_Number`) %>%
  dplyr::summarise(`Building_Type`=paste(`Building_Type`, collapse = " vs ")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(dplyr::vars(`Building_Type`), dplyr::recode,
                   "Entertainment/PA vs Museum"="Museum",
                   "Public services vs Service"="Public services") %>%
  ## dplyr::left_join(building_name_unique, by=c("Building_Number"="BLDGNUM")) %>%
  dplyr::mutate(`data_source`="from building name") %>%
  {.}

type.unique = type.unique.1 %>%
  dplyr::bind_rows(type.unique.2, type.unique.3) %>%
  {.}

## collect key words for utility systems
dftype %>%
  dplyr::group_by(`Building_Number`) %>%
  dplyr::filter(`Building_Type` %in% "Utility Systems") %>%
  dplyr::ungroup() %>%
  dplyr::select(-data_source) %>%
  dplyr::left_join(building_name, by=c("Building_Number"="BLDGNUM")) %>%
  readr::write_csv("utility_systems_name.csv")

## keywords for utility systems
kw = c("HEATING TUNNELS", "DISTRI TUNNELS", "STEAM SERV", "POWER PLANT",
       "BOILER PLANT", "MECHANICAL EQUIPMENT", "GENERATOR", "PUMPHOUSE",
       "WATER TOWER", "HOTA STM", "HTG PLT", "HEATING PLANT", "UTLY PLANT",
       "BOILERHOUSE", "ELEC SUB STA", "PUMP HOUSE", "WIND TURBINE",
       "GAS METER HOUS", "HTG PLNT", "PWR PLNT", "UTLY PLNT", "INCINERATOR",
       "HTG PLANT", "HEAT PLANT", "STANDBY GEN", " PLNT", "PWR HSE", "UTIL PLA")

is.utility.name <- function(str) {
  result = sapply(kw, function(x) {
    return(stringr::str_detect(str, x))
  })
  result = rowSums(result) > 0
  result <- ifelse(is.na(result), F, result)
  return(result)
}

is.utility.name(c("WEST HTG PLNT STM", "OIEJF"))

type.unique <- type.unique %>%
  dplyr::left_join(building_name_unique, by=c("Building_Number"="BLDGNUM")) %>%
  dplyr::mutate(overwrite = (is.utility.name(name)) & (Building_Type != "Utility Systems")) %>%
  dplyr::mutate(Building_Type = ifelse(overwrite, "Utility Systems", Building_Type)) %>%
  dplyr::mutate(data_source = ifelse(overwrite, "from building name", data_source)) %>%
  dplyr::select(-overwrite) %>%
  {.}

## is.utility.name(c("WEST HTG PLNT STM", "OIEJF"))

building.type.lookup = type.unique

usethis::use_data(building.type.lookup, overwrite = T)
