## code to prepare `retrofit` dataset goes here

library("readxl")
library("dplyr")


## ARRA retrofit projects
df <- readxl::read_excel("retrofit_related/DtoD downloads/Light-Touch M&V - ARRA Targets to Actuals and Commissioning Details.xlsx",
                   sheet = 2, skip=3) %>%
  dplyr::select(`Building ID`, `Project Name`, `Project Type`,
                `Total ARRA Obligation`, `BA Code`, `ARRA Substantial Completion Date`, `Advanced Metering`,
                `Building Envelope`, `Building Tune Up` , `HVAC`,
                `Indoor Environmental Quality`, `Lighting`, `Renewable Energy`,
                `Water`, `FirstFuel`, `GSA Link`, `E4`) %>%
  dplyr::filter(!is.na(`Project Name`)) %>%
  {.}

retrofit <- df

## ## this shows there's large difference between the documented completion date
## project.date.hpgb = readxl::read_excel("retrofit_related/light touch HPGB/Portfolio HPGB Dashboard 12-18-2015.xlsx (1).xlsx",
##                    sheet="High-Level Scope", skip=3) %>%
##   dplyr::filter(!is.na(`Substantial Completion Date`)) %>%
##   dplyr::select(`Project Name`, `Building ID`, `Substantial Completion Date`) %>%
##   dplyr::mutate(source="HPGB") %>%
##   {.}

## project.date.lt = df %>%
##   dplyr::filter(!is.na(`ARRA Substantial Completion Date`)) %>%
##   dplyr::select(`Project Name`, `Building ID`, `ARRA Substantial Completion Date`) %>%
##   dplyr::rename(`Substantial Completion Date`=`ARRA Substantial Completion Date`) %>%
##   dplyr::mutate(source="LT") %>%
##   {.}

## project.date.hpgb %>%
##   dplyr::bind_rows(project.date.lt) %>%
##   dplyr::distinct(`Project Name`, `Building ID`, `Substantial Completion Date`, `source`) %>%
##   tidyr::spread(source, `Substantial Completion Date`) %>%
##   dplyr::mutate(timediff = difftime(LT, HPGB, units="days")) %>%
##   dplyr::group_by(`Building ID`) %>%
##   dplyr::filter(n()>1) %>%
##   dplyr::arrange(`Building ID`) %>%
##   {.}

usethis::use_data(retrofit)

devtools::load_all("~/Dropbox/gsa_2017/db.interface")

retrofit_from_db = db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_ecm") %>%
  dplyr::filter(!is.na(`Substantial_Completion_Date`)) %>%
  dplyr::mutate(Substantial_Completion_Date = gsub("/", "-", Substantial_Completion_Date)) %>%
  dplyr::mutate(Substantial_Completion_Date = gsub(" 00:00:00", "", Substantial_Completion_Date)) %>%
  dplyr::mutate(Substantial_Completion_Date = as.POSIXct(`Substantial_Completion_Date`)) %>%
  {.}

usethis::use_data(retrofit_from_db, overwrite = T)

retro_long = retrofit_from_db %>%
  dplyr::distinct(`Building_Number`, Substantial_Completion_Date, high_level_ECM) %>%
  dplyr::arrange(`Building_Number`, Substantial_Completion_Date, high_level_ECM) %>%
  {.}

dfs = retro_long %>%
  dplyr::group_split(high_level_ECM) %>%
  {.}

result = lapply(dfs, function(x) {
  action = x$high_level_ECM[[1]]
  wide = x %>%
    dplyr::select(-high_level_ECM) %>%
    dplyr::rename(target.date = `Substantial_Completion_Date`) %>%
    dplyr::left_join(retro_long, by=c("Building_Number")) %>%
    dplyr::mutate(prev.action=as.numeric(`Substantial_Completion_Date` <= target.date)) %>%
    dplyr::select(-`Substantial_Completion_Date`) %>%
    tidyr::spread(high_level_ECM, prev.action, fill=0) %>%
    dplyr::mutate(!!rlang::sym(action) := 0) %>%
    dplyr::mutate(target.action = action) %>%
    dplyr::select(`Building_Number`, target.action, target.date, everything()) %>%
    {.}
})

prev_actions = dplyr::bind_rows(result) %>%
  {.}

usethis::use_data(prev_actions, overwrite = T)

load("../data/energy_monthly_web_withloc.rda")

time.retro = retro_long %>%
  dplyr::distinct(`Building_Number`, `Substantial_Completion_Date`) %>%
  dplyr::rename(BLDGNUM = `Building_Number`) %>%
  {.}

## energy for retrofitted buildings
energydata.retro = energy_monthly_web_withloc %>%
  dplyr::filter(BLDGNUM %in% unique(time.retro$`BLDGNUM`)) %>%
  dplyr::select(BLDGNUM, Year, Month, variable, amt.kbtu, GROSSSQFT) %>%
  dplyr::mutate(`time`=sprintf("%04d-%02d-15", `Year`, `Month`)) %>%
  dplyr::mutate(`time`=as.POSIXct(time)) %>%
  {.}

energy.retro = time.retro %>%
  dplyr::left_join(energydata.retro, by=c("BLDGNUM")) %>%
  {.}

energy.pre.period <- energy.retro %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`) %>%
  dplyr::filter(`Substantial_Completion_Date` - lubridate::years(5) <= time,
                time <  `Substantial_Completion_Date` - lubridate::years(2)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(retro.status = "pre") %>%
  {.}

avg.energy.pre <- energy.pre.period %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, variable) %>%
  dplyr::summarise(mean.kbtu = mean(amt.kbtu),
                   GROSSSQFT = mean(GROSSSQFT),
                   start = min(time),
                   end = max(time)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(retro.status = "pre") %>%
  na.omit() %>%
  {.}

energy.post.period <- energy.retro %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`) %>%
  dplyr::filter(`Substantial_Completion_Date` <= time,
                time <  `Substantial_Completion_Date` + lubridate::years(3)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(retro.status = "post") %>%
  {.}

avg.energy.post <- energy.post.period %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, variable) %>%
  dplyr::summarise(mean.kbtu = mean(amt.kbtu),
                   GROSSSQFT = mean(GROSSSQFT),
                   start = min(time),
                   end = max(time)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(retro.status = "post") %>%
  na.omit() %>%
  {.}

## create a fake retrofit time by median of the actual retrofit dates
median.retro.date =

  time.retro %>%
  dplyr::distinct()

## use similar approach to create energy file for non-retrofitted buildings
