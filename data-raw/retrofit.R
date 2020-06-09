## code to prepare `retrofit` dataset goes here

library("readxl")
library("dplyr")
library("lubridate")
library("readr")

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

clean.result.check <- function (df, idcol="BLDGNUM") {
  nbuilding = df %>%
    distinct(!!rlang::sym(idcol)) %>%
    nrow() %>%
    {.}
  nrec = df %>%
    nrow() %>%
    {.}
  sprintf("number of building: %d, number of record: %d", nbuilding, nrec)
}

usethis::use_data(retrofit)

devtools::load_all("~/Dropbox/gsa_2017/db.interface")

retrofit_from_db = db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_ecm") %>%
  dplyr::filter(!is.na(`Substantial_Completion_Date`)) %>%
  dplyr::mutate(Substantial_Completion_Date = gsub("/", "-", Substantial_Completion_Date)) %>%
  dplyr::mutate(Substantial_Completion_Date = gsub(" 00:00:00", "", Substantial_Completion_Date)) %>%
  dplyr::mutate(Substantial_Completion_Date = as.POSIXct(`Substantial_Completion_Date`)) %>%
  ## dplyr::mutate_at(vars(high_level_ECM), recode, "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate(top_level_ECM = ifelse(high_level_ECM %in% c("Advanced Metering", "GSALink", "Building Tuneup or Utility Improvements"), "Operational", "Capital")) %>%
  {.}

db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_ecm") %>%
  dplyr::filter(!is.na(high_level_ECM)) %>%
  dplyr::distinct(Building_Number, high_level_ECM,  Substantial_Completion_Date, source_highlevel) %>%
  dplyr::arrange(Building_Number, high_level_ECM, source_highlevel) %>%
  ## dplyr::distinct(Building_Number, high_level_ECM,  Substantial_Completion_Date) %>%
  dplyr::filter(is.na(Substantial_Completion_Date)) %>%
  distinct(source_highlevel) %>%
  {.}

clean.result.check(retrofit_from_db, "Building_Number")
## [1] "number of building: 310, number of record: 2185"

usethis::use_data(retrofit_from_db, overwrite = T)

retro_long_detail = retrofit_from_db %>%
  dplyr::distinct(`Building_Number`, Substantial_Completion_Date, high_level_ECM, detail_level_ECM) %>%
  dplyr::arrange(`Building_Number`, Substantial_Completion_Date, high_level_ECM, detail_level_ECM) %>%
  tidyr::unite("high.and.detail", high_level_ECM:detail_level_ECM) %>%
  {.}

retro_long = retrofit_from_db %>%
  dplyr::distinct(`Building_Number`, Substantial_Completion_Date, high_level_ECM) %>%
  dplyr::arrange(`Building_Number`, Substantial_Completion_Date, high_level_ECM) %>%
  {.}

retro_long_toplevel = retrofit_from_db %>%
  dplyr::distinct(`Building_Number`, Substantial_Completion_Date, top_level_ECM) %>%
  dplyr::arrange(`Building_Number`, Substantial_Completion_Date, top_level_ECM) %>%
  dplyr::group_by(`Building_Number`, Substantial_Completion_Date) %>%
  dplyr::summarise(top_level_ECM = paste(top_level_ECM, collapse = " and ")) %>%
  dplyr::ungroup() %>%
  {.}

## joint-action high-level
retro_long_joint_highlevel = retro_long %>%
  dplyr::arrange(Building_Number, `Substantial_Completion_Date`, high_level_ECM) %>%
  dplyr::group_by(Building_Number, `Substantial_Completion_Date`) %>%
  dplyr::summarise(joint.highlevel = paste(high_level_ECM, collapse = " and ")) %>%
  dplyr::ungroup() %>%
  {.}

clean.result.check(retro_long, "Building_Number")
## [1] "number of building: 310, number of record: 940"

options(tibble.width=Inf)

## get previous action for top level
get.prev.action <- function (data, colname) {
  dfs = data %>%
    dplyr::group_split(!!rlang::sym(colname)) %>%
    {.}
  result = lapply(dfs, function(x) {
    action = x[[colname]][[1]]
    print(action)
    wide = x %>%
      dplyr::select(-one_of(colname)) %>%
      dplyr::rename(target.date = `Substantial_Completion_Date`) %>%
      dplyr::left_join(data, by=c("Building_Number")) %>%
      ## pre or co-existing actions
      dplyr::mutate(prev.action=as.numeric((`Substantial_Completion_Date` < target.date) |
                                           ((`Substantial_Completion_Date` == target.date) & (!!rlang::sym(colname) != action)))) %>%
      dplyr::select(-`Substantial_Completion_Date`) %>%
      dplyr::distinct() %>%
      ## keep the ones with no prev action, and the ones with any prev action
      dplyr::group_by(`Building_Number`, target.date) %>%
      dplyr::filter(sum(prev.action) == 0 | prev.action == 1) %>%
      dplyr::ungroup() %>%
      tidyr::spread(!!rlang::sym(colname), prev.action, fill=0) %>%
      dplyr::mutate(target.action = action) %>%
      dplyr::select(`Building_Number`, target.action, target.date, everything()) %>%
      dplyr::arrange(`Building_Number`, target.action, target.date) %>%
      {.}
  })
  retrofit_prev_actions = dplyr::bind_rows(result) %>%
    replace(is.na(.), 0) %>%
    {.}
  return(retrofit_prev_actions)
}

## single high-level
retrofit_prev_actions_highlevel = get.prev.action(data=retro_long, colname="high_level_ECM")
usethis::use_data(retrofit_prev_actions_highlevel, overwrite = T)

## joint high-level
retrofit_prev_actions_joint_highlevel = get.prev.action(data=retro_long_joint_highlevel, colname="joint.highlevel")
usethis::use_data(retrofit_prev_actions_joint_highlevel, overwrite = T)

## capital vs operational (top-level)
retrofit_prev_actions_toplevel = get.prev.action(data=retro_long_toplevel, colname="top_level_ECM")
usethis::use_data(retrofit_prev_actions_toplevel, overwrite = T)

## single detail-level
retrofit_prev_actions_detaillevel = get.prev.action(data=retro_long_detail, colname="high.and.detail")
usethis::use_data(retrofit_prev_actions_detaillevel, overwrite = T)

load("../data/retrofit.alldata.rda")
load("../data/energy_monthly_web_withloc.rda")

## 6 months implementation time for these buildings and gsalink
single.commissioning = retro_long %>%
  dplyr::group_by(`Building_Number`, `Substantial_Completion_Date`) %>%
  dplyr::filter("Building Tuneup or Utility Improvements" == `high_level_ECM`, n() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(short.implement = T) %>%
  {.}

single.gsalink = retro_long %>%
  dplyr::group_by(`Building_Number`, `Substantial_Completion_Date`) %>%
  dplyr::filter("GSALink" == `high_level_ECM`, n() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(short.implement = T) %>%
  {.}

hardware = retro_long %>%
  dplyr::group_by(`Building_Number`, `Substantial_Completion_Date`) %>%
  dplyr::filter(!("Building Tuneup or Utility Improvements" == `high_level_ECM` & n() == 1)) %>%
  dplyr::filter(!("GSALink" == `high_level_ECM` & n() == 1)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(short.implement = F) %>%
  {.}

retro_long_adj_time <- dplyr::bind_rows(hardware, single.gsalink, single.commissioning)

time.retro = retro_long_adj_time %>%
  dplyr::distinct(`Building_Number`, `Substantial_Completion_Date`, short.implement) %>%
  dplyr::rename(BLDGNUM = `Building_Number`) %>%
  {.}

## energy for retrofitted buildings
energydata.retro = energy_monthly_web_withloc %>%
  dplyr::filter(BLDGNUM %in% unique(time.retro$`BLDGNUM`)) %>%
  dplyr::select(BLDGNUM, Year, Month, variable, amt.kbtu, GROSSSQFT, BLDGCAT, REGNNUM) %>%
  dplyr::mutate(`time`=sprintf("%04d-%02d-15", `Year`, `Month`)) %>%
  dplyr::mutate(`time`=as.POSIXct(time)) %>%
  {.}

clean.result.check(energydata.retro)
## [1] "number of building: 309, number of record: 254513"

energy.retro = time.retro %>%
  dplyr::inner_join(energydata.retro, by=c("BLDGNUM")) %>%
  {.}

clean.result.check(energy.retro)
## [1] "number of building: 309, number of record: 298014"

energy.pre.period.short.implement <- energy.retro %>%
  dplyr::filter(short.implement) %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`) %>%
  dplyr::filter(`Substantial_Completion_Date` %m-% months(42) <= time,
                time <  `Substantial_Completion_Date` %m-% months(6)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(retro.status = "pre") %>%
  dplyr::mutate(is.real.retrofit = T) %>%
  {.}

energy.pre.period.long.implement <- energy.retro %>%
  dplyr::filter(!short.implement) %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`) %>%
  dplyr::filter(`Substantial_Completion_Date` - lubridate::years(5) <= time,
                time <  `Substantial_Completion_Date` - lubridate::years(2)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(retro.status = "pre") %>%
  dplyr::mutate(is.real.retrofit = T) %>%
  {.}

energy.pre.period = energy.pre.period.short.implement %>%
  dplyr::bind_rows(energy.pre.period.long.implement) %>%
  dplyr::select(-short.implement) %>%
  {.}

energy.post.period <- energy.retro %>%
  dplyr::select(-short.implement) %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`) %>%
  dplyr::filter(`Substantial_Completion_Date` <= time,
                time < `Substantial_Completion_Date` + lubridate::years(3)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(retro.status = "post") %>%
  dplyr::mutate(is.real.retrofit = T) %>%
  {.}

energy.pre.period %>%
  dplyr::bind_rows(energy.post.period) %>%
  clean.result.check()
## [1] "number of building: 307, number of record: 55418"

set.seed(0)

## use random sampled retrofit dates from actual retrofits
fake.time.retro = energy_monthly_web_withloc %>%
  dplyr::distinct(BLDGNUM) %>%
  dplyr::filter(!(BLDGNUM %in% unique(time.retro$`BLDGNUM`))) %>%
  dplyr::mutate(`Substantial_Completion_Date`=sample(time.retro$Substantial_Completion_Date,
                                                     size=nrow(.), replace=T)) %>%
  {.}

usethis::use_data(fake.time.retro, overwrite = T)

energydata.no.retro = energy_monthly_web_withloc %>%
  dplyr::select(BLDGNUM, Year, Month, variable, amt.kbtu, GROSSSQFT, BLDGCAT, REGNNUM) %>%
  dplyr::filter(!(BLDGNUM %in% unique(time.retro$`BLDGNUM`))) %>%
  dplyr::mutate(`time`=sprintf("%04d-%02d-15", `Year`, `Month`)) %>%
  dplyr::mutate(`time`=as.POSIXct(time)) %>%
  {.}

clean.result.check(energydata.no.retro )
## [1] "number of building: 2858, number of record: 795875"

energy.no.retro = fake.time.retro %>%
  dplyr::left_join(energydata.no.retro, by=c("BLDGNUM")) %>%
  {.}

energy.pre.fake.period <- energy.no.retro %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`) %>%
  dplyr::filter(`Substantial_Completion_Date` - lubridate::years(5) <= time,
                time <  `Substantial_Completion_Date` - lubridate::years(2)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(retro.status = "pre") %>%
  dplyr::mutate(is.real.retrofit = F) %>%
  {.}

energy.post.fake.period <- energy.no.retro %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`) %>%
  dplyr::filter(`Substantial_Completion_Date` <= time,
                time < `Substantial_Completion_Date` + lubridate::years(3)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(retro.status = "post") %>%
  dplyr::mutate(is.real.retrofit = F) %>%
  {.}

energy.pre.fake.period %>%
  dplyr::bind_rows(energy.post.fake.period) %>%
  clean.result.check()
## [1] "number of building: 1159, number of record: 106103"

retrofit.energy <- dplyr::bind_rows(energy.pre.period,
                                    energy.post.period,
                                    energy.pre.fake.period,
                                    energy.post.fake.period)

usethis::use_data(retrofit.energy, overwrite = T)

## solving conflict by randomly selecting one building category
## Following are the conflicting ones, no impact on ownership
## retrofit.energy %>%
##   dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, variable, retro.status,
##                   is.real.retrofit, BLDGCAT) %>%
##   dplyr::count() %>%
##   dplyr::ungroup() %>%
##   dplyr::distinct(BLDGNUM, `Substantial_Completion_Date`, variable, retro.status,
##                   is.real.retrofit, BLDGCAT, n) %>%
##   dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, variable, retro.status,
##                   is.real.retrofit) %>%
##   dplyr::filter(length(BLDGCAT[n==max(n)]) > 1) %>%
##   dplyr::ungroup() %>%
##   readr::write_csv("counts.csv")
## BLDGNUM	Substantial_Completion_Date	variable	retro.status	is.real.retrofit	BLDGCAT	n
## AL0028ZZ	2012-03-16T04:00:00Z	KWHR	post	TRUE	A	18
## AL0028ZZ	2012-03-16T04:00:00Z	KWHR	post	TRUE	B	18
## MD0827WO	2012-09-26T04:00:00Z	KWHR	post	FALSE	E	12
## MD0827WO	2012-09-26T04:00:00Z	KWHR	post	FALSE	I	12
## MS0080ZZ	2012-10-05T04:00:00Z	KWHR	pre	  FALSE	A	12
## MS0080ZZ	2012-10-05T04:00:00Z	KWHR	pre	  FALSE	B	12
## ND0000AO	2012-03-19T04:00:00Z	KWHR	pre	  FALSE	B	18
## ND0000AO	2012-03-19T04:00:00Z	KWHR	pre	  FALSE	I	18
## TX0000DL	2011-03-30T04:00:00Z	KWHR	pre	  FALSE	B	18
## TX0000DL	2011-03-30T04:00:00Z	KWHR	pre	  FALSE	I	18

set.seed(0)

category.ownership <- retrofit.energy %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, variable, retro.status,
                  is.real.retrofit, BLDGCAT) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, variable, retro.status,
                  is.real.retrofit) %>%
  dplyr::summarise(BLDGCAT = sample(BLDGCAT[n==max(n)], size=1)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-variable) %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, retro.status,
                  is.real.retrofit) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ownership = ifelse(BLDGCAT %in% c("C", "D"), "leased", "owned")) %>%
  {.}

load("../data/building.type.lookup.rda")

building.type.lookup <- building.type.lookup %>%
  dplyr::select(-`data_source`)

load("../data/historic.building.rda")

gsf.info = retrofit.energy %>%
  dplyr::distinct(BLDGNUM, `Substantial_Completion_Date`, retro.status, time, GROSSSQFT) %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, retro.status) %>%
  dplyr::summarise(GROSSSQFT=mean(GROSSSQFT)) %>%
  dplyr::ungroup() %>%
  {.}

retrofit.avg.energy <- retrofit.energy %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, variable, retro.status,
                  is.real.retrofit) %>%
  dplyr::summarise(mean.kbtu = mean(amt.kbtu),
                   count = n(),
                   REGNNUM = paste(unique(REGNNUM), collapse = ";"),
                   start = min(time), end = max(time)) %>%
  dplyr::ungroup() %>%
  na.omit() %>%
  dplyr::left_join(category.ownership,
                   by=c("BLDGNUM", "Substantial_Completion_Date",
                        "retro.status", "is.real.retrofit")) %>%
  dplyr::left_join(gsf.info, by=c("BLDGNUM", "Substantial_Completion_Date",
                                  "retro.status")) %>%
  dplyr::left_join(building.type.lookup, by=c("BLDGNUM"="Building_Number")) %>%
  dplyr::mutate(historic = (BLDGNUM %in% unique(historic.building$BLDGNUM))) %>%
  {.}

options(tibble.width=Inf)

usethis::use_data(retrofit.avg.energy, overwrite = T)

retrofit.avg.energy.enough.data <- retrofit.avg.energy %>%
  ## has enough count and non-zero
  dplyr::filter(count > 12, mean.kbtu > 0) %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, variable) %>%
  dplyr::filter(n()==2) %>%
  dplyr::ungroup() %>%
  {.}

retrofit.avg.energy.enough.data %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()
## is.real.retrofit `n()`
## <lgl>            <int>
## 1 FALSE              720
## 2 TRUE               294

study.types <- retrofit.avg.energy.enough.data %>%
  dplyr::distinct(BLDGNUM, Building_Type, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit, Building_Type) %>%
  dplyr::summarise(count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Building_Type, is.real.retrofit) %>%
  dplyr::filter(count > 10) %>%
  dplyr::group_by(Building_Type) %>%
  dplyr::filter(n()==2) %>%
  dplyr::ungroup() %>%
  na.omit() %>%
  {.}

retrofit.avg.energy.enough.data <- retrofit.avg.energy.enough.data %>%
  dplyr::filter(Building_Type %in% unique(study.types$Building_Type)) %>%
  {.}

retrofit.avg.energy.enough.data %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()
## is.real.retrofit `n()`
## <lgl>            <int>
## 1 FALSE              529
## 2 TRUE               287

ownership.non.changing = retrofit.avg.energy.enough.data %>%
  dplyr::distinct(BLDGNUM, ownership, retro.status) %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::filter(length(unique(ownership)) == 1) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(BLDGNUM)

retrofit.avg.energy.enough.data <- retrofit.avg.energy.enough.data %>%
  dplyr::filter(BLDGNUM %in% ownership.non.changing$BLDGNUM) %>%
  {.}

retrofit.avg.energy.enough.data %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()

## is.real.retrofit `n()`
## <lgl>            <int>
## 1 FALSE              523
## 2 TRUE               287

## ## see non of the leased buildings are retrofitted
## retrofit.avg.energy.enough.data %>%
##   dplyr::distinct(BLDGNUM, ownership, is.real.retrofit) %>%
##   dplyr::group_by(is.real.retrofit, ownership) %>%
##   dplyr::summarise(n()) %>%
##   dplyr::ungroup()
## is.real.retrofit ownership `n()`
## <lgl>            <chr>     <int>
## 1 FALSE            leased      223
## 2 FALSE            owned       291
## 3 TRUE             owned       286

retrofit.avg.energy.enough.data <- retrofit.avg.energy.enough.data %>%
  dplyr::filter(ownership == "owned") %>%
  {.}

retrofit.avg.energy.enough.data %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()
## is.real.retrofit `n()`
## <lgl>            <int>
## 1 FALSE              296
## 2 TRUE               287

## join with leed
load("../data/leed.lookup.rda")

leed.lookup <- leed.lookup %>%
  dplyr::distinct(Building_Number, Action, time) %>%
  {.}

with.leed.pre.retro <- retro_long %>%
  dplyr::distinct(Building_Number, Substantial_Completion_Date) %>%
  dplyr::filter(Building_Number %in% unique(retrofit.avg.energy.enough.data$BLDGNUM)) %>%
  dplyr::left_join(leed.lookup, by="Building_Number") %>%
  dplyr::filter(difftime(Substantial_Completion_Date, time, "days") > 365) %>%
  dplyr::distinct(Building_Number, Substantial_Completion_Date) %>%
  dplyr::mutate(with.leed = T) %>%
  {.}

retrofit.avg.energy.enough.data <- retrofit.avg.energy.enough.data %>%
  dplyr::left_join(with.leed.pre.retro, by=c("BLDGNUM"="Building_Number",
                                             "Substantial_Completion_Date"="Substantial_Completion_Date")) %>%
  dplyr::mutate(with.leed = ifelse(is.na(with.leed), F, with.leed)) %>%
  {.}

retrofit.avg.energy.enough.data %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit, with.leed) %>%
  dplyr::group_by(is.real.retrofit, with.leed) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()

retrofit.avg.energy.enough.data %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit, with.leed) %>%
  dplyr::filter(with.leed, is.real.retrofit) %>%
  dplyr::left_join(leed.lookup, by=c("BLDGNUM"="Building_Number")) %>%
  dplyr::select(-with.leed, -is.real.retrofit) %>%
  dplyr::arrange(BLDGNUM, time) %>%
  readr::write_csv("temp.csv")

usethis::use_data(retrofit.avg.energy.enough.data, overwrite = T)

load("../data/retrofit.ann.cldd.normal.rda")
load("../data/retrofit.ann.htdd.normal.rda")

retrofit.ann.cldd.normal <- retrofit.ann.cldd.normal %>%
  dplyr::select(BLDGNUM, cldd) %>%
  {.}
retrofit.ann.htdd.normal <- retrofit.ann.htdd.normal %>%
  dplyr::select(BLDGNUM, htdd) %>%
  {.}

retrofit.avg.energy.enough.data <- retrofit.avg.energy.enough.data %>%
  dplyr::inner_join(retrofit.ann.cldd.normal, by="BLDGNUM") %>%
  dplyr::inner_join(retrofit.ann.htdd.normal, by="BLDGNUM") %>%
  {.}

retrofit.avg.energy.enough.data %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()
## is.real.retrofit `n()`
## <lgl>            <int>
## 1 FALSE              296
## 2 TRUE               286

usethis::use_data(retrofit.avg.energy.enough.data, overwrite = T)

load("../data/retrofit.avg.energy.enough.data.rda")

weather.data.building = retrofit.avg.energy.enough.data %>%
  dplyr::distinct(BLDGNUM) %>%
  dplyr::mutate(exists = file.exists(paste0("weather_data/ghcnd_by_building_retrofit/", BLDGNUM, "_TMIN.csv"))) %>%
  dplyr::filter(exists) %>%
  .$BLDGNUM

weather.files = list.files("weather_data/ghcnd_by_building_retrofit/", "_TMIN.csv")
result = lapply(weather.files, function(f) {
  readr::read_csv(paste0("weather_data/ghcnd_by_building_retrofit/", f), col_types = readr::cols()) %>%
    {.}
})
df.tmin = dplyr::bind_rows(result)

weather.files = list.files("weather_data/ghcnd_by_building_retrofit/", "_TMAX.csv")
result = lapply(weather.files, function(f) {
  readr::read_csv(paste0("weather_data/ghcnd_by_building_retrofit/", f), col_types = readr::cols()) %>%
    {.}
})
df.tmax = dplyr::bind_rows(result)

weather.files = list.files("weather_data/ghcnd_by_building", "_PRCP.csv")
result = lapply(weather.files, function(f) {
  readr::read_csv(paste0("weather_data/ghcnd_by_building/", f)) %>%
    {.}
})
df.prcp = dplyr::bind_rows(result)

df.tmin <- df.tmin %>%
  dplyr::select(-ends_with("distance")) %>%
  dplyr::filter(building %in% weather.data.building) %>%
  dplyr::distinct() %>%
  dplyr::mutate(variable = "tmin")

df.tmax <- df.tmax %>%
  dplyr::select(-ends_with("distance")) %>%
  dplyr::filter(building %in% weather.data.building) %>%
  dplyr::distinct() %>%
  dplyr::mutate(variable = "tmax")

df.prcp <- df.prcp %>%
  dplyr::select(-ends_with("distance")) %>%
  dplyr::rename(PRCP=weighted) %>%
  {.}

## optional join precipitation
summary(df.prcp)

lowerbound = min(df.prcp$PRCP)
upperbound = max(df.prcp$PRCP)

df.weather.retro = df.tmin %>%
  dplyr::bind_rows(df.tmax) %>%
  tidyr::spread(variable, weighted) %>%
  na.omit() %>%
  dplyr::mutate(`AVGMINMAX` = (`tmin` + `tmax`) / 2) %>%
  dplyr::mutate(`HDD` = ifelse(`AVGMINMAX` <= 65, 65 - `AVGMINMAX`, 0)) %>%
  dplyr::mutate(`CDD` = ifelse(`AVGMINMAX` > 65, `AVGMINMAX` - 65, 0)) %>%
  {.}

lowerbound = min(df.weather.retro$AVGMINMAX)
upperbound = max(df.weather.retro$AVGMINMAX)
breaks = c(lowerbound, seq(10, 90, by=10), upperbound)
break_labels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90")

## fixme: check missing data
df.weather.retro.bin =
  df.weather.retro %>%
  dplyr::mutate(`value_label`=dplyr::case_when(`AVGMINMAX` < 10 ~ "<10",
                                               `AVGMINMAX` < 20 ~ break_labels[2],
                                               `AVGMINMAX` < 30 ~ break_labels[3],
                                               `AVGMINMAX` < 40 ~ break_labels[4],
                                               `AVGMINMAX` < 50 ~ break_labels[5],
                                               `AVGMINMAX` < 60 ~ break_labels[6],
                                               `AVGMINMAX` < 70 ~ break_labels[7],
                                               `AVGMINMAX` < 80 ~ break_labels[8],
                                               `AVGMINMAX` < 90 ~ break_labels[9],
                                               TRUE ~ break_labels[10]
                                               )) %>%
  dplyr::mutate(`value_label`=factor(`value_label`, levels=break_labels)) %>%
  dplyr::mutate(`year`=as.numeric(format(date, "%Y"))) %>%
  dplyr::mutate(`month`=as.numeric(format(date, "%m"))) %>%
  dplyr::select(-`date`) %>%
  dplyr::group_by(`building`, `year`, `month`, `value_label`) %>%
  dplyr::summarise(`value_count`=n()) %>%
  dplyr::ungroup() %>%
  tidyr::spread(key=`value_label`, value=`value_count`) %>%
  dplyr::mutate_all(funs(ifelse(is.na(.), 0L, .))) %>%
  {.}

df.weather.retro.dd = df.weather.retro %>%
  dplyr::mutate(`year`=as.numeric(format(date, "%Y"))) %>%
  dplyr::mutate(`month`=as.numeric(format(date, "%m"))) %>%
  dplyr::select(-`date`) %>%
  dplyr::group_by(`building`, `year`, `month`) %>%
  dplyr::summarise(HDD=sum(HDD), CDD=sum(CDD), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(date = as.Date(paste0(year, "-", month, "-01"))) %>%
  dplyr::mutate(next.month = as.Date(paste0(strftime(date + 35, format="%Y-%m"), "-01"))) %>%
  dplyr::mutate(num.days = as.integer(next.month - date)) %>%
  dplyr::select(-next.month) %>%
  {.}

## fixme: these months needs to be re-downloaded
## building  year month   HDD    CDD count date       num.days
## <chr>    <dbl> <dbl> <dbl>  <dbl> <int> <date>        <int>
## 1 LA0033ZZ  2007    12 189.   52.9     29 2007-12-01       31
## 2 LA0035ZZ  2007    12 189.   52.9     29 2007-12-01       31
## 3 LA0085ZZ  2007    12 189.   52.9     29 2007-12-01       31
## 4 NM0030ZZ  2015    10 153.    9.45    25 2015-10-01       31
## 5 NY0196ZZ  2015     6  77.9  31.0     28 2015-06-01       30
## 6 PR0000FP  2017     9   0   533.      28 2017-09-01       30
df.weather.retro.dd %>%
  dplyr::filter(count < num.days) %>%
  {.}

df.weather.retro.dd <- df.weather.retro.dd %>%
  dplyr::select(-count, -date, -num.days) %>%
  {.}

## output the set of building-year to download data
retrofit.energy %>%
  dplyr::distinct(BLDGNUM, Substantial_Completion_Date, Year, Month, time, retro.status, is.real.retrofit) %>%
  dplyr::arrange(BLDGNUM, Substantial_Completion_Date, Year, Month) %>%
  dplyr::filter(BLDGNUM %in% unique(retrofit.avg.energy.enough.data$BLDGNUM)) %>%
  dplyr::left_join(df.weather.retro.bin, by=c("BLDGNUM"="building", "Year"="year", "Month"="month")) %>%
  dplyr::left_join(df.weather.retro.dd, by=c("BLDGNUM"="building", "Year"="year", "Month"="month")) %>%
  dplyr::filter(is.na(HDD)) %>%
  dplyr::distinct(BLDGNUM, Year) %>%
  readr::write_csv("need_to_download.csv")

retrofit.weather.measure = retrofit.energy %>%
  dplyr::distinct(BLDGNUM, Substantial_Completion_Date, Year, Month, time, retro.status, is.real.retrofit) %>%
  dplyr::arrange(BLDGNUM, Substantial_Completion_Date, Year, Month) %>%
  dplyr::filter(BLDGNUM %in% unique(retrofit.avg.energy.enough.data$BLDGNUM)) %>%
  dplyr::left_join(df.weather.retro.bin, by=c("BLDGNUM"="building", "Year"="year", "Month"="month")) %>%
  dplyr::left_join(df.weather.retro.dd, by=c("BLDGNUM"="building", "Year"="year", "Month"="month")) %>%
  dplyr::mutate(model="measured", scenario="measured") %>%
  {.}

library("feather")

monthly.climate.projection = feather::read_feather("../data/cmip5_concat.feather") %>%
  tibble::as_tibble() %>%
  {.}

load("../data/building_location.rda")
building_location <- building_location %>%
  dplyr::select(BLDGNUM, Latitude, Longitude) %>%
  dplyr::mutate(Longitude360=Longitude %% 360) %>%
  {.}

projected.weather.now = monthly.climate.projection %>%
  dplyr::filter(period == "2005Jan through 2019Jan") %>%
  dplyr::left_join(building_location,
                   by=c("lat.pts"="Latitude", "lon.pts"="Longitude360")) %>%
  dplyr::mutate(Year=as.numeric(year), Month=as.numeric(month)) %>%
  dplyr::filter(Missing == 0) %>%
  dplyr::select(-lat.pts, -lon.pts, -year, -month, -Missing, -folder, -file, -Longitude, -period) %>%
  {.}

retrofit.weather.projected = retrofit.energy %>%
  dplyr::distinct(BLDGNUM, Substantial_Completion_Date, Year, Month, time, retro.status, is.real.retrofit) %>%
  dplyr::arrange(BLDGNUM, Substantial_Completion_Date, Year, Month) %>%
  dplyr::filter(BLDGNUM %in% unique(retrofit.avg.energy.enough.data$BLDGNUM)) %>%
  dplyr::left_join(projected.weather.now, by=c("BLDGNUM", "Year", "Month")) %>%
  na.omit() %>%
  ## this model is not in rcp45
  dplyr::filter(model != "gfdl-cm3.1") %>%
  {.}

retrofit.weather = retrofit.weather.measure %>%
  dplyr::bind_rows(retrofit.weather.projected) %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, Year, Month) %>%
  dplyr::filter(n() > 1) %>%
  dplyr::ungroup() %>%
  {.}

retrofit.weather %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()
## is.real.retrofit `n()`
## <lgl>            <int>
## 1 FALSE              291
## 2 TRUE               276

usethis::use_data(retrofit.weather, overwrite = T)

building.with.36month.pre.weather = retrofit.weather %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::group_by(BLDGNUM, Substantial_Completion_Date, retro.status, is.real.retrofit) %>%
  dplyr::filter(n()==36 * (19 * 2 + 1)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(BLDGNUM, Substantial_Completion_Date) %>%
  {.}

retrofit.36month.pre.weather <- retrofit.weather %>%
  dplyr::inner_join(building.with.36month.pre.weather, by=c("BLDGNUM", "Substantial_Completion_Date")) %>%
  {.}

retrofit.36month.pre.weather %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()
## is.real.retrofit `n()`
## <lgl>            <int>
## 1 FALSE              282
## 2 TRUE               270

usethis::use_data(building.with.36month.pre.weather, overwrite = T)

retrofit.36month.pre.weather.summary <- retrofit.36month.pre.weather %>%
  dplyr::select(-Year, -Month, -time) %>%
  dplyr::group_by(BLDGNUM, Substantial_Completion_Date, retro.status, is.real.retrofit, model, scenario) %>%
  dplyr::summarise_all(function(x) {sum(x) / 3}) %>%
  dplyr::ungroup() %>%
  {.}

retrofit.alldata = retrofit.avg.energy.enough.data %>%
  dplyr::inner_join(retrofit.36month.pre.weather.summary,
                    by=c("BLDGNUM", "Substantial_Completion_Date", "retro.status", "is.real.retrofit")) %>%
  {.}

retrofit.alldata %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()
## is.real.retrofit `n()`
## <lgl>            <int>
## 1 FALSE              282
## 2 TRUE               270

usethis::use_data(retrofit.alldata, overwrite = T)

## check weather station distance
weather.files = list.files("weather_data/ghcnd_by_building_retrofit/", "_TMIN.csv")
result = lapply(weather.files, function(f) {
  readr::read_csv(paste0("weather_data/ghcnd_by_building_retrofit/", f), col_types = readr::cols()) %>%
    {.}
})
df.tmin = dplyr::bind_rows(result)

df.tmin %>%
  dplyr::filter(building %in% unique(retrofit.alldata$BLDGNUM)) %>%
  dplyr::group_by(building) %>%
  dplyr::summarise(min_distance = min(min_distance),
                   max_distance = max(max_distance)) %>%
  dplyr::ungroup() %>%
  summary() %>%
  {.}
## building          min_distance      max_distance
## Length:552         Min.   : 0.3503   Min.   :10.33
## Class :character   1st Qu.: 2.9534   1st Qu.:21.51
## Mode  :character   Median : 5.0178   Median :31.91
## Mean   : 5.9762   Mean   :33.27
## 3rd Qu.: 8.0925   3rd Qu.:40.87
## Max.   :31.4303   Max.   :98.42

weather.files = list.files("weather_data/ghcnd_by_building_retrofit", "_TMAX.csv")
result = lapply(weather.files, function(f) {
  readr::read_csv(paste0("weather_data/ghcnd_by_building_retrofit/", f)) %>%
    {.}
})
df.tmax = dplyr::bind_rows(result)

df.tmax %>%
  dplyr::filter(building %in% unique(retrofit.alldata$BLDGNUM)) %>%
  dplyr::group_by(building) %>%
  dplyr::summarise(min_distance = min(min_distance),
                   max_distance = max(max_distance)) %>%
  dplyr::ungroup() %>%
  summary() %>%
  {.}
## building          min_distance      max_distance
## Length:552         Min.   : 0.3503   Min.   :10.33
## Class :character   1st Qu.: 2.9534   1st Qu.:21.19
## Mode  :character   Median : 4.9796   Median :31.91
## Mean   : 5.9659   Mean   :33.22
## 3rd Qu.: 8.0524   3rd Qu.:40.75
## Max.   :31.4303   Max.   :98.42

load("../data/retrofit.alldata.rda")

to.estimate = retrofit.alldata %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::mutate(is.office = (`Building_Type`=="Office")) %>%
  dplyr::select(-ownership, -variable, -retro.status, -mean.kbtu, -name, -count,
                -start, -end, -`Building_Type`) %>%
  dplyr::distinct() %>%
  dplyr::mutate(REGNNUM = factor(REGNNUM)) %>%
  dplyr::mutate(BLDGCAT = factor(BLDGCAT)) %>%
  dplyr::arrange(BLDGNUM, `Substantial_Completion_Date`) %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::mutate(has.previous.retrofit = (min(`Substantial_Completion_Date`) != `Substantial_Completion_Date`)) %>%
  dplyr::ungroup() %>%
  {.}

pre.energy = retrofit.alldata %>%
  dplyr::filter(!(variable %in% c("KWDMD", "OIL"))) %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(BLDGNUM, is.real.retrofit,
                `Substantial_Completion_Date`, variable, mean.kbtu, GROSSSQFT) %>%
  dplyr::mutate(kbtu.per.sqft = mean.kbtu/GROSSSQFT) %>%
  dplyr::select(-mean.kbtu, -GROSSSQFT) %>%
  tidyr::spread(variable, kbtu.per.sqft, fill=0) %>%
  {.}

## difference in kbtu per sqft of the sum of electricity, gas, steam, chilled water, Y
eui.diff.total = retrofit.alldata %>%
  dplyr::filter(!(variable %in% c("KWDMD", "OIL", "COAL"))) %>%
  dplyr::select(BLDGNUM, Substantial_Completion_Date, retro.status,
                is.real.retrofit, variable, mean.kbtu, GROSSSQFT) %>%
  dplyr::mutate(kbtu.per.sqft = mean.kbtu / GROSSSQFT) %>%
  dplyr::group_by(BLDGNUM, Substantial_Completion_Date, retro.status,
                  is.real.retrofit) %>%
  dplyr::summarise(kbtu.per.sqft = sum(kbtu.per.sqft)) %>%
  tidyr::spread(retro.status, kbtu.per.sqft) %>%
  dplyr::mutate(eui.diff = post - pre) %>%
  dplyr::select(-post, -pre) %>%
  {.}

to.estimate <- to.estimate %>%
  dplyr::inner_join(pre.energy,
                    by=c("BLDGNUM", "is.real.retrofit",
                         "Substantial_Completion_Date")) %>%
  dplyr::inner_join(eui.diff.total,
                    by=c("BLDGNUM", "is.real.retrofit",
                         "Substantial_Completion_Date")) %>%
  {.}

to.estimate %>%
  dplyr::mutate_if(is.logical, as.numeric) %>%
  readr::write_csv("retrofit_totalEUI_X.csv")

## to.estimate %>%
##   dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
##   dplyr::group_by(is.real.retrofit) %>%
##   dplyr::summarise(n()) %>%
##   dplyr::ungroup()

set.seed(0)

propensity.logit = glm(formula = is.real.retrofit ~ GROSSSQFT + REGNNUM + BLDGCAT + historic + with.leed + cldd + htdd + `<10` + `[10-20)` + `[20-30)` + `[30-40)` + `[40-50)` + `[50-60)` + `[70-80)` + `[80-90)` + `>90` + HDD + CDD + is.office + has.previous.retrofit + CHILLWTR + GAS + KWHR + STEAM,
    family = binomial(), data = to.estimate)

## grf::regression_forest()

with.propensity = to.estimate %>%
  dplyr::distinct(BLDGNUM, Substantial_Completion_Date) %>%
  dplyr::mutate(propensity.estimate = fitted.values(propensity.logit)) %>%
  ## dplyr::filter(0.05 < propensity.estimate, propensity.estimate < 0.95) %>%
  {.}

retrofit.alldata <- retrofit.alldata %>%
  dplyr::left_join(with.propensity, by=c("BLDGNUM", "Substantial_Completion_Date")) %>%
  {.}

usethis::use_data(retrofit.alldata, overwrite = T)

retrofit.alldata.highprop <- retrofit.alldata %>%
  dplyr::filter(0.05 < propensity.estimate, propensity.estimate < 0.95) %>%
  {.}

retrofit.alldata.highprop %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()

usethis::use_data(retrofit.alldata.highprop, overwrite = T)
