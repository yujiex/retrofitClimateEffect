library("dplyr")

load("../data/retrofit_from_db.rda")

cost.lightouch =
  readr::read_csv("ecm/Light-Touch M&V_sheet1_new.csv", skip=3) %>%
  dplyr::select(`Building ID`,
                ## `Project Name`,
                `Total ARRA Obligation`,
                `ARRA Substantial Completion Date`) %>%
  dplyr::rename(cost = `Total ARRA Obligation`,
                `Building_Number`=`Building ID`) %>%
  na.omit() %>%
  dplyr::mutate(`Substantial_Completion_Date`=as.POSIXct(`ARRA Substantial Completion Date`, format="%m/%d/%Y"),
                cost = gsub("$", "", cost, fixed=T),
                cost = as.numeric(gsub(",", "", cost, fixed=T))
                ) %>%
  dplyr::select(-`ARRA Substantial Completion Date`) %>%
  {.}

load("../data/retrofit.alldata.rda")

features = retrofit.alldata %>%
  dplyr::filter(retro.status == "pre", is.real.retrofit) %>%
  dplyr::distinct(BLDGNUM, GROSSSQFT, `Building_Type`, historic) %>%
  dplyr::mutate(is.office = as.numeric(`Building_Type`=="Office"),
                historic=as.numeric(historic)) %>%
  dplyr::select(-`Building_Type`) %>%
  {.}

## HPDB does not have cost info
dfs = retrofit_from_db %>%
  dplyr::distinct(Building_Number, Substantial_Completion_Date, high_level_ECM, source_highlevel) %>%
  ## distinct(source_highlevel) %>%
  dplyr::mutate(value = 1) %>%
  tidyr::spread(high_level_ECM, value, fill=0) %>%
  dplyr::left_join(cost.lightouch, by=c("Building_Number", "Substantial_Completion_Date")) %>%
  dplyr::left_join(features, by=c("Building_Number"="BLDGNUM")) %>%
  na.omit() %>%
  dplyr::mutate(cost.per.sqft = cost/GROSSSQFT) %>%
  dplyr::select(-`Building_Number`, -`Substantial_Completion_Date`,
                -`source_highlevel`, -cost) %>%
  ## dplyr::group_by(historic, is.office) %>%
  dplyr::group_by(historic) %>%
  dplyr::group_split() %>%
  {.}

result = lapply(dfs, function(df) {
  historic = df$historic[[1]]
  ## office = df$is.office[[1]]
  cost.est = lm(cost.per.sqft~`Advanced Metering` + `Building Envelope` + `Building Tuneup or Utility Improvements` + `HVAC` + `Lighting`, data=df)
  df.result = data.frame(summary(cost.est)$coefficients) %>%
    tibble::rownames_to_column("action") %>%
    dplyr::mutate(historic = historic) %>%
    ## dplyr::mutate(historic = historic, is.office = office) %>%
    tibble::as_tibble() %>%
    {.}
})

cost.est.result = result %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(action = gsub("`", "", action)) %>%
  {.}

cost.est.result %>%
  readr::write_csv("action_cost_with_ci.csv")

cost.est.result %>%
  dplyr::select(action, Estimate, historic) %>%
  dplyr::filter(action != "(Intercept)") %>%
  readr::write_csv("action_cost.csv")

est = cost.est.result %>%
  dplyr::select(action, Estimate, historic) %>%
  dplyr::filter(action != "(Intercept)") %>%
  tidyr::spread(action, Estimate) %>%
  dplyr::mutate(field = "estimate") %>%
  {.}
err = cost.est.result %>%
  dplyr::select(action, `Std..Error`, historic) %>%
  dplyr::filter(action != "(Intercept)") %>%
  tidyr::spread(action, `Std..Error`) %>%
  dplyr::mutate(field = "stderr") %>%
  {.}

est %>%
  dplyr::bind_rows(err) %>%
  dplyr::arrange(historic, field) %>%
  dplyr::select(field, everything()) %>%
  readr::write_csv("action_cost_wide.csv")
