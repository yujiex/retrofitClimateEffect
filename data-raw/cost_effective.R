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

kw = "toplevel"
kw = "highlevel"

## fit toplevel
if (kw == "toplevel") {
  dfs = retrofit_from_db %>%
    dplyr::distinct(Building_Number, Substantial_Completion_Date, high_level_ECM) %>%
    dplyr::mutate(top_level_ECM = ifelse(high_level_ECM %in% c("Advanced Metering", "GSALink", "Building Tuneup or Utility Improvements"), "Operational", "Capital")) %>%
    dplyr::distinct(`Building_Number`, Substantial_Completion_Date, top_level_ECM) %>%
    dplyr::arrange(`Building_Number`, Substantial_Completion_Date, top_level_ECM) %>%
    dplyr::group_by(`Building_Number`, Substantial_Completion_Date) %>%
    dplyr::summarise(top_level_ECM = paste(top_level_ECM, collapse = " and ")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = 1) %>%
    tidyr::spread(top_level_ECM, value, fill=0) %>%
    dplyr::inner_join(cost.lightouch, by=c("Building_Number", "Substantial_Completion_Date")) %>%
    dplyr::inner_join(features, by=c("Building_Number"="BLDGNUM")) %>%
    dplyr::mutate(cost.per.sqft = cost/GROSSSQFT) %>%
    dplyr::select(-`Building_Number`, -`Substantial_Completion_Date`, -cost) %>%
    dplyr::group_by(historic) %>%
    dplyr::group_split() %>%
    {.}
  result = lapply(dfs, function(df) {
    historic = df$historic[[1]]
    cost.est = lm(cost.per.sqft~Capital + Operational + `Capital and Operational`-1, data=df)
    df.result = data.frame(summary(cost.est)$coefficients) %>%
      tibble::rownames_to_column("action") %>%
      dplyr::mutate(historic = historic) %>%
      tibble::as_tibble() %>%
      {.}
  })
} else if (kw == "highlevel") {
  dfs = retrofit_from_db %>%
    dplyr::distinct(Building_Number, Substantial_Completion_Date, high_level_ECM, source_highlevel) %>%
    dplyr::filter(high_level_ECM != "GSALink") %>%
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
    cost.est = lm(cost.per.sqft~`Advanced Metering` + `Building Envelope` + `Building Tuneup or Utility Improvements` + `HVAC` + `Lighting`, data=df)
    df.result = data.frame(summary(cost.est)$coefficients) %>%
      tibble::rownames_to_column("action") %>%
      dplyr::mutate(historic = historic) %>%
      tibble::as_tibble() %>%
      {.}
  })
}

cost.est.result = result %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(action = gsub("`", "", action)) %>%
  {.}

cost.est.result %>%
  readr::write_csv(sprintf("%s_action_cost_with_ci.csv", kw))
cost.est.result %>%
  dplyr::select(action, Estimate, historic) %>%
  dplyr::filter(action != "(Intercept)") %>%
  readr::write_csv(sprintf("%s_action_cost.csv", kw))
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
  readr::write_csv(sprintf("%s_action_cost_wide.csv", kw))

## fit detaillevel
dfs.detail = retrofit_from_db %>%
  dplyr::distinct(Building_Number, Substantial_Completion_Date, detail_level_ECM) %>%
  dplyr::mutate(value = 1) %>%
  na.omit() %>%
  tidyr::spread(detail_level_ECM, value, fill=0) %>%
  dplyr::inner_join(cost.lightouch, by=c("Building_Number", "Substantial_Completion_Date")) %>%
  dplyr::inner_join(features, by=c("Building_Number"="BLDGNUM")) %>%
  dplyr::mutate(cost.per.sqft = cost/GROSSSQFT) %>%
  dplyr::select(-`Building_Number`, -`Substantial_Completion_Date`, -cost) %>%
  dplyr::group_by(historic) %>%
  dplyr::group_split() %>%
  {.}

## fit with lm gives many negative coefs, but implementation cost should not be
## negative
df = dfs.detail[[1]]
historic = df$historic[[1]]
df.model = df %>%
  dplyr::select(-is.office, -historic, -GROSSSQFT, -GSALink) %>%
  {.}

cost.est = lm(cost.per.sqft~., data=df.model)
summary(cost.est)

## fit with non-negative least square, there are many 0 coefs
mat.model = df.model %>%
  dplyr::select(-cost.per.sqft) %>%
  as.matrix()
constraint.reg.result = nnls::nnls(A=mat.model, b=df.model$cost.per.sqft)
tibble::tibble(variable = colnames(mat.model), coef=constraint.reg.result$x) %>%
  print(n=Inf)
