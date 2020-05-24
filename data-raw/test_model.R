library("dplyr")
library("grf")

load("../data/retrofit.alldata.rda")
load("../data/retrofit.alldata.highprop.rda")

load("../data/retrofit_prev_actions.rda")

dfs.by.action = retrofit_prev_actions %>%
  dplyr::group_by(target.action) %>%
  dplyr::group_split() %>%
  {.}

pre.nonenergy = retrofit.alldata.highprop %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::mutate(is.office = (`Building_Type`=="Office")) %>%
  dplyr::select(-ownership, -variable, -retro.status, -mean.kbtu, -name, -count,
                -start, -end, -`Building_Type`, -`propensity.estimate`, -HDD, -CDD) %>%
  dplyr::distinct() %>%
  dplyr::mutate(REGNNUM = paste0("Region ", REGNNUM)) %>%
  dplyr::mutate(value=T) %>%
  tidyr::spread(REGNNUM, value, fill=F) %>%
  dplyr::mutate(BLDGCAT= paste0("Category ", BLDGCAT)) %>%
  dplyr::mutate(value=T) %>%
  tidyr::spread(BLDGCAT, value, fill=F) %>%
  {.}

pre.energy = retrofit.alldata.highprop %>%
  dplyr::filter(!(variable %in% c("KWDMD", "OIL"))) %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(BLDGNUM, is.real.retrofit,
                `Substantial_Completion_Date`, variable, mean.kbtu, GROSSSQFT) %>%
  dplyr::mutate(kbtu.per.sqft = mean.kbtu/GROSSSQFT) %>%
  dplyr::select(-mean.kbtu, -GROSSSQFT) %>%
  tidyr::spread(variable, kbtu.per.sqft, fill=0) %>%
  {.}

eui.diff.total = retrofit.alldata.highprop %>%
  dplyr::filter(!(variable %in% c("KWDMD", "OIL"))) %>%
  dplyr::select(BLDGNUM, Substantial_Completion_Date, retro.status,
                is.real.retrofit, variable, mean.kbtu, GROSSSQFT) %>%
  dplyr::mutate(kbtu.per.sqft = mean.kbtu / GROSSSQFT) %>%
  dplyr::select(-mean.kbtu, -GROSSSQFT) %>%
  ## dplyr::group_by(BLDGNUM, Substantial_Completion_Date, retro.status,
  ##                 is.real.retrofit) %>%
  ## dplyr::summarise(kbtu.per.sqft = sum(kbtu.per.sqft)) %>%
  tidyr::spread(retro.status, kbtu.per.sqft) %>%
  dplyr::mutate(eui.diff = post - pre) %>%
  dplyr::select(-post, -pre) %>%
  {.}

dfs.by.fuel = pre.nonenergy %>%
  dplyr::left_join(pre.energy, by=c("BLDGNUM", "is.real.retrofit", "Substantial_Completion_Date")) %>%
  dplyr::left_join(eui.diff.total, by=c("BLDGNUM", "is.real.retrofit", "Substantial_Completion_Date")) %>%
  dplyr::filter(variable %in% c("KWHR", "GAS")) %>%
  dplyr::group_by(variable) %>%
  dplyr::group_split() %>%
  {.}

## dfs.by.fuel = retrofit.alldata.highprop %>%
##   ## dplyr::filter(!(variable %in% c("KWDMD", "OIL"))) %>%
##   dplyr::filter(variable %in% c("KWHR", "GAS")) %>%
##   dplyr::group_by(variable) %>%
##   dplyr::group_split() %>%
##   {.}

set.seed(0)

options(tibble.width=NULL)

result = lapply(dfs.by.action, function(df.action) {
  result.inner = lapply(dfs.by.fuel, function(df.fuel) {
    action = df.action$target.action[[1]]
    fuel.type = df.fuel$variable[[1]]
    print(sprintf("action: %s, fuel: %s", action, fuel.type))
    df.allfeature = df.fuel %>%
      dplyr::left_join(df.action, by=c("BLDGNUM"="Building_Number", "Substantial_Completion_Date"="target.date")) %>%
      dplyr::filter(!(is.real.retrofit & is.na(target.action))) %>%
      dplyr::select(-target.action) %>%
      dplyr::mutate_at(vars(`Advanced Metering`:`Lighting`),
                      function (x) {ifelse(is.na(x), 0, x)}) %>%
      dplyr::mutate_if(is.logical, as.numeric) %>%
      dplyr::select(-variable) %>%
      {.}
    df.allfeature %>%
      distinct(BLDGNUM, is.real.retrofit) %>%
      dplyr::group_by(is.real.retrofit) %>%
      dplyr::count() %>%
      dplyr::ungroup()
    X = df.allfeature %>%
      select(-BLDGNUM, -is.real.retrofit, -`Substantial_Completion_Date`, -eui.diff) %>%
      as.matrix() %>%
      {.}
    Y = df.allfeature$eui.diff
    W = df.allfeature$is.real.retrofit
    c.forest <- grf::causal_forest(X, Y, W)
    c.pred <- predict(c.forest, estimate.variance = T)
    ## plot importance
    importance = variable_importance(c.forest)
    df.var.importance = tibble::tibble(variable = colnames(X),
                                      importance = as.vector(importance)) %>%
      dplyr::mutate(action = action, fuel=fuel.type) %>%
      {.}
    df.result = df.allfeature %>%
      dplyr::bind_cols(c.pred) %>%
      dplyr::mutate(action = action, fuel=fuel.type) %>%
      {.}
    return(list("df.result"=df.result, "df.var.importance"=df.var.importance, "model"=c.forest))
  })
})

unnested = unlist(result, recursive = F)

dfs.result.prediction = lapply(unnested, function(li) {
  li$df.result
})

dfs.var.importance = lapply(unnested, function(li) {
  li$df.var.importance
})

## the list of features used
dfs.result.prediction %>%
  dplyr::bind_rows() %>%
  dplyr::select(-starts_with("Region ")) %>%
  dplyr::select(-starts_with("Category ")) %>%
  dplyr::select(-eui.diff, -(predictions:fuel),
                -(BLDGNUM:is.real.retrofit)) %>%
  names() %>%
  {.}

dfs.result.prediction %>%
  dplyr::bind_rows() %>%
  readr::write_csv("../tables/grf_result.csv")

dfs.var.importance %>%
  dplyr::bind_rows() %>%
  readr::write_csv("../tables/grf_var_importance.csv")

## compare results under different climate scenario
load("../data/cmip5.bin.period.rda")

dfs.cmip5 = cmip5.bin.period %>%
  dplyr::filter(period == "2050Jan through 2059Jan",
                scenario == "rcp45") %>%
  dplyr::group_by(scenario, period, model) %>%
  dplyr::group_split() %>%
  {.}

set.seed(0)

result.outer = lapply(unnested, function(li){
  learned.model = li$model
  old.data = li$df.result
  fuel = old.data$fuel[[1]]
  action = old.data$action[[1]]
  result.inner = lapply(dfs.cmip5, function(df.climate) {
    period = df.climate$period[[1]]
    scenario = df.climate$scenario[[1]]
    modelname = df.climate$model[[1]]
    print(paste(fuel, action, period, scenario, modelname))
    df = df.climate %>%
      dplyr::select(-Latitude, -Longitude, -period, -model, -scenario) %>%
      {.}
    print(head(df))
    new.data <- old.data %>%
      dplyr::select(-(`<10`:`>90`)) %>%
      dplyr::inner_join(df, by="BLDGNUM") %>%
      {.}
    X.new = new.data %>%
      dplyr::select(-BLDGNUM, -is.real.retrofit, -`Substantial_Completion_Date`, -eui.diff, -predictions, -debiased.error, -excess.error, -action, -fuel) %>%
      ## reorder data columns
      ## dplyr::select(GROSSSQFT:htdd, `<10`:`>90`, is.office:Lighting) %>%
      as.matrix() %>%
      {.}
    prediction.new = predict(learned.model, X.new, estimate.variance = T)
    print(head(prediction.new$predictions))
    df.inner = old.data %>%
      dplyr::select(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit,
                    action, fuel) %>%
      dplyr::bind_cols(prediction.new) %>%
      dplyr::rename(cmip5.predictions = predictions) %>%
      dplyr::mutate(period=period, scenario=scenario, model=modelname) %>%
      {.}
    return(df.inner)
  })
  df.inner.agg = dplyr::bind_rows(result.inner)
  return(df.inner.agg)
})

model.weights = readr::read_csv("climate_model_weights.csv") %>%
  dplyr::select(-`Skill Weight`, -`Uniqueness Weight`) %>%
  na.omit()

result.outer %>%
  dplyr::bind_rows() %>%
  dplyr::left_join(model.weights, by="model") %>%
  readr::write_csv("prediction_under_climate_change.csv")

plot(grf::get_tree(unnested[[6]]$model, 5))

