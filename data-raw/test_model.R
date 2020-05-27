library("dplyr")
library("grf")

## load("../data/retrofit.alldata.rda")
## load("../data/retrofit.alldata.highprop.rda")

load("../data/retrofit_prev_actions_toplevel.rda")
load("../data/retrofit_prev_actions_highlevel.rda")
load("../data/retrofit_prev_actions_joint_highlevel.rda")
load("../data/retrofit_prev_actions_detaillevel.rda")

load("../data/non.action.data.rda")

options(tibble.width=NULL)

## compare results under different climate scenario
load("../data/cmip5.bin.period.rda")

dfs.cmip5 = cmip5.bin.period %>%
  dplyr::filter(period == "2050Jan through 2059Jan",
                scenario == "rcp45") %>%
  dplyr::group_by(scenario, period, model) %>%
  dplyr::group_split() %>%
  {.}

## prev.action: df with previous action
## fuel.and.other: df with other fields
## kw: key words in file naming
fit.cf <- function(prev.action, fuel.and.other, kw) {
  dfs.by.action = prev.action %>%
    dplyr::group_by(target.action) %>%
    dplyr::group_split() %>%
    {.}
  dfs.by.fuel = fuel.and.other %>%
    dplyr::group_by(variable) %>%
    dplyr::group_split() %>%
    {.}
  result = lapply(dfs.by.action, function(df.action) {
    result.inner = lapply(dfs.by.fuel, function(df.fuel) {
      action = df.action$target.action[[1]]
      name.all.actions = names(df.action)
      name.all.actions <- name.all.actions[!(name.all.actions %in% c("Building_Number", "target.action", "target.date"))]
      fuel.type = df.fuel$variable[[1]]
      print(sprintf("action: %s, fuel: %s, %s", action, fuel.type, kw))
      df.allfeature = df.fuel %>%
        dplyr::left_join(df.action, by=c("BLDGNUM"="Building_Number", "Substantial_Completion_Date"="target.date")) %>%
        dplyr::filter(!(is.real.retrofit & is.na(target.action))) %>%
        dplyr::select(-target.action) %>%
        dplyr::mutate_at(name.all.actions,
                         function (x) {ifelse(is.na(x), 0, x)}) %>%
        dplyr::mutate_if(is.logical, as.numeric) %>%
        dplyr::select(-variable) %>%
        {.}
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
  ## prediction for current climate
  dfs.result.prediction %>%
    dplyr::bind_rows() %>%
    readr::write_csv(sprintf("../tables/grf_result_%s.csv", kw))
  dfs.var.importance %>%
    dplyr::bind_rows() %>%
    readr::write_csv(sprintf("../tables/grf_var_importance_%s.csv", kw))
  model.weights = readr::read_csv("climate_model_weights.csv") %>%
    dplyr::select(-`Skill Weight`, -`Uniqueness Weight`) %>%
    na.omit()
  result.outer = lapply(unnested, function(li){
    learned.model = li$model
    old.data = li$df.result
    fuel = old.data$fuel[[1]]
    action = old.data$action[[1]]
    result.inner = lapply(seq_along(dfs.cmip5), function(j) {
      df.climate = dfs.cmip5[[j]]
      period = df.climate$period[[1]]
      scenario = df.climate$scenario[[1]]
      modelname = df.climate$model[[1]]
      print(paste(fuel, action, period, scenario, j, "----", modelname))
      df = df.climate %>%
        dplyr::select(-Latitude, -Longitude, -period, -model, -scenario) %>%
        {.}
      new.data <- old.data %>%
        dplyr::select(-(`<10`:`>90`)) %>%
        dplyr::inner_join(df, by="BLDGNUM") %>%
        {.}
      X.new = new.data %>%
        dplyr::select(-BLDGNUM, -is.real.retrofit, -`Substantial_Completion_Date`, -eui.diff, -predictions, -debiased.error, -excess.error, -action, -fuel, -variance.estimates) %>%
        as.matrix() %>%
        {.}
      prediction.new = predict(learned.model, X.new, estimate.variance = T)
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
  result.outer %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(model.weights, by="model") %>%
    readr::write_csv(sprintf("prediction_under_climate_change_%s.csv", kw))
}

## output retrofit effect estimates
set.seed(0)
fit.cf(retrofit_prev_actions_highlevel, non.action.data, "highlevel")

set.seed(0)
fit.cf(retrofit_prev_actions_toplevel, non.action.data, "toplevel")

set.seed(0)
fit.cf(retrofit_prev_actions_joint_highlevel, non.action.data, "joint_highlevel")

set.seed(0)
fit.cf(retrofit_prev_actions_detaillevel, non.action.data, "detaillevel")
