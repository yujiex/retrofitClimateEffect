library("dplyr")
library("grf")
library("ggplot2")

## load("../data/retrofit.alldata.rda")
## load("../data/retrofit.alldata.highprop.rda")

load("../data/retrofit_prev_actions_toplevel.rda")
load("../data/retrofit_prev_actions_highlevel.rda")
load("../data/retrofit_prev_actions_joint_highlevel.rda")
load("../data/retrofit_prev_actions_detaillevel.rda")

load("../data/non.action.data.rda")

load("../data/non.action.data.binary.rda")

options(tibble.width=NULL)

## compare results under different climate scenario
load("../data/cmip5.bin.period.rda")

load("../data/cmip5.bin.period.threeyear.rda")

## prev.action: df with previous action
## fuel.and.other: df with other fields
## kw: key words in file naming
fit.cf <- function(prev.action, fuel.and.other, yvarname, kw, projected.weather=TRUE, bound.propensity=TRUE) {
  if (bound.propensity) {
    kw = paste0(kw, "_bp")
  }
  dfs.by.action = prev.action %>%
    dplyr::group_by(target.action) %>%
    dplyr::group_split() %>%
    {.}
  if (projected.weather) {
    dfs.by.fuel = fuel.and.other %>%
      ## use climate projected now to build model
      dplyr::filter(model != "measured") %>%
      dplyr::group_by(variable, model, scenario) %>%
      dplyr::group_split() %>%
      {.}
  } else {
    dfs.by.fuel = fuel.and.other %>%
      ## use measured weather to build model
      dplyr::filter(model == "measured") %>%
      dplyr::group_by(variable, model, scenario) %>%
      dplyr::group_split() %>%
      {.}
  }
  result = lapply(dfs.by.action, function(df.action) {
    result.inner = lapply(dfs.by.fuel, function(df.fuel) {
      action = df.action$target.action[[1]]
      modelname = df.fuel$model[[1]]
      scenarioname = df.fuel$scenario[[1]]
      name.all.actions = names(df.action)
      name.all.actions <- name.all.actions[!(name.all.actions %in% c("Building_Number",
                                                                     "target.action",
                                                                     "target.date"))]
      fuel.type = df.fuel$variable[[1]]
      print(sprintf("action: %s, fuel: %s, model: %s, scenario: %s, %s", action,
                    fuel.type, modelname, scenarioname, kw))
      df.allfeature = df.fuel %>%
        dplyr::left_join(df.action,
                         by=c("BLDGNUM"="Building_Number",
                              "Substantial_Completion_Date"="target.date")) %>%
        ## control group (no action) and buildings with target action being some A
        dplyr::filter((!is.real.retrofit) | (!is.na(target.action))) %>%
        dplyr::select(-target.action) %>%
        dplyr::mutate_at(name.all.actions,
                         function (x) {ifelse(is.na(x), 0, x)}) %>%
        dplyr::mutate_if(is.logical, as.numeric) %>%
        dplyr::select(-variable) %>%
        {.}
      X = df.allfeature %>%
        dplyr::select(-BLDGNUM, -is.real.retrofit, -`Substantial_Completion_Date`, -model, -scenario) %>%
        dplyr::select(-yvarname) %>%
        as.matrix() %>%
        {.}
      Y = df.allfeature[[yvarname]]
      W = df.allfeature$is.real.retrofit
      c.forest.initial <- grf::causal_forest(X, Y, W)
      ## bound treatment probability
      if (bound.propensity) {
        W.hat.initial <- c.forest.initial$W.hat
        W.hat.bounded = ifelse(W.hat.initial > 0.95, 0.95, ifelse(W.hat.initial < 0.05, 0.05, W.hat.initial))
        c.forest <- grf::causal_forest(X, Y, W, W.hat=W.hat.bounded)
      } else {
        c.forest = c.forest.initial
      }
      prediction.old <- predict(c.forest, estimate.variance = T)
      if (projected.weather) {
        new.data <- df.allfeature %>%
          dplyr::select(-(`<10`:`>90`)) %>%
          dplyr::inner_join(cmip5.bin.period.threeyear, by=c("BLDGNUM", "model", "scenario")) %>%
          dplyr::select(-Missing, -Latitude, -Longitude) %>%
          {.}
      } else {
        new.data <- df.allfeature %>%
          dplyr::select(-(`<10`:`>90`), -model, -scenario) %>%
          dplyr::inner_join(cmip5.bin.period.threeyear, by="BLDGNUM") %>%
          dplyr::select(-Missing, -Latitude, -Longitude) %>%
          {.}
      }
      ## use all climate scenario
      X.new <- new.data %>%
        dplyr::select(-period, -model, -scenario) %>%
        dplyr::select(-BLDGNUM, -is.real.retrofit, -`Substantial_Completion_Date`) %>%
        dplyr::select(-yvarname) %>%
        dplyr::select(GROSSSQFT:htdd, `<10`:`>90`, everything()) %>%
        as.matrix() %>%
        {.}
      prediction.new = predict(c.forest, X.new, estimate.variance = T)
      df.result = dplyr::bind_cols(df.allfeature, prediction.old) %>%
        dplyr::mutate(period = "now") %>%
        dplyr::bind_rows(dplyr::bind_cols(new.data, prediction.new)) %>%
        dplyr::mutate(action = action, fuel=fuel.type) %>%
        {.}
      importance = variable_importance(c.forest)
      df.var.importance = tibble::tibble(variable = colnames(X),
                                        importance = as.vector(importance)) %>%
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
  if (projected.weather) {
    suf = ""
  } else {
    suf = "_measured_input"
  }
  if (yvarname == "with.leed.post") {
    suf = paste0(suf, "_", "leed")
  }
  output.result = dfs.result.prediction %>%
    dplyr::bind_rows() %>%
    {.}
  output.result %>%
    readr::write_csv(sprintf("../tables/grf_result_%s%s.csv", kw, suf))
  output.result %>%
    dplyr::select(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit,
                  action, fuel, period, model, scenario, predictions,
                  variance.estimates) %>%
    readr::write_csv(sprintf("../tables/grf_result_fewcol_%s%s.csv", kw, suf))
  dfs.var.importance %>%
    dplyr::bind_rows() %>%
    readr::write_csv(sprintf("../tables/grf_var_importance_%s%s.csv", kw, suf))
}

## predicts energy
## output retrofit effect estimates
set.seed(0)
fit.cf(retrofit_prev_actions_highlevel, non.action.data, yvarname="eui.diff", kw="highlevel")

set.seed(0)
fit.cf(retrofit_prev_actions_toplevel, non.action.data, yvarname="eui.diff", "toplevel")

set.seed(0)
fit.cf(retrofit_prev_actions_joint_highlevel, non.action.data, yvarname="eui.diff", "joint_highlevel")

set.seed(0)
fit.cf(retrofit_prev_actions_detaillevel, non.action.data, yvarname="eui.diff", "detaillevel")

## use NOAA measured input
set.seed(0)
fit.cf(retrofit_prev_actions_highlevel, non.action.data, yvarname="eui.diff", "highlevel", projected.weather=FALSE, bound.propensity=TRUE)

set.seed(0)
fit.cf(retrofit_prev_actions_toplevel, non.action.data, yvarname="eui.diff", "toplevel", projected.weather=FALSE, bound.propensity=TRUE)

set.seed(0)
fit.cf(retrofit_prev_actions_joint_highlevel, non.action.data, yvarname="eui.diff", "joint_highlevel", projected.weather=FALSE, bound.propensity=TRUE)

set.seed(0)
fit.cf(retrofit_prev_actions_detaillevel, non.action.data, yvarname="eui.diff", "detaillevel", projected.weather=FALSE, bound.propensity=TRUE)

## predict binary leed
## output retrofit effect estimates
set.seed(0)
fit.cf(retrofit_prev_actions_highlevel, non.action.data.binary, yvarname="with.leed.post", kw="highlevel", projected.weather=FALSE, bound.propensity=TRUE)

set.seed(0)
fit.cf(retrofit_prev_actions_toplevel, non.action.data.binary, yvarname="with.leed.post", "toplevel", projected.weather=FALSE, bound.propensity=TRUE)

set.seed(0)
fit.cf(retrofit_prev_actions_joint_highlevel, non.action.data.binary, yvarname="with.leed.post", "joint_highlevel", projected.weather=FALSE, bound.propensity=TRUE)

set.seed(0)
fit.cf(retrofit_prev_actions_detaillevel, non.action.data.binary, yvarname="with.leed.post", "detaillevel", projected.weather=FALSE, bound.propensity=TRUE)

