library("dplyr")
library("xtable")
library("kableExtra")

load("../data/retrofit.alldata.rda")
load("../data/retrofit.alldata.highprop.rda")
load("../data/retrofit_from_db.rda")

tabledir = "~/Dropbox/thesis/code/retrofitClimateEffect/tables"

## global settings
table.fontsize = 10

highlevel = retrofit_from_db %>%
  dplyr::distinct(`Building_Number`, `Substantial_Completion_Date`, high_level_ECM) %>%
  {.}

retrofit.alldata.highprop %>%
  dplyr::distinct(BLDGNUM, `Substantial_Completion_Date`) %>%
  dplyr::inner_join(highlevel, by=c("BLDGNUM"="Building_Number", "Substantial_Completion_Date"="Substantial_Completion_Date")) %>%
  dplyr::group_by(high_level_ECM) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  readr::write_csv(sprintf("%s/retrofit_count_highlevel.csv", tabledir))

detaillevel = retrofit_from_db %>%
  dplyr::distinct(`Building_Number`, `Substantial_Completion_Date`, high_level_ECM, detail_level_ECM) %>%
  {.}

retrofit.alldata.highprop %>%
  dplyr::distinct(BLDGNUM, `Substantial_Completion_Date`) %>%
  dplyr::inner_join(detaillevel, by=c("BLDGNUM"="Building_Number", "Substantial_Completion_Date"="Substantial_Completion_Date")) %>%
  dplyr::group_by(high_level_ECM, detail_level_ECM) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  readr::write_csv(sprintf("%s/retrofit_count_detaillevel.csv", tabledir))

## write to latex
result_table_tex <- xtable::xtable(count.detail.ecm, caption="Building count of retrofits")

print(result_table_tex, tabular.environment = "tabular", include.rownames=FALSE,
      file=sprintf("%s/detail_retrofit_building_count.tex", tabledir),
      size="\\footnotesize")

sink("../tables/detail_retrofit_building_count.tex")
count.detail.ecm %>%
  dplyr::select(-high_level_ECM) %>%
  knitr::kable("latex", booktabs = T,
               format.args=list(big.mark=",", digits=2, scientific=F),
               caption="Number of buildings in each Retrofit Type") %>%
  kableExtra::kable_styling(full_width = FALSE, font_size = table.fontsize,
                            latex_options = "HOLD_position") %>%
  kableExtra::group_rows("Advanced Metering (168)", 1, 2, bold=FALSE) %>%
  kableExtra::group_rows("Building Envelope (120)", 3, 9, bold=FALSE) %>%
  kableExtra::group_rows("Building Tuneup or Utility Improvements", 10, 10, bold=FALSE) %>%
  kableExtra::group_rows("GSALink ", 11, 11, bold=FALSE) %>%
  kableExtra::group_rows("HVAC (174)", 12, 21, bold=FALSE) %>%
  kableExtra::group_rows("Lighting (168)", 22, 28, bold=FALSE) %>%
  print()
sink()

non.energy.features = retrofit.alldata %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(-variable, -mean.kbtu, -count, -retro.status, -start, -end) %>%
  dplyr::mutate(is.office = (`Building_Type`=="Office")) %>%
  dplyr::distinct() %>%
  {.}

energy.features = retrofit.alldata %>%
  dplyr::filter(scenario == "measured") %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit, variable, mean.kbtu, GROSSSQFT) %>%
  dplyr::mutate(kbtu.per.sqft = mean.kbtu/GROSSSQFT) %>%
  dplyr::select(-mean.kbtu) %>%
  tidyr::spread(variable, kbtu.per.sqft) %>%
  {.}

all.features = non.energy.features %>%
  dplyr::inner_join(energy.features, by=c("BLDGNUM", "Substantial_Completion_Date", "is.real.retrofit", "GROSSSQFT")) %>%
  {.}

options(tibble.width=Inf)

balance.check.vars = c("GROSSSQFT", "cldd", "htdd",
                       c("<10",
                         sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)),
                         ">90"), "CHILLWTR", "GAS", "KWHR", "STEAM")

result = lapply(balance.check.vars, function(v) {
  tresult <- t.test(as.formula(sprintf("`%s` ~ is.real.retrofit", v)), data = all.features, alternative = "two.sided")
  mu0 <- tresult$estimate[[1]]
  mu1 <- tresult$estimate[[2]]
  dif <- mu1 - mu0
  p <- tresult$p.value
  p.with.sig <- ifelse(p < 0.05, ifelse(p >= 0.01, sprintf("%.2f*", p), sprintf("%.2f**", p)), sprintf("%.2f", p))
  print(p)
  print(p.with.sig)
  var.label = ifelse(stringr::str_detect(v, "[0-9]"), paste0("$", v, "$"), v)
  return(list("variable" = var.label, "mu0" = mu0, "mu1" = mu1, "dif" = dif, "p" = p, "p.with.sig" = p.with.sig))
})

df.result = result %>%
  dplyr::bind_rows()

sink("../tables/numeric_feature_balance_compare.tex")
df.result %>%
  dplyr::select(-p) %>%
  knitr::kable("latex", booktabs = T, escape = F,
               format.args=list(big.mark=",", digits=2, scientific=F),
               caption="Covariate balance of numeric features in the retrofit study") %>%
  kableExtra::kable_styling(full_width = FALSE, font_size = table.fontsize,
                            latex_options = "HOLD_position") %>%
  print()
sink()

project.counts = non.energy.features %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()

building.counts = non.energy.features %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()

## fixme: add categorical features
df.cat = non.energy.features %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit, BLDGCAT) %>%
  dplyr::mutate(BLDGCAT= paste0("Category ", BLDGCAT)) %>%
  dplyr::mutate(value=T) %>%
  tidyr::spread(BLDGCAT, value, fill=F) %>%
  {.}

df.region = non.energy.features %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit, REGNNUM) %>%
  dplyr::mutate(REGNNUM = paste0("Region ", REGNNUM)) %>%
  dplyr::mutate(value=T) %>%
  tidyr::spread(REGNNUM, value, fill=F) %>%
  dplyr::select(BLDGNUM:`Region 1`, `Region 2`:`Region 9`, `Region 10`:`Region 11`) %>%
  {.}

df.prop = non.energy.features %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit, historic, with.leed, is.office) %>%
  dplyr::inner_join(df.cat, by=c("BLDGNUM", "Substantial_Completion_Date", "is.real.retrofit")) %>%
  dplyr::inner_join(df.region, by=c("BLDGNUM", "Substantial_Completion_Date", "is.real.retrofit")) %>%
  {.}

prop.vars = names(df.prop)[4:21]

result = lapply(prop.vars, function(v) {
  count.retrofit = df.prop %>%
    dplyr::filter(!!rlang::sym(v)) %>%
    dplyr::filter(is.real.retrofit) %>%
    nrow() %>%
    {.}
  count.non.retrofit = df.prop %>%
    dplyr::filter(!!rlang::sym(v)) %>%
    dplyr::filter(!is.real.retrofit) %>%
    nrow() %>%
    {.}
  zresult = prop.test(x=c(count.non.retrofit, count.retrofit),
                      n=project.counts$`n()`, p = NULL,
                      alternative = "two.sided", correct = F)
  ps = c(count.non.retrofit, count.retrofit) / building.counts$`n()`
  print(ps)
  p <- zresult$p.value
  p.with.sig <- ifelse(p < 0.05, ifelse(p >= 0.01, sprintf("%.2f*", p), sprintf("%.2f**", p)), sprintf("%.2f", p))
  return(list("variable"=v, "p0"=ps[[1]], "p1"=ps[[2]], "dif"=ps[[2]]-ps[[1]],
              "p.with.sig"=p.with.sig))
})

df.result = result %>%
  dplyr::bind_rows()

sink("../tables/indicator_feature_balance_compare.tex")
df.result %>%
  knitr::kable("latex", booktabs = T,
               format.args=list(big.mark=",", digits=2, scientific=F),
               caption="Covariate balance of categorical features in the retrofit study") %>%
  kableExtra::kable_styling(full_width = FALSE, font_size = table.fontsize,
                            latex_options = "HOLD_position") %>%
  print()
sink()

## balance after removing low propensity score ones
non.energy.features = retrofit.alldata.highprop %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(-variable, -mean.kbtu, -count, -retro.status, -start, -end) %>%
  dplyr::mutate(is.office = (`Building_Type`=="Office")) %>%
  dplyr::distinct() %>%
  {.}

energy.features = retrofit.alldata.highprop %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit, variable, mean.kbtu, GROSSSQFT) %>%
  dplyr::mutate(kbtu.per.sqft = mean.kbtu/GROSSSQFT) %>%
  dplyr::select(-mean.kbtu) %>%
  tidyr::spread(variable, kbtu.per.sqft) %>%
  {.}

all.features = non.energy.features %>%
  dplyr::inner_join(energy.features, by=c("BLDGNUM", "Substantial_Completion_Date", "is.real.retrofit", "GROSSSQFT")) %>%
  {.}

options(tibble.width=Inf)

balance.check.vars = c("GROSSSQFT", "cldd", "htdd",
                       c("<10",
                         sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)),
                         ">90"), "CHILLWTR", "GAS", "KWHR", "STEAM")

result = lapply(balance.check.vars, function(v) {
  tresult <- t.test(as.formula(sprintf("`%s` ~ is.real.retrofit", v)), data = all.features, alternative = "two.sided")
  mu0 <- tresult$estimate[[1]]
  mu1 <- tresult$estimate[[2]]
  dif <- mu1 - mu0
  p <- tresult$p.value
  p.with.sig <- ifelse(p < 0.05, ifelse(p >= 0.01, sprintf("%.2f*", p), sprintf("%.2f**", p)), sprintf("%.2f", p))
  print(p)
  print(p.with.sig)
  var.label = ifelse(stringr::str_detect(v, "[0-9]"), paste0("$", v, "$"), v)
  return(list("variable" = var.label, "mu0" = mu0, "mu1" = mu1, "dif" = dif, "p" = p, "p.with.sig" = p.with.sig))
})

df.result = result %>%
  dplyr::bind_rows()

sink("../tables/numeric_feature_balance_compare_highprop.tex")
df.result %>%
  dplyr::select(-p) %>%
  knitr::kable("latex", booktabs = T, escape = F,
               format.args=list(big.mark=",", digits=2, scientific=F),
               caption="Covariate balance of numeric features in the retrofit study") %>%
  kableExtra::kable_styling(full_width = FALSE, font_size = table.fontsize,
                            latex_options = "HOLD_position") %>%
  print()
sink()

project.counts = non.energy.features %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()

building.counts = non.energy.features %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()

df.cat = non.energy.features %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit, BLDGCAT) %>%
  dplyr::mutate(BLDGCAT= paste0("Category ", BLDGCAT)) %>%
  dplyr::mutate(value=T) %>%
  tidyr::spread(BLDGCAT, value, fill=F) %>%
  {.}

df.region = non.energy.features %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit, REGNNUM) %>%
  dplyr::mutate(REGNNUM = paste0("Region ", REGNNUM)) %>%
  dplyr::mutate(value=T) %>%
  tidyr::spread(REGNNUM, value, fill=F) %>%
  dplyr::select(BLDGNUM:`Region 1`, `Region 2`:`Region 9`, `Region 10`:`Region 11`) %>%
  {.}

df.prop = non.energy.features %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit, historic, with.leed, is.office) %>%
  dplyr::inner_join(df.cat, by=c("BLDGNUM", "Substantial_Completion_Date", "is.real.retrofit")) %>%
  dplyr::inner_join(df.region, by=c("BLDGNUM", "Substantial_Completion_Date", "is.real.retrofit")) %>%
  {.}

prop.vars = names(df.prop)[4:21]

result = lapply(prop.vars, function(v) {
  count.retrofit = df.prop %>%
    dplyr::filter(!!rlang::sym(v)) %>%
    dplyr::filter(is.real.retrofit) %>%
    nrow() %>%
    {.}
  count.non.retrofit = df.prop %>%
    dplyr::filter(!!rlang::sym(v)) %>%
    dplyr::filter(!is.real.retrofit) %>%
    nrow() %>%
    {.}
  zresult = prop.test(x=c(count.non.retrofit, count.retrofit),
                      n=project.counts$`n()`, p = NULL,
                      alternative = "two.sided", correct = F)
  ps = c(count.non.retrofit, count.retrofit) / building.counts$`n()`
  print(ps)
  p <- zresult$p.value
  p.with.sig <- ifelse(p < 0.05, ifelse(p >= 0.01, sprintf("%.2f*", p), sprintf("%.2f**", p)), sprintf("%.2f", p))
  return(list("variable"=v, "p0"=ps[[1]], "p1"=ps[[2]], "dif"=ps[[2]]-ps[[1]],
              "p.with.sig"=p.with.sig))
})

df.result = result %>%
  dplyr::bind_rows()

sink("../tables/indicator_feature_balance_compare_highprop.tex")
df.result %>%
  knitr::kable("latex", booktabs = T,
               format.args=list(big.mark=",", digits=2, scientific=F),
               caption="Covariate balance of categorical features in the retrofit study") %>%
  kableExtra::kable_styling(full_width = FALSE, font_size = table.fontsize,
                            latex_options = "HOLD_position") %>%
  print()
sink()

load("../data/non.action.data.lean.rda")

lean.summary = non.action.data.lean %>%
  dplyr::distinct(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit,
                  baseload_GAS, baseload_KWHR, htcl_GAS, htcl_KWHR) %>%
  tidyr::gather(variable, value, baseload_GAS:htcl_KWHR) %>%
  dplyr::mutate(variable = variable * 12) %>%
  dplyr::group_by(is.real.retrofit, variable) %>%
  dplyr::summarise_at(vars(value), tibble::lst(mean, median, sd, min, max)) %>%
  dplyr::ungroup() %>%
  {.}

## by elec and gas, annual
energy.summary = retrofit.alldata %>%
  ## change to annual summary
  dplyr::mutate(kbtu.per.sqft = mean.kbtu/GROSSSQFT * 12) %>%
  dplyr::filter(variable %in% c("GAS", "KWHR"), scenario == "measured") %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::group_by(is.real.retrofit, variable) %>%
  dplyr::summarise_at(vars(kbtu.per.sqft), tibble::lst(mean, median, sd, min, max)) %>%
  dplyr::ungroup() %>%
  {.}

nonenergy.summary = retrofit.alldata %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::filter(scenario == "measured") %>%
  dplyr::distinct(BLDGNUM, `Substantial_Completion_Date`,
                  is.real.retrofit, HDD, CDD, GROSSSQFT) %>%
  tidyr::gather(variable, value, HDD:GROSSSQFT) %>%
  dplyr::group_by(is.real.retrofit, variable) %>%
  dplyr::summarise_at(vars(value), tibble::lst(mean, median, sd, min, max)) %>%
  dplyr::ungroup() %>%
  {.}

energy.summary %>%
  dplyr::bind_rows(nonenergy.summary) %>%
  dplyr::bind_rows(lean.summary) %>%
  dplyr::mutate(var.rank = case_when(variable == "KWHR" ~ 1,
                                     variable == "GAS" ~ 2,
                                     variable == "baseload_KWHR" ~ 3,
                                     variable == "baseload_GAS" ~ 4,
                                     variable == "htcl_KWHR" ~ 5,
                                     variable == "htcl_GAS" ~ 6,
                                     variable == "CDD" ~ 7,
                                     variable == "HDD" ~ 8,
                                     variable == "GROSSSQFT" ~ 9,
                                     TRUE ~ 0)) %>%
  dplyr::mutate_at(vars(variable), recode,
                   "GROSSSQFT"="Gross Square Footage",
                   "GAS" = "Gas (kBtu/sqft/year)",
                   "KWHR" = "Electricity (kBtu/sqft/year)",
                   "baseload_GAS" = "Baseload Gas (kBtu/sqft/year)",
                   "baseload_KWHR" = "Baseload Electricity (kBtu/sqft/year)",
                   "htcl_GAS" = "Heating/Cooling Gas (kBtu/sqft/year)",
                   "htcl_KWHR" = "Heating/Cooling Electricity (kBtu/sqft/year)") %>%
  dplyr::arrange(desc(is.real.retrofit), var.rank) %>%
  dplyr::select(is.real.retrofit, var.rank, variable, min, mean, median, max, sd) %>%
  readr::write_csv("numeric_summary.csv")

load("../data/retrofit_prev_actions_toplevel.rda")
load("../data/retrofit_prev_actions_highlevel.rda")
load("../data/retrofit_prev_actions_joint_highlevel.rda")
load("../data/retrofit_prev_actions_detaillevel.rda")

retro.buildings = retrofit.alldata %>%
  dplyr::filter(is.real.retrofit) %>%
  dplyr::distinct(BLDGNUM, `Substantial_Completion_Date`)

toplevel.count = retrofit_prev_actions_toplevel %>%
  distinct(`Building_Number`, `target.action`, `target.date`) %>%
  dplyr::inner_join(retro.buildings, by=c("Building_Number"="BLDGNUM",
                                          "target.date"="Substantial_Completion_Date")) %>%
  dplyr::group_by(target.action) %>%
  dplyr::summarise(count=n()) %>%
  dplyr::ungroup() %>%
  {.}

highlevel.count = retrofit_prev_actions_highlevel %>%
  distinct(`Building_Number`, `target.action`, `target.date`) %>%
  dplyr::inner_join(retro.buildings, by=c("Building_Number"="BLDGNUM",
                                          "target.date"="Substantial_Completion_Date")) %>%
  dplyr::group_by(target.action) %>%
  dplyr::summarise(count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(highlevel = paste0(target.action, "(n=", count, ")")) %>%
  dplyr::select(-count) %>%
  dplyr::mutate(toplevel = ifelse(target.action %in% c("Building Envelope", "HVAC", "Lighting"), "Capital", "Operational")) %>%
  dplyr::mutate(toplevel = ifelse(toplevel == "Capital",
                                   paste0(toplevel, "(n=",
                                          toplevel.count[[1,2]] + toplevel.count[[2,2]], ")"),
                                  paste0(toplevel, "(n=",
                                          toplevel.count[[3,2]] + toplevel.count[[2,2]], ")"))) %>%
  {.}

df.count = retrofit_prev_actions_detaillevel %>%
  distinct(`Building_Number`, `target.action`, `target.date`) %>%
  dplyr::inner_join(retro.buildings, by=c("Building_Number"="BLDGNUM",
                                          "target.date"="Substantial_Completion_Date")) %>%
  dplyr::group_by(target.action) %>%
  dplyr::summarise(count=n()) %>%
  dplyr::ungroup() %>%
  tidyr::separate(target.action, into=c("l1", "l2", "l3"), sep="_") %>%
  tidyr::unite("action", l2:l3) %>%
  dplyr::mutate(action = gsub("_NA", "", action)) %>%
  dplyr::mutate(action = gsub("_", " ", action)) %>%
  dplyr::inner_join(highlevel.count, by=c("l1"="target.action")) %>%
  dplyr::select(-l1) %>%
  dplyr::select(toplevel, highlevel, action, count) %>%
  dplyr::mutate(highlevel = gsub("Building Tuneup or Utility Improvement", "Commissioning", highlevel)) %>%
  dplyr::arrange(toplevel, highlevel, action) %>%
  dplyr::rename(`broad category`=`toplevel`,
                `category`=`highlevel`,
                `sub category`=action) %>%
  {.}

df.count %>%
  readr::write_csv(sprintf("%s/retrofit_count_all.csv", tabledir))

sink(sprintf("%s/retrofit_count_all.tex", tabledir))
df.count %>%
  knitr::kable("latex", booktabs = T,
               caption="Building Count by Action Category") %>%
  kableExtra::kable_styling("striped", full_width = FALSE,
                            font_size = table.fontsize,
                            latex_options = "HOLD_position") %>%
  kableExtra::collapse_rows(columns=1:3, latex_hline="custom",
                            custom_latex_hline = c(1,2)) %>%
  print()
sink()
