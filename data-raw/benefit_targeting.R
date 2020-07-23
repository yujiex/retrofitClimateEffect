library("ggplot2")
library("dplyr")

## helpers
compile.targeting.df <- function(df.no, df.yes, pre.energy, building.count) {
  to.plot = df.no %>%
    dplyr::bind_rows(df.yes) %>%
    dplyr::mutate(mmbtu = predictions.kbtu / 1000) %>%
    ## compute percent saving in pre-retrofit energy
    dplyr::left_join(pre.energy, by="fuel") %>%
    dplyr::left_join(building.count, by=c("fuel", "period", "scenario")) %>%
    dplyr::mutate(percent.save = predictions.kbtu/pre.kbtu * 100) %>%
    dplyr::mutate_at(vars(fuel), dplyr::recode,
                    "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                    "KWHR"="Electricity", "STEAM"="Steam") %>%
    dplyr::mutate_at(vars(period), dplyr::recode,
                    "2050Jan through 2059Jan"="mid-century",
                    "2090Jan through 2099Jan"="late-century") %>%
    dplyr::mutate(fuel.label = paste0(fuel, "\nn=", n)) %>%
    dplyr::mutate(fuel.label = ordered(fuel.label, levels=unique(.$fuel.label))) %>%
    dplyr::mutate(saving.suf = ifelse(predictions.kbtu < 0, "reduction", "increase")) %>%
    dplyr::mutate(targeting.label = paste0(label, "\n",
                                          abs(round(percent.save, 1)),
                                          "% ", saving.suf)) %>%
    ## make savings positive
    dplyr::mutate(mmbtu=(-1) * mmbtu) %>%
    {.}
  return(to.plot)
}

## compute the benefit of targeting

load("../data/building_location.rda")
load("../data/retrofit_from_db.rda")

imagedir = "~/Dropbox/thesis/code/retrofitClimateEffect/images"
tabledir = "~/Dropbox/thesis/code/retrofitClimateEffect/tables"

suf = "_measured_input"

## kw = "highlevel_bp"
## kw = "detaillevel_bp"
kw = "toplevel_bp"
## kw = "joint_highlevel_bp"
kw = paste0(kw, suf)

cf.result = readr::read_csv(sprintf("%s/grf_result_%s.csv", tabledir, kw))

pre.energy = cf.result %>%
  dplyr::filter(is.real.retrofit==0, scenario == "measured") %>%
  dplyr::distinct(BLDGNUM, Substantial_Completion_Date, KWHR, GAS, GROSSSQFT) %>%
  tidyr::gather(fuel, value, KWHR:GAS) %>%
  dplyr::mutate(kbtu = value * GROSSSQFT) %>%
  dplyr::group_by(fuel) %>%
  dplyr::summarise(pre.kbtu = sum(kbtu)) %>%
  dplyr::ungroup() %>%
  {.}

building.action.saving = cf.result %>%
  dplyr::filter(is.real.retrofit==0) %>%
  dplyr::select(BLDGNUM, predictions, action, fuel, GROSSSQFT, scenario, period) %>%
  dplyr::mutate(predictions.kbtu = predictions * GROSSSQFT) %>%
  dplyr::group_by(BLDGNUM, action, fuel, scenario, period) %>%
  dplyr::summarise(predictions.kbtu = mean(predictions.kbtu),
                   predictions = mean(predictions)) %>%
  dplyr::ungroup() %>%
  {.}

building.count = building.action.saving %>%
  dplyr::distinct(BLDGNUM, fuel, period, scenario) %>%
  dplyr::group_by(fuel, period, scenario) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  {.}

all.no.targeting = building.action.saving %>%
  dplyr::group_by(fuel, period, scenario) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "No targeting") %>%
  {.}

all.with.targeting = building.action.saving %>%
  ## set savings to 0 for the buildings increasing consumption,
  ## assuming taking no actions means 0 savings
  dplyr::mutate(predictions.kbtu = ifelse(predictions.kbtu < 0,
                                          predictions.kbtu, 0)) %>%
  dplyr::group_by(fuel, period, scenario) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "With targeting") %>%
  {.}

## print benefit and percent benefit of targeting vs no-targeting in mmbtu
## (million btu)
all.no.targeting %>%
  dplyr::bind_rows(all.with.targeting) %>%
  dplyr::mutate(mmbtu = predictions.kbtu / 1000) %>%
  dplyr::select(period, scenario, fuel, label, mmbtu) %>%
  tidyr::spread(label, mmbtu) %>%
  dplyr::mutate(percent = `With targeting` / `No targeting` * 100) %>%
  {.}

## saving of targeting and no targeting as percentage of pre-retrofit energy
all.no.targeting %>%
  dplyr::bind_rows(all.with.targeting) %>%
  dplyr::left_join(pre.energy, by="fuel") %>%
  dplyr::mutate(percent.save = predictions.kbtu/pre.kbtu * 100) %>%
  dplyr::arrange(period, scenario, fuel, label) %>%
  {.}

## only high level action and detail level action has comparison of targeting vs
## retrofit all buildings with all action, as the other two has overlaps in each
## action choice
if (stringr::str_detect(kw, "highlevel") | stringr::str_detect(kw, "detail")) {
  to.plot = compile.targeting.df(all.no.targeting, all.with.targeting,
                                 pre.energy, building.count)
  if (stringr::str_detect(kw, "highlevel")) {
    mult = 1.4
  } else {
    mult = 1.5
  }
  ## for all action
  dfs.to.plot = to.plot %>%
    dplyr::group_by(period, scenario) %>%
    dplyr::group_split()
  lapply(dfs.to.plot, function(df.to.plot) {
    target.period = df.to.plot$period[[1]]
    target.scenario = df.to.plot$scenario[[1]]
    scenario.str = ifelse(target.scenario == "measured", "", target.scenario)
    lim = max(df.to.plot$mmbtu) * mult
    df.to.plot %>%
      dplyr::filter(scenario == target.scenario, period == target.period) %>%
      ggplot2::ggplot(ggplot2::aes(x=fuel.label, y=mmbtu, fill=interaction(label, fuel))) +
      ggplot2::geom_bar(stat="identity", position="dodge") +
      ggplot2::ggtitle(sprintf("Targeting only the reducers \nvs all buildings taking all retrofits\n%s %s", target.period, scenario.str)) +
      ggplot2::scale_fill_manual(values=RColorBrewer::brewer.pal(8, "Paired")[1:4], name="") +
      ggplot2::geom_text(ggplot2::aes(label=targeting.label), hjust=1.05, position = position_dodge(0.9)) +
      ggplot2::expand_limits(y=lim) +
      ggplot2::ylab("MMBtu") +
      ggplot2::scale_y_reverse() +
      ggplot2::coord_flip() +
      ggplot2::theme(legend.position="none", axis.title.y=element_blank())
    ggplot2::ggsave(sprintf("%s/targeting_all_slides_%s_%s_%s.png", imagedir, target.period, target.scenario, kw), width=5, height=4)
  })
}

## by action now
by.action.no.targeting = building.action.saving %>%
  dplyr::group_by(action, fuel, scenario, period) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "No targeting") %>%
  {.}

by.action.with.targeting = building.action.saving %>%
  ## set savings to 0 for the buildings increasing consumption,
  ## assuming taking no actions means 0 savings
  dplyr::mutate(predictions.kbtu = ifelse(predictions.kbtu < 0,
                                          predictions.kbtu, 0)) %>%
  dplyr::group_by(action, fuel, scenario, period) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "With targeting") %>%
  {.}

to.plot = compile.targeting.df(by.action.no.targeting, by.action.with.targeting,
                               pre.energy, building.count)

if (!stringr::str_detect(kw, "detail")) {
  if (stringr::str_detect(kw, "toplevel") | stringr::str_detect(kw, "joint")) {
    num.of.col = 3
    image.width = 9
    image.height = 3
    mult = 1.9
  } else {
    num.of.col = 2
    image.width = 7
    image.height = 6
    mult = 1.5
  }
  if (stringr::str_detect(kw, "joint_highlevel")) {
    to.plot <- to.plot %>%
      dplyr::mutate(action = gsub(" and ", " & ", action)) %>%
      dplyr::mutate(action = gsub("Advanced Metering", "A", action)) %>%
      dplyr::mutate(action = gsub("Building Envelope", "B", action)) %>%
      dplyr::mutate(action = gsub("Building Tuneup or Utility Improvements", "C",
                                  action)) %>%
      dplyr::mutate(action = gsub("HVAC", "H", action)) %>%
      dplyr::mutate(action = gsub("Lighting", "L", action)) %>%
      dplyr::filter(action %in% c("A & B & C & H & L", "A & C & H & L",
                                  "C & H & L")) %>%
      {.}
  }
  print(mult)
  print(num.of.col)
  dfs.to.plot = to.plot %>%
    dplyr::group_by(period, scenario) %>%
    dplyr::group_split()
  lapply(dfs.to.plot, function(df.to.plot) {
    target.period = df.to.plot$period[[1]]
    target.scenario = df.to.plot$scenario[[1]]
    scenario.str = ifelse(target.scenario == "measured", "", target.scenario)
    lim = max(df.to.plot$mmbtu) * mult
    df.to.plot %>%
      dplyr::filter(period == target.period, scenario == target.scenario) %>%
      ggplot2::ggplot(ggplot2::aes(x=fuel.label, y=mmbtu,
                                   fill=interaction(label, fuel.label),
                                   label=count)) +
      ggplot2::geom_bar(stat="identity", position="dodge") +
      ggplot2::ggtitle(sprintf("Targeting only the reducers vs all buildings taking an action\n%s %s", target.period, scenario.str)) +
      ggplot2::scale_fill_manual(values=RColorBrewer::brewer.pal(8, "Paired")[1:4], name="") +
      ggplot2::facet_wrap(.~action, ncol=num.of.col) +
      ggplot2::coord_flip() +
      ggplot2::geom_text(ggplot2::aes(label=targeting.label), hjust=1.05,
                         position = position_dodge(0.9), lineheight = 0.7, size=3) +
      ## ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9), size=3) +
      ggplot2::expand_limits(y=lim) +
      ggplot2::scale_y_reverse() +
      ggplot2::ylab("MMBtu") +
      ggplot2::theme(legend.position="none", title=element_text(size=9), axis.title.y=element_blank())
    ggplot2::ggsave(sprintf("%s/targeting_by_action_slides_%s_%s_%s.png", imagedir, target.period, target.scenario, kw), width=image.width, height=image.height)
  })
} else {
  ## each l1 action in a plot
  ## leave only large-n actions
  ## filter actions with large enough n
  ## large.n = cf.result %>%
  ##   dplyr::filter(period == "now") %>%
  ##   dplyr::filter(is.real.retrofit==1) %>%
  ##   dplyr::distinct(action, fuel, BLDGNUM, Substantial_Completion_Date) %>%
  ##   dplyr::group_by(action, fuel) %>%
  ##   dplyr::count() %>%
  ##   dplyr::ungroup() %>%
  ##   dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
  ##                    "GAS"="Natural Gas", "KWHR"="Electricity") %>%
  ##   dplyr::filter(n>20) %>%
  ##   dplyr::select(action, fuel) %>%
  ##   {.}
  split.by.l1.action = to.plot %>%
    dplyr::mutate(fuel = as.character(fuel)) %>%
    ## dplyr::inner_join(large.n, by=c("action", "fuel")) %>%
    tidyr::separate(action, into=c("l1", "l2", "l3"), sep="_") %>%
    dplyr::filter(l2 != "NA") %>%
    dplyr::arrange(l1, l3, l2) %>%
    dplyr::mutate(action = ifelse(is.na(l3), l2, paste(l2, " ", l3))) %>%
    dplyr::mutate_at(dplyr::vars(l1), dplyr::recode,
                     "Building Tuneup or Utility Improvements"="Commissioning") %>%
    dplyr::mutate(l1 = ifelse(l1 %in% c("Advanced Metering", "Commissioning", "GSALink"), "Operational", l1)) %>%
    dplyr::group_by(period, scenario, l1) %>%
    dplyr::group_split() %>%
    {.}
  lapply(split.by.l1.action, function(df.action) {
    l1.action = df.action$l1[[1]]
    target.period = df.action$period[[1]]
    target.scenario = df.action$scenario[[1]]
    scenario.str = ifelse(target.scenario == "measured", "", target.scenario)
    if (l1.action == "Operational") {
      df.action <- df.action %>%
        dplyr::mutate_at(vars(action), recode, "Building / Facility"="Advanced Metering (Building / Facility)", "Submetering"="Advanced Metering (Submetering)")
    }
    lim = max(df.action$mmbtu) * 1.7
    n.action = length(unique(df.action$action))
    image.height = round(n.action * 0.9, 1)
    p <- df.action %>%
      ggplot2::ggplot(ggplot2::aes(x=fuel, y=mmbtu, fill=interaction(label, fuel), label=count)) +
      ggplot2::geom_bar(stat="identity", position="dodge") +
      ggplot2::ggtitle(sprintf("Targeting only the reducers vs all buildings taking an action\n%s %s", target.period, scenario.str)) +
      ggplot2::scale_fill_manual(values=RColorBrewer::brewer.pal(8, "Paired")[1:4], name="")
    if (l1.action %in% c("HVAC", "Building Envelope")) {
      p <- p +
        ggplot2::facet_grid(l3~l2)
    } else {
      p <- p +
        ggplot2::facet_wrap(.~action, ncol=2)
    }
    p +
      ggplot2::coord_flip() +
      ## ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9), size=3) +
      ggplot2::geom_text(ggplot2::aes(label=targeting.label), hjust=1.05,
                         position = position_dodge(0.9), lineheight = 0.7, size=3) +
      ggplot2::expand_limits(y=lim) +
      ggplot2::scale_y_reverse() +
      ggplot2::ylab("MMBtu") +
      ggplot2::theme(legend.position="none", title=element_text(size=9), axis.title.y=element_blank())
    ggplot2::ggsave(sprintf("%s/targeting_by_action_slides_%s_%s_%s_%s.png", imagedir, gsub(" ", "-", l1.action), target.period, target.scenario, kw),
                    width=7, height=image.height)
  })
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ROI targeting (positive ROI)
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

state.abb.lookup = tibble::tibble(name=c(state.name, "District of Columbia"), abbr=c(state.abb, "DC")) %>%
  {.}

electric.rate = readxl::read_excel("EIA march2020/Table_5_06_A.xlsx", skip=3) %>%
  dplyr::select(`Census Division\r\nand State`, `January 2019...5`) %>%
  dplyr::rename(State = `Census Division\r\nand State`) %>%
  dplyr::filter(!(State %in% c("New England", "Middle Atlantic",
                               "East North Central", "West North Central",
                               "South Atlantic", "East South Central",
                               "West South Central", "Mountain",
                               "Pacific Contiguous", "Pacific Noncontiguous",
                               "U.S. Total"))) %>%
  dplyr::left_join(state.abb.lookup, by=c("State"="name")) %>%
  dplyr::mutate(dollar.kbtu = `January 2019...5` / 3.412) %>%
  dplyr::select(abbr, dollar.kbtu) %>%
  dplyr::mutate(fuel="Electricity") %>%
  {.}

gas.rate = readr::read_csv("gas_price_by_state.csv") %>%
  dplyr::filter(State != "U.S.") %>%
  dplyr::left_join(state.abb.lookup, by=c("State"="name")) %>%
  dplyr::select(abbr, `2018`) %>%
  ## original unit kcf
  dplyr::mutate(`dollar.kbtu`=`2018` / 1026) %>%
  dplyr::select(-`2018`) %>%
  dplyr::mutate(fuel="Gas") %>%
  {.}

util.price.change.future = readr::read_csv("utility_price/Table_1._Total_Energy_Supply_Disposition_and_Price_Summary_subset.csv") %>%
  dplyr::mutate(variable = c("Gas", "Electricity")) %>%
  dplyr::select(-`full name`, -`api key`, -`Growth (2019-2050)`, -X1) %>%
  dplyr::select(variable, `2019`, `2050`) %>%
  dplyr::mutate(scaler = `2050` / `2019`) %>%
  dplyr::select(variable, scaler) %>%
  {.}

util.rate = electric.rate %>%
  dplyr::bind_rows(gas.rate) %>%
  dplyr::mutate(period = "now") %>%
  {.}

util.rate <- util.rate %>%
  dplyr::inner_join(util.price.change.future, by=c("fuel"="variable")) %>%
  dplyr::mutate(dollar.kbtu = dollar.kbtu * scaler) %>%
  dplyr::select(-scaler) %>%
  dplyr::mutate(period = "mid-century") %>%
  dplyr::bind_rows(util.rate) %>%
  {.}

building.state = building_location %>%
  dplyr::distinct(BLDGNUM, State) %>%
  {.}

## according to https://portfoliomanager.zendesk.com/hc/en-us/articles/216670148-What-are-the-Site-to-Source-Conversion-Factors-
mult.to.source = tibble::tibble(fuel = c("Electricity", "Gas", "STEAM", "CHILLWTR"),
                                mult=c(2.8, 1.05, 1.2, 0.91))
## according to https://www.epa.gov/energy/greenhouse-gases-equivalencies-calculator-calculations-and-references
mult.to.co2 = tibble::tibble(fuel = c("Electricity", "Gas"),
                             mult=c(1/3.412*7.07e-4, 1/1.031/1000*0.0549))

static.info = cf.result %>%
  distinct(BLDGNUM, historic, GROSSSQFT) %>%
  {.}

to.plot.roi = building.action.saving %>%
  dplyr::select(BLDGNUM, predictions, action, fuel, scenario, period) %>%
  dplyr::mutate_at(vars(period), dplyr::recode,
                   "2050Jan through 2059Jan"="mid-century",
                   "2090Jan through 2099Jan"="late-century") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::inner_join(building.state, by="BLDGNUM") %>%
  ## not analyze for now, as the pre-retrofit period might be different from the
  ## other ones, no need to block out two years
  dplyr::inner_join(util.rate, by=c("State"="abbr", "fuel"="fuel", "period"="period")) %>%
  dplyr::mutate(predictions.dollar = predictions * dollar.kbtu) %>%
  dplyr::left_join(mult.to.source, by="fuel") %>%
  dplyr::mutate(predictions.source = predictions * mult) %>%
  dplyr::select(-mult) %>%
  ## get tons of co2
  dplyr::left_join(mult.to.co2, by="fuel") %>%
  dplyr::mutate(predictions.co2 = predictions * mult) %>%
  dplyr::select(-mult) %>%
  ## dplyr::mutate(predictions.scc = predictions.co2 * 38) %>%
  dplyr::group_by(BLDGNUM, action, scenario, period) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(static.info, by="BLDGNUM") %>%
  dplyr::select(-dollar.kbtu) %>%
  {.}

## implementation cost
if (stringr::str_detect(kw, "toplevel")) {
  action.cost.per.sqft = readr::read_csv("toplevel_action_cost.csv") %>%
    {.}
} else if (stringr::str_detect(kw, "highlevel")) {
  action.cost.per.sqft = readr::read_csv("highlevel_action_cost.csv") %>%
    {.}
}
action.cost.per.sqft <- action.cost.per.sqft %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::rename(imp.cost.per.sqft = Estimate) %>%
  {.}

## action lifespan
action.lifespan = readr::read_csv("retrofit_lifespan_tidy.csv") %>%
  dplyr::select(-Source) %>%
  {.}

action.lifespan <- action.lifespan %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(`Lifespan (years)` = mean(`Lifespan (years)`)) %>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(action.lifespan) %>%
  dplyr::distinct() %>%
  {.}

building.action.lifespan = retrofit_from_db %>%
  dplyr::distinct(`Building_Number`, `Substantial_Completion_Date`, `high_level_ECM`, `detail_level_ECM`) %>%
  tidyr::separate(detail_level_ECM, c("pref", "suf"), "_") %>%
  dplyr::mutate_at(dplyr::vars(high_level_ECM), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(vars(suf), recode, "Indoor Daylighting or Lighting Strategies"="Daylighting",
                   "Retrofit or Replacement"="LED fixture",
                   "Commissioning Measures"="Commissioning") %>%
  dplyr::left_join(action.lifespan, by=c("high_level_ECM"="Category", "suf"="Sub-category")) %>%
  {.}

## highlevel
retroed.building.action.lifespan.highlevel = building.action.lifespan %>%
  dplyr::group_by(`Building_Number`, `Substantial_Completion_Date`, `high_level_ECM`) %>%
  dplyr::summarise(lifespan.year = mean(`Lifespan (years)`)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-`Substantial_Completion_Date`) %>%
  {.}
high.level.ecm.lifespan = retroed.building.action.lifespan.highlevel %>%
  dplyr::group_by(high_level_ECM) %>%
  dplyr::summarise(lifespan.year = mean(lifespan.year)) %>%
  {.}

## toplevel
retroed.building.action.lifespan.toplevel = building.action.lifespan %>%
  dplyr::group_by(`Building_Number`, `Substantial_Completion_Date`) %>%
  dplyr::summarise(lifespan.year = mean(`Lifespan (years)`)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-`Substantial_Completion_Date`) %>%
  {.}
top.level.ecm.lifespan = retrofit_from_db %>%
  dplyr::distinct(Building_Number, Substantial_Completion_Date, high_level_ECM) %>%
  dplyr::mutate(top_level_ECM = ifelse(high_level_ECM %in% c("Advanced Metering", "GSALink", "Building Tuneup or Utility Improvements"), "Operational", "Capital")) %>%
  dplyr::distinct(`Building_Number`, Substantial_Completion_Date, top_level_ECM) %>%
  dplyr::arrange(`Building_Number`, Substantial_Completion_Date, top_level_ECM) %>%
  dplyr::group_by(`Building_Number`, Substantial_Completion_Date) %>%
  dplyr::summarise(top_level_ECM = paste(top_level_ECM, collapse = " and ")) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(retroed.building.action.lifespan.toplevel, by="Building_Number") %>%
  dplyr::group_by(top_level_ECM) %>%
  dplyr::summarise(lifespan.year = mean(lifespan.year)) %>%
  dplyr::ungroup() %>%
  {.}

building.action.roi = to.plot.roi %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  tidyr::gather(variable, value.per.sqft, starts_with("predictions")) %>%
  dplyr::filter(variable == "predictions") %>%
  dplyr::mutate(value = value.per.sqft * GROSSSQFT) %>%
  dplyr::left_join(action.cost.per.sqft, by=c("action", "historic")) %>%
  dplyr::mutate(implement.cost = imp.cost.per.sqft * GROSSSQFT) %>%
  dplyr::select(-GROSSSQFT, -imp.cost.per.sqft) %>%
  {.}

if (stringr::str_detect(kw, "toplevel")) {
  building.action.roi <- building.action.roi %>%
    dplyr::left_join(top.level.ecm.lifespan,
                     by=c("action"="top_level_ECM")) %>%
    {.}
} else if (stringr::str_detect(kw, "highlevel")) {
  building.action.roi <- building.action.roi %>%
    dplyr::left_join(high.level.ecm.lifespan,
                     by=c("action"="high_level_ECM")) %>%
    {.}
}

building.action.roi <- building.action.roi %>%
  dplyr::mutate(implement.cost = ifelse(action == "GSALink", (48000 + 155000) / 2, implement.cost)) %>%
  dplyr::mutate(annual.value = value * 12) %>%
  dplyr::mutate(ROI = ((-1) * annual.value * lifespan.year - implement.cost) / implement.cost) %>%
  dplyr::select(BLDGNUM, action, scenario, period, implement.cost, lifespan.year, annual.value, ROI) %>%
  {.}

all.no.targeting.roi = building.action.roi %>%
  dplyr::mutate(label = "No targeting") %>%
  {.}

## only implement those with savings > implementation cost
all.with.targeting.roi = building.action.roi %>%
  dplyr::mutate(lose.money = (-1) * annual.value * lifespan.year < implement.cost) %>%
  dplyr::mutate(annual.value = ifelse(lose.money, 0, annual.value),
                implement.cost = ifelse(lose.money, 0, implement.cost)) %>%
  dplyr::mutate(label = "With targeting") %>%
  {.}

## plot all action
to.plot <- all.no.targeting.roi %>%
  dplyr::bind_rows(all.with.targeting.roi) %>%
  dplyr::group_by(scenario, period, label) %>%
  dplyr::summarise(ROI = sum((-1) * annual.value * lifespan.year - implement.cost) / sum(implement.cost)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(variable="ROI") %>%
  dplyr::mutate(target.label = paste0(label,
                                      "\n", "ROI = ",
                                      round(ROI, 1))) %>%
  {.}

dfs.to.plot = to.plot %>%
  dplyr::group_by(period, scenario) %>%
  dplyr::group_split()
lapply(dfs.to.plot, function(df.to.plot) {
  target.period = df.to.plot$period[[1]]
  target.scenario = df.to.plot$scenario[[1]]
  scenario.str = ifelse(target.scenario == "measured", "", target.scenario)
  lim = max(df.to.plot$ROI) * 1.4
  df.to.plot %>%
    dplyr::filter(scenario == target.scenario, period == target.period) %>%
    ggplot2::ggplot(ggplot2::aes(x=variable, y=ROI, fill=label,
                                 group=interaction(label, period, scenario))) +
    ggplot2::geom_bar(stat="identity", position="dodge") +
    ggplot2::ggtitle(sprintf("Targeting only the reducers \nvs all buildings taking all retrofits\n%s %s", target.period, scenario.str)) +
    ggplot2::scale_fill_manual(values=RColorBrewer::brewer.pal(8, "Paired")[5:6], name="") +
    ggplot2::geom_text(ggplot2::aes(label=target.label), hjust=1.05,
                       position = position_dodge(0.9)) +
    ggplot2::expand_limits(y=lim) +
    ggplot2::ylab("ROI") +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_flip() +
    ggplot2::theme(legend.position="none", axis.title.y=element_blank())
  ggplot2::ggsave(sprintf("%s/targeting_all_roi_%s_%s_%s.png", imagedir, target.period, target.scenario, kw), width=5, height=3)
})

## plot by action
to.plot.by.action <- all.no.targeting.roi %>%
  dplyr::bind_rows(all.with.targeting.roi) %>%
  dplyr::group_by(scenario, period, label, action, label) %>%
  dplyr::summarise(ROI = sum((-1) * annual.value * lifespan.year - implement.cost) / sum(implement.cost)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(variable="ROI") %>%
  {.}

num.of.col = 3
if (stringr::str_detect(kw, "toplevel")) {
  image.width = 7
  image.height = 3
  mult = 2
} else if (stringr::str_detect(kw, "highlevel")){
  image.width = 9
  image.height = 4
  mult = 1.9
}
to.plot.by.action <- to.plot.by.action %>%
  dplyr::mutate(target.label = paste0(label,
                                      "\n", "ROI = ",
                                      round(ROI, 1))) %>%
  {.}

dfs.to.plot = to.plot.by.action %>%
  dplyr::group_by(period, scenario) %>%
  dplyr::group_split()
lapply(dfs.to.plot, function(df.to.plot) {
  target.period = df.to.plot$period[[1]]
  target.scenario = df.to.plot$scenario[[1]]
  scenario.str = ifelse(target.scenario == "measured", "", target.scenario)
  lim = max(df.to.plot$ROI) * mult
  p <- df.to.plot %>%
    dplyr::filter(scenario == target.scenario, period == target.period) %>%
    ggplot2::ggplot(ggplot2::aes(x=variable, y=ROI, fill=label,
                                 group=interaction(action, label, period,
                                                   scenario))) +
    ggplot2::geom_bar(stat="identity", position="dodge") +
    ggplot2::ggtitle(sprintf("Targeting only the reducers \nvs all buildings taking one retrofits\n%s %s", target.period, scenario.str)) +
    ggplot2::scale_fill_manual(values=RColorBrewer::brewer.pal(8, "Paired")[5:6], name="") +
    ggplot2::geom_text(ggplot2::aes(label=target.label), hjust=1.05,
                       position = position_dodge(0.9), lineheight = 1.0) +
    ggplot2::facet_wrap(.~action, ncol=num.of.col) +
    ggplot2::expand_limits(y=lim) +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_flip() +
    ggplot2::theme(legend.position="none", axis.title.y=element_blank(),
                   axis.text.y=element_blank())
  ggplot2::ggsave(sprintf("%s/targeting_by_action_roi_%s_%s_%s.png", imagedir, target.period, target.scenario, kw), width=image.width, height=image.height)
})

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## generate file for the results benefit from targeting, noaa input
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## under current climate noaa input
cf.fewcol.result = readr::read_csv(sprintf("%s/grf_result_fewcol_detaillevel_bp_measured_input.csv", tabledir))
actions = cf.fewcol.result %>%
  distinct(action) %>%
  tidyr::separate(action, into=c("l1", "l2", "l3"), sep="_") %>%
  dplyr::filter(l2 != "NA") %>%
  dplyr::arrange(l1, l3, l2) %>%
  dplyr::mutate(action = ifelse(is.na(l3), l2, paste(l2, " ", l3))) %>%
  dplyr::mutate_at(dplyr::vars(l1), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate(l1 = ifelse(l1 %in% c("Advanced Metering", "Commissioning", "GSALink"), "Operational", l1)) %>%
  distinct(l1) %>%
  dplyr::mutate(l1 = gsub(" ", "-", l1)) %>%
  .$l1 %>%
  {.}

period = "now"
scenario = "measured"

compile.targeting.by.action <- function(period, scenario, actions) {
  images <- sprintf("targeting_by_action_slides_%s_%s_%s_bp_measured_input", period, scenario,
                    c("toplevel", "highlevel", "joint_highlevel"))
  images <- c(images,
              sprintf("targeting_by_action_slides_%s_%s_%s_detaillevel_bp_measured_input",
                      actions, period, scenario))
  contents = sapply(images, function(imagei) {
    newlines <- c("\\begin{figure}[H]",
                  "\\centering",
                  sprintf("\\includegraphics[width=0.9\\linewidth]{../images/%s}",
                          imagei),
                  "\\end{figure}")
    return(newlines)
  })
  con <- file(sprintf("../images/targeting_by_action_noaa_%s_%s.tex", period, scenario), open = "w+")
  writeLines(contents, con, sep = "\n", useBytes = FALSE)
  close(con)
}

compile.targeting.by.action("now", "measured", actions)
for (period in c("mid-century", "late-century")) {
  for (scenario in c("rcp45", "rcp85")) {
    compile.targeting.by.action(period, scenario, actions)
  }
}

compile.targeting.by.action.roi <- function(period, scenario) {
  images <- sprintf("targeting_by_action_roi_%s_%s_%s_bp_measured_input.png",
                    period, scenario, c("toplevel", "highlevel"))
  contents = sapply(images, function(imagei) {
    newlines <- c("\\begin{figure}[H]",
                  "\\centering",
                  sprintf("\\includegraphics[width=0.9\\linewidth]{../images/%s}",
                          imagei),
                  "\\end{figure}")
    return(newlines)
  })
  con <- file(sprintf("../images/targeting_by_action_roi_noaa_%s_%s.tex", period, scenario), open = "w+")
  writeLines(contents, con, sep = "\n", useBytes = FALSE)
  close(con)
}

compile.targeting.by.action.roi("now", "measured")
for (period in c("mid-century", "late-century")) {
  for (scenario in c("rcp45", "rcp85")) {
    compile.targeting.by.action.roi(period, scenario)
  }
}
