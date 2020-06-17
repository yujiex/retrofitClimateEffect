library("ggplot2")
library("dplyr")

## compute the benefit of targeting

imagedir = "~/Dropbox/thesis/code/retrofitClimateEffect/images"
tabledir = "~/Dropbox/thesis/code/retrofitClimateEffect/tables"

pre.energy = cf.result %>%
  dplyr::filter(is.real.retrofit==0, scenario == "measured") %>%
  dplyr::distinct(BLDGNUM, Substantial_Completion_Date, KWHR, GAS, GROSSSQFT) %>%
  tidyr::gather(fuel, value, KWHR:GAS) %>%
  dplyr::mutate(kbtu = value * GROSSSQFT) %>%
  dplyr::group_by(fuel) %>%
  dplyr::summarise(pre.kbtu = sum(kbtu)) %>%
  dplyr::ungroup() %>%
  {.}

suf = "_measured_input"

## kw = "highlevel_bp"
## kw = "detaillevel_bp"
## kw = "toplevel_bp"
kw = "joint_highlevel_bp"
kw = paste0(kw, suf)

cf.result = readr::read_csv(sprintf("%s/grf_result_%s.csv", tabledir, kw))
## fixme: detail level filename
## fixme: detail level facet order

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
  to.plot = all.no.targeting %>%
    dplyr::bind_rows(all.with.targeting) %>%
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
                                          round(percent.save, 0) * (-1),
                                          "% ", saving.suf)) %>%
    ## make savings positive
    dplyr::mutate(mmbtu=(-1) * mmbtu) %>%
    {.}
  ## for all action
  dfs.to.plot = to.plot %>%
    dplyr::group_by(period, scenario) %>%
    dplyr::group_split()
  lapply(dfs.to.plot, function(df.to.plot) {
    target.period = df.to.plot$period[[1]]
    target.scenario = df.to.plot$scenario[[1]]
    scenario.str = ifelse(target.scenario == "measured", "", target.scenario)
    lim = max(df.to.plot$mmbtu) * 1.4
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

to.plot <- by.action.no.targeting %>%
  dplyr::bind_rows(by.action.with.targeting) %>%
  dplyr::mutate(mmbtu = predictions.kbtu / 1000) %>%
  dplyr::mutate_at(vars(fuel), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  dplyr::mutate_at(vars(period), dplyr::recode,
                   "2050Jan through 2059Jan"="mid-century",
                   "2090Jan through 2099Jan"="late-century") %>%
  dplyr::mutate(fuel=factor(fuel, levels=c("Natural Gas", "Electricity"))) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate(mmbtu = -1 * mmbtu) %>%
  {.}

if (!stringr::str_detect(kw, "detail")) {
  if (stringr::str_detect(kw, "toplevel")) {
    num.of.col = 3
    image.width = 9
    image.height = 3
    mult = 1.5
  } else {
    num.of.col = 2
    image.width = 7
    image.height = 4
    mult = 1.4
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
      ggplot2::ggplot(ggplot2::aes(x=fuel, y=mmbtu, fill=interaction(label, fuel), label=count)) +
      ggplot2::geom_bar(stat="identity", position="dodge") +
      ggplot2::ggtitle(sprintf("Targeting only the reducers vs all buildings taking an action\n%s %s", target.period, scenario.str)) +
      ggplot2::scale_fill_manual(values=RColorBrewer::brewer.pal(8, "Paired")[1:4], name="") +
      ggplot2::facet_wrap(.~action, ncol=num.of.col) +
      ggplot2::coord_flip() +
      ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9), size=3) +
      ggplot2::expand_limits(y=lim) +
      ggplot2::scale_y_reverse() +
      ggplot2::ylab("MMBtu") +
      ggplot2::theme(legend.position="none", title=element_text(size=9), axis.title.y=element_blank())
    ggplot2::ggsave(sprintf("%s/targeting_by_action_slides_%s_%s_%s.png", imagedir, target.period, target.scenario, kw), width=image.width, height=image.height)
  })
} else if (stringr::str_detect(kw, "detail")) {
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
    lim = max(df.action$mmbtu) * 1.5
    n.action = length(unique(df.action$action))
    image.height = round(n.action * 2/3, 1)
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
      ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9), size=3) +
      ggplot2::expand_limits(y=lim) +
      ggplot2::scale_y_reverse() +
      ggplot2::ylab("MMBtu") +
      ggplot2::theme(legend.position="none", title=element_text(size=9), axis.title.y=element_blank())
    ggplot2::ggsave(sprintf("%s/targeting_by_action_slides_%s_%s_%s_%s.png", imagedir, l1.action, target.period, target.scenario, kw), width=7, height=image.height)
  })
}
