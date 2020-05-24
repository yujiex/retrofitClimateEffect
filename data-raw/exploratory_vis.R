library("sf")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("jrvFinance")

load("../data/building_location.rda")
load("../data/energy_monthly_web_withloc.rda")
load("../data/energy_monthly_web_continental.rda")
load("../data/energy_90_to_18.rda")
load("../data/retrofit_from_db.rda")

load("../data/retrofit.alldata.rda")
load("../data/retrofit.alldata.highprop.rda")

imagedir = "~/Dropbox/thesis/code/retrofitClimateEffect/images"
## imagedir = "~/Dropbox/thesis/writeups/plan/images"
## tabledir = "~/Dropbox/thesis/writeups/plan/tables"
tabledir = "~/Dropbox/thesis/code/retrofitClimateEffect/tables"

## plot monthly building kbtu/sqft trend for all buildings (with location info)
dfs = energy_monthly_web_withloc %>%
  dplyr::group_by(variable) %>%
  dplyr::group_split() %>%
  {.}

lapply(dfs, function(df) {
  fuel = df$variable[[1]]
  df %>%
    dplyr::mutate(kbtu.per.sqft = amt.kbtu/GROSSSQFT) %>%
    dplyr::group_by(BLDGNUM, Month, variable) %>%
    dplyr::summarise(kbtu.per.sqft = mean(kbtu.per.sqft)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Month = factor(Month)) %>%
    ggplot2::ggplot(ggplot2::aes(x=Month, y=kbtu.per.sqft, group=BLDGNUM)) +
    ggplot2::ggtitle(sprintf("Average monthly %s consumption by building", fuel)) +
    ggplot2::ylab("kBtu/sqft") +
    ggplot2::geom_line() +
    ggplot2::theme()
  ggsave(sprintf("%s/avg_monthly_kbtu_%s_by_building.png", imagedir, fuel), width=5, height=3)
})

## plot monthly building kbtu/sqft trend for retrofit buildings
dfs = energy_monthly_web_withloc %>%
  dplyr::filter(BLDGNUM %in% unique(retrofit.alldata$BLDGNUM)) %>%
  ## restrict to the years for the retrofit analysis
  dplyr::filter(2004 < Year, Year < 2019) %>%
  dplyr::group_by(variable) %>%
  dplyr::group_split() %>%
  {.}

lapply(dfs, function(df) {
  fuel = df$variable[[1]]
  df %>%
    dplyr::mutate(kbtu.per.sqft = amt.kbtu/GROSSSQFT) %>%
    dplyr::group_by(BLDGNUM, Month, variable) %>%
    dplyr::summarise(kbtu.per.sqft = mean(kbtu.per.sqft)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Month = factor(Month)) %>%
    ggplot2::ggplot(ggplot2::aes(x=Month, y=kbtu.per.sqft, group=BLDGNUM)) +
    ggplot2::labs(title=sprintf("Average monthly %s consumption by building", fuel),
                  subtitle="2005-2018") +
    ggplot2::ylab("kBtu/sqft") +
    ggplot2::geom_line() +
    ggplot2::theme()
  ggsave(sprintf("%s/retro_avg_monthly_kbtu_%s_by_building.png", imagedir, fuel), width=5, height=3)
})

## plot number of buildings with energy data in each fiscal year
energy_monthly_web_withloc %>%
  dplyr::group_by(FYR) %>%
  dplyr::summarise(`number of building`=length(unique(BLDGNUM))) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x=`FYR`, y=`number of building`)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::xlab("Fiscal Year") +
  ggplot2::ylab("number of building") +
  ggplot2::theme_bw() +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/euas_web_num_building.png", imagedir), width=6, height=2)

## plot distribution of the number of years of data a building has
energy_monthly_web_withloc %>%
  dplyr::distinct(FYR, BLDGNUM) %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::summarise(cnt=n()) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x=cnt)) +
  ggplot2::geom_histogram(binwidth = 5) +
  ggplot2::theme_bw() +
  ggplot2::xlab("number of years with data") +
  ggplot2::ylab("number of building") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/euas_web_num_year.png", imagedir), width=6, height=2)

longterm = energy_90_to_18 %>%
  distinct(BLDGNUM) %>%
  dplyr::left_join(building_location, by=c("BLDGNUM")) %>%
  dplyr::select(BLDGNUM, Latitude, Longitude)

state_shape = sf::st_read("geographical/cb_2017_us_state_20m.shp")

p <- state_shape %>%
  dplyr::filter(!`STUSPS` %in% c("AK", "HI", "VI", "GU", "PR", "VI")) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill="grey")

print(p)

df = energy_monthly_web_continental %>%
  distinct(BLDGNUM, Year) %>%
  dplyr::left_join(building_location, by=c("BLDGNUM")) %>%
  dplyr::select(BLDGNUM, Year, Latitude, Longitude)

year = 1990
dfyear = df %>%
  dplyr::filter(Year==year)

p +
  ggplot2::ggtitle(sprintf("GSA buildings in %s", year)) +
  ggplot2::geom_point(ggplot2::aes(y=`Latitude`, x=`Longitude`), data=dfyear, size=0.5) +
  ggplot2::ggtitle(sprintf("Buildings in year %s (n=%d)", year, nrow(dfyear))) +
  ggplot2::theme()
ggsave(sprintf("%s/buildingloc_%d.png", imagedir, year), width=6, height=3)

year = 2018
dfyear = df %>%
  dplyr::filter(Year==year)

p +
  ggplot2::ggtitle(sprintf("GSA buildings in %s", year)) +
  ggplot2::geom_point(ggplot2::aes(y=`Latitude`, x=`Longitude`), data=dfyear, size=0.5) +
  ggplot2::ggtitle(sprintf("Buildings in year %s (n=%d)", year, nrow(dfyear))) +
  ggplot2::theme()
ggsave(sprintf("%s/buildingloc_%d.png", imagedir, year), width=6, height=3)

p +
  ggplot2::geom_point(ggplot2::aes(y=`Latitude`, x=`Longitude`), data=longterm, size=0.5) +
  ggplot2::ggtitle(sprintf("GSA buildings with data from 1990 to 2018 (n=%d)", year, nrow(longterm))) +
  ggplot2::theme()
ggsave(sprintf("%s/buildingloc_30year.png", imagedir), width=6, height=3)

load("../data/building_ann_cldd_normal.rda")

building_ann_cldd_normal <- building_ann_cldd_normal %>%
  dplyr::left_join(building_location, by="BLDGNUM") %>%
  dplyr::rename(latitude=Latitude, longitude=Longitude) %>%
  {.}

p +
  ggplot2::geom_point(ggplot2::aes(y=`latitude`, x=`longitude`, color=cldd), data=building_ann_cldd_normal) +
  ggplot2::scale_color_gradient(low="deepskyblue", high="orangered") +
  ggplot2::theme()
ggsave(sprintf("%s/buildingloc_30year_cddnormal.png", imagedir), width=6, height=3)

building_ann_cldd_normal <- building_ann_cldd_normal %>%
  dplyr::mutate(climate.verbose=factor(climate.verbose, levels=c("cold:[30.4,737.7)", "mild:[737.7,1402.1)", "hot:[1402.1,4764.2)")))

p +
  ggplot2::ggtitle("Climate classification") +
  ggplot2::geom_point(ggplot2::aes(y=`latitude`, x=`longitude`, color=climate.verbose), data=building_ann_cldd_normal) +
  ggplot2::scale_color_manual(values = RColorBrewer::brewer.pal(3, "RdBu")[c(3, 2, 1)], name="Climate") +
  ggplot2::theme()
ggsave(sprintf("%s/buildingloc_30year_climate.png", imagedir), width=6, height=3)

retrofit_from_db %>%
  dplyr::rename(`BLDGNUM`=`Building_Number`) %>%
  dplyr::distinct(BLDGNUM, Substantial_Completion_Date) %>%
  dplyr::arrange((`Substantial_Completion_Date`)) %>%
  {.}

energy_during_retro_period = energy_monthly_web_withloc %>%
  dplyr::filter(2005 < Year,  Year < 2019) %>%
  {.}

counts = retrofit.alldata.highprop %>%
## counts = retrofit.alldata %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::rename(retrofitted = is.real.retrofit) %>%
  dplyr::distinct(retrofitted, BLDGNUM) %>%
  dplyr::group_by(retrofitted) %>%
  dplyr::summarise(n()) %>%
  {.}
## retrofitted `n()`
## <lgl>       <int>
## 1 FALSE         274
## 2 TRUE          255

## plot propensity score distribution
retrofit.alldata.highprop %>%
  dplyr::filter(retro.status=="pre") %>%
  dplyr::distinct(BLDGNUM, `Substantial_Completion_Date`, propensity.estimate, is.real.retrofit) %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                         paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  ggplot2::ggplot(ggplot2::aes(x = propensity.estimate, group=retrofit.label, fill=retrofit.label)) +
  ggplot2::geom_density(alpha=0.5) +
  ggplot2::labs(fill = "Whether retrofitted") +
  ggplot2::xlab("Estimated propensity score") +
  ggplot2::ggtitle("Distribution of estimated propensity score") +
  ggplot2::theme(legend.position="bottom")
ggsave(sprintf("%s/propensity_by_treat_highprop.png", imagedir), width=6, height=5)

## plot number of buildings with energy data in each fiscal year
note.size = 3
energy_during_retro_period %>%
  dplyr::filter(variable %in% c("KWHR", "GAS")) %>%
  ## dplyr::filter(!(variable %in% c("OIL", "KWDMD"))) %>%
  dplyr::filter(BLDGNUM %in% unique(retrofit.alldata.highprop$BLDGNUM)) %>%
  ## dplyr::filter(BLDGNUM %in% unique(retrofit.alldata$BLDGNUM)) %>%
  dplyr::mutate(kbtu.per.sqft = amt.kbtu/GROSSSQFT) %>%
  dplyr::group_by(BLDGNUM, Year, Month, variable) %>%
  dplyr::summarise(total.kbtu.per.sqft = sum(kbtu.per.sqft, na.rm = TRUE),
                   GROSSSQFT=first(GROSSSQFT)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`retrofit status`= ifelse(BLDGNUM %in% unique(retrofit_from_db$`Building_Number`),
                                     paste0("retrofitted n=", counts$`n()`[[2]]),
                                     paste0("un-retrofitted n=", counts$`n()`[[1]]))) %>%
  ## dplyr::group_by(`retrofit status`, Year, Month, variable) %>%
  ## dplyr::summarise(avg.total.kbtu.sqft = mean(total.kbtu.per.sqft)) %>%
  ## dplyr::ungroup() %>%
  dplyr::mutate(`yearmonth`=zoo::as.yearmon(paste(`Year`, `Month`), "%Y %m")) %>%
  dplyr::mutate_at(vars(variable), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  ggplot2::ggplot(ggplot2::aes(x=yearmonth, y=total.kbtu.per.sqft,
                               group=`retrofit status`,
                               color=`retrofit status`)) +
  ## ggplot2::ggplot(ggplot2::aes(x=yearmonth, y=avg.total.kbtu.sqft, group=`retrofit status`, color=`retrofit status`)) +
  ggfan::geom_interval() +
  ggplot2::scale_linetype_manual(values=c("solid","82","42")) +
  ## ggfan::geom_fan(intervals = 1:4/5) +
  ggplot2::geom_vline(xintercept=zoo::as.yearmon("2010-03", format="%Y-%m"), linetype="dashed") +
  ggplot2::geom_vline(xintercept=zoo::as.yearmon("2015-10", format="%Y-%m"), linetype="dashed") +
  ggplot2::annotate("text", x=zoo::as.yearmon("2009-03", format="%Y-%m"), y=13, label="2010-03", size=note.size) +
  ggplot2::annotate("text", x=zoo::as.yearmon("2016-10", format="%Y-%m"), y=13, label="2015-10", size=note.size) +
  ggplot2::annotate("text", x=zoo::as.yearmon("2013-03", format="%Y-%m"), y=13, label="retrofit completion dates \nfall in this interval", size=note.size) +
  ggplot2::expand_limits(y=15) +
  ggplot2::facet_wrap(.~variable, ncol=1) +
  ggplot2::ylab("Monthly kbtu/sqft") +
  ggplot2::xlab("Time") +
  ggplot2::theme(legend.position = "bottom")
ggsave(sprintf("%s/fanplot_trend_by_fuel_highprop.png", imagedir), width=8, height=6)

## plot number of buildings with energy data in each fiscal year
note.size = 3
energy_during_retro_period %>%
  dplyr::filter(variable %in% c("KWHR", "GAS")) %>%
  ## dplyr::filter(!(variable %in% c("OIL", "KWDMD"))) %>%
  dplyr::filter(BLDGNUM %in% unique(retrofit.alldata.highprop$BLDGNUM)) %>%
  ## dplyr::filter(BLDGNUM %in% unique(retrofit.alldata$BLDGNUM)) %>%
  dplyr::mutate(kbtu.per.sqft = amt.kbtu/GROSSSQFT) %>%
  dplyr::group_by(BLDGNUM, Year, Month, variable) %>%
  dplyr::summarise(total.kbtu.per.sqft = sum(kbtu.per.sqft, na.rm = TRUE),
                   GROSSSQFT=first(GROSSSQFT)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`retrofit status`= ifelse(BLDGNUM %in% unique(retrofit_from_db$`Building_Number`),
                                     paste0("retrofitted n=", counts$`n()`[[2]]),
                                     paste0("un-retrofitted n=", counts$`n()`[[1]]))) %>%
  ## dplyr::group_by(`retrofit status`, Year, Month, variable) %>%
  ## dplyr::summarise(avg.total.kbtu.sqft = mean(total.kbtu.per.sqft)) %>%
  ## dplyr::ungroup() %>%
  dplyr::mutate(`yearmonth`=zoo::as.yearmon(paste(`Year`, `Month`), "%Y %m")) %>%
  dplyr::mutate_at(vars(variable), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  ggplot2::ggplot(ggplot2::aes(x=yearmonth, y=total.kbtu.per.sqft,
                               group=interaction(`retrofit status`, variable)
                               ## color=`retrofit status`
                               )) +
  ## ggplot2::ggplot(ggplot2::aes(x=yearmonth, y=avg.total.kbtu.sqft, group=`retrofit status`, color=`retrofit status`)) +
  ## ggfan::geom_interval() +
  ggfan::geom_fan(intervals = 1:4/5) +
  ggplot2::geom_vline(xintercept=zoo::as.yearmon("2010-03", format="%Y-%m"), linetype="dashed") +
  ggplot2::geom_vline(xintercept=zoo::as.yearmon("2015-10", format="%Y-%m"), linetype="dashed") +
  ggplot2::annotate("text", x=zoo::as.yearmon("2009-03", format="%Y-%m"), y=13, label="2010-03", size=note.size) +
  ggplot2::annotate("text", x=zoo::as.yearmon("2016-10", format="%Y-%m"), y=13, label="2015-10", size=note.size) +
  ggplot2::annotate("text", x=zoo::as.yearmon("2013-03", format="%Y-%m"), y=13, label="retrofit completion dates \nfall in this interval", size=note.size) +
  ggplot2::expand_limits(y=15) +
  ggplot2::facet_grid(`retrofit status`~variable) +
  ggplot2::ylab("Monthly kbtu/sqft") +
  ggplot2::xlab("Time") +
  ggplot2::theme(legend.position = "bottom")

ggsave(sprintf("%s/fanplot_trend_by_fuel_highprop.png", imagedir), width=8, height=6)

## plot average buildings energy trend
note.size = 3
energy_during_retro_period %>%
  dplyr::filter(!(variable %in% c("OIL", "KWDMD"))) %>%
  dplyr::filter(BLDGNUM %in% unique(retrofit.alldata.highprop$BLDGNUM)) %>%
  ## dplyr::filter(BLDGNUM %in% unique(retrofit.alldata$BLDGNUM)) %>%
  dplyr::mutate(kbtu.per.sqft = amt.kbtu/GROSSSQFT) %>%
  dplyr::group_by(BLDGNUM, Year, Month) %>%
  dplyr::summarise(total.kbtu.per.sqft = sum(kbtu.per.sqft, na.rm = TRUE),
                   GROSSSQFT=first(GROSSSQFT)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`retrofit status`= ifelse(BLDGNUM %in% unique(retrofit_from_db$`Building_Number`),
                                     paste0("retrofitted n=", counts$`n()`[[2]]),
                                     paste0("un-retrofitted n=", counts$`n()`[[1]]))) %>%
  dplyr::group_by(`retrofit status`, Year, Month) %>%
  dplyr::summarise(avg.total.kbtu.sqft = mean(total.kbtu.per.sqft)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`yearmonth`=zoo::as.yearmon(paste(`Year`, `Month`), "%Y %m")) %>%
  ggplot2::ggplot(ggplot2::aes(x=yearmonth, y=avg.total.kbtu.sqft, group=`retrofit status`, color=`retrofit status`)) +
  ggplot2::geom_line() +
  ggplot2::geom_vline(xintercept=zoo::as.yearmon("2010-03", format="%Y-%m"), linetype="dashed") +
  ggplot2::geom_vline(xintercept=zoo::as.yearmon("2015-10", format="%Y-%m"), linetype="dashed") +
  ggplot2::annotate("text", x=zoo::as.yearmon("2009-03", format="%Y-%m"), y=9, label="2010-03", size=note.size) +
  ggplot2::annotate("text", x=zoo::as.yearmon("2016-10", format="%Y-%m"), y=9, label="2015-10", size=note.size) +
  ggplot2::annotate("text", x=zoo::as.yearmon("2013-03", format="%Y-%m"), y=9, label="retrofit completion dates \nfall in this interval", size=note.size) +
  ggplot2::expand_limits(y=9.5) +
  ggplot2::ylab("Monthly average kbtu/sqft") +
  ggplot2::xlab("Time") +
  ggplot2::theme(legend.position = "bottom")
## ggsave(sprintf("%s/energy_retrofit_status.png", imagedir), width=6, height=3)
ggsave(sprintf("%s/energy_retrofit_status_highprop.png", imagedir), width=6, height=3)

load("../data/retrofit.energy.rda")

## check out outlier buildings
## retrofit.energy %>%
retrofit.energy %>%
  dplyr::filter(BLDGNUM %in% c("IL0300ZZ" , "MD0334ZZ" , "MO0134ZZ" , "NY0000MB" , "UT0037ZZ")) %>%
  dplyr::filter(BLDGNUM %in% unique(retrofit.alldata.highprop$BLDGNUM)) %>%
  dplyr::mutate(kbtu.per.sqft = amt.kbtu/GROSSSQFT) %>%
  dplyr::filter(variable == "GAS") %>%
  dplyr::mutate(`yearmonth`=zoo::as.yearmon(paste(`Year`, `Month`), "%Y %m")) %>%
  ggplot2::ggplot(ggplot2::aes(x=yearmonth, y=kbtu.per.sqft, group=`BLDGNUM`, color=`BLDGNUM`)) +
  ggplot2::geom_line() +
  ggplot2::geom_vline(xintercept=zoo::as.yearmon("2011-05", format="%Y-%m"), linetype="dashed") +
  ggplot2::geom_vline(xintercept=zoo::as.yearmon("2010-11", format="%Y-%m"), linetype="dashed") +
  ggplot2::geom_vline(xintercept=zoo::as.yearmon("2012-05", format="%Y-%m"), linetype="dashed") +
  ggplot2::geom_vline(xintercept=zoo::as.yearmon("2013-09", format="%Y-%m"), linetype="dashed") +
  ggplot2::theme()

energy_during_retro_period %>%
  dplyr::filter(BLDGNUM %in% c("IL0300ZZ" , "MD0334ZZ" , "MO0134ZZ" , "NY0000MB" , "UT0037ZZ")) %>%
  dplyr::mutate(kbtu.per.sqft = amt.kbtu/GROSSSQFT) %>%
  dplyr::filter(variable %in% c("KWHR", "GAS")) %>%
  dplyr::arrange(variable, BLDGNUM, Year, Month) %>%
  readr::write_csv("eui_outlier.csv")

load("../data/retrofit.alldata.rda")

## summary stats of retrofit data

## compare average monthly consumption before and after retrofit for the actually retrofitted
retrofit.alldata.highprop %>%
  dplyr::filter(!(variable %in% c("OIL", "KWDMD"))) %>%
  dplyr::mutate_at(vars(variable), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`, variable,
                is.real.retrofit, mean.kbtu, retro.status) %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                          paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                          paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  tidyr::spread(retro.status, mean.kbtu) %>%
  dplyr::mutate(diff.mean.kbtu = post - pre) %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=diff.mean.kbtu, fill=retrofit.label)) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_brewer(name = "Whether retrofitted", palette = "Purples", direction = -1) +
  ggplot2::xlab("fuel type") +
  ggplot2::ylab("post minus pre retrofit average monthly consumption") +
  ggplot2::theme()
ggsave(sprintf("%s/diff_mean_energy_highprop.png", imagedir), width=7, height=4)

options(tibble.width=Inf)

df.energy.diff = retrofit.alldata.highprop %>%
  ## df.energy.diff = retrofit.alldata %>%
  dplyr::filter(!(variable %in% c("OIL", "KWDMD"))) %>%
  dplyr::mutate_at(vars(variable), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`, variable,
                is.real.retrofit, mean.kbtu, retro.status, GROSSSQFT) %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                         paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::select(-is.real.retrofit) %>%
  dplyr::mutate(mean.kbtu.psqft = mean.kbtu / GROSSSQFT) %>%
  dplyr::select(-mean.kbtu, -GROSSSQFT) %>%
  tidyr::spread(retro.status, mean.kbtu.psqft) %>%
  dplyr::mutate(diff.mean.kbtu = post - pre) %>%
  {.}

df.energy.diff %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=diff.mean.kbtu, fill=retrofit.label)) +
  ## ggplot2::geom_violin() +
  ggplot2::geom_boxplot() +
  ggplot2::xlab("fuel type") +
  ggplot2::ylab("kbtu per sqft") +
  ggplot2::ggtitle("post minus pre-retrofit average monthly kBtu/sqft") +
  ggplot2::scale_fill_brewer(name = "Whether retrofitted", palette = "Purples", direction = -1) +
  ggplot2::theme(legend.position="bottom")
ggplot2::ggsave(sprintf("%s/diff_mean_eui_highprop.png", imagedir), width=7, height=4)

## before and after retrofit, plot energy against sqft
retrofit.alldata.highprop %>%
  dplyr::filter(variable %in% c("KWHR", "GAS")) %>%
  dplyr::mutate(kbtu.per.sqft = mean.kbtu/GROSSSQFT) %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                         paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::mutate_at(vars(variable), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  ggplot2::ggplot(ggplot2::aes(x=GROSSSQFT, y=kbtu.per.sqft,
                               group=interaction(retro.status, retrofit.label, variable),
                               color=retro.status)) +
  ggplot2::geom_smooth(fill="#D9D9D9") +
  ggplot2::geom_point(size=0.3, alpha=0.3) +
  ggplot2::facet_grid(retrofit.label ~ variable) +
  ## ggplot2::scale_color_manual(name="timing", values=c("#E87D72", "#E8B4B0", "#55BCC2", "#9DD3D6")) +
  ggplot2::scale_color_brewer(name="timing", palette="Set1") +
  ggplot2::scale_x_log10() +
  ggplot2::xlab("Building Size (in log scale)") +
  ggplot2::ylab("kbtu/sqft") +
  ggplot2::ggtitle("Energy use before and after retrofit vs building size") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position="bottom")
ggplot2::ggsave(sprintf("%s/before_vs_after_by_sqft.png", imagedir), width=6, height=4)

df.size = retrofit.alldata.highprop %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit, `GROSSSQFT`) %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                         paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::select(-is.real.retrofit) %>%
  dplyr::mutate(dataset = "study") %>%
  {.}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## distribution of building size in the study set
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
df.size %>%
  ggplot2::ggplot(ggplot2::aes(y=GROSSSQFT, x=retrofit.label, fill=retrofit.label)) +
  ggplot2::geom_violin(alpha=0.5) +
  ggplot2::geom_boxplot(width=0.05, outlier.shape=NA) +
  ggplot2::ylab("Building Size (sqft)") +
  ggplot2::xlab("Whether retrofitted") +
  ggplot2::expand_limits(y=6.5e6) +
  ggplot2::theme(legend.position="none")
ggplot2::ggsave(sprintf("%s/difference_in_sqft_studyset.png", imagedir), width=3, height=4)

load("../data/building_sqft_category_region_raw.rda")
load("../data/retrofit_from_db.rda")

cnt.raw = building_sqft_category_region_raw %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::summarise(GROSSSQFT = mean(GROSSSQFT)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(is.retrofit = BLDGNUM %in% unique(retrofit_from_db$Building_Number)) %>%
  dplyr::group_by(is.retrofit) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup() %>%
  {.}

df.size.all = building_sqft_category_region_raw %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::summarise(GROSSSQFT = mean(GROSSSQFT)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(retrofit.label = ifelse(BLDGNUM %in% unique(retrofit_from_db$Building_Number),
                                        paste0("With retrofit \n(n=", cnt.raw$`n()`[[2]], ")"),
                                        paste0("Without retrofit \n(n=", cnt.raw$`n()`[[1]], ")"))) %>%
  dplyr::mutate(dataset = "all") %>%
  {.}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## distribution of building size in the whole portfolio
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
df.size.all %>%
  ggplot2::ggplot(ggplot2::aes(y=GROSSSQFT, x=retrofit.label, fill=retrofit.label)) +
  ggplot2::geom_violin(alpha=0.5) +
  ggplot2::geom_boxplot(width=0.02, outlier.shape=NA) +
  ggplot2::ylab("Building Size (sqft)") +
  ggplot2::xlab("Whether retrofitted") +
  ggplot2::theme(legend.position="none")
ggplot2::ggsave(sprintf("%s/difference_in_sqft_raw.png", imagedir), width=3, height=4)

df.energy.diff %>%
  dplyr::mutate(retrofit.label = ifelse(BLDGNUM %in% unique(retrofit_from_db$Building_Number),
                                        paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                        paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::filter(variable %in% c("Electricity", "Natural Gas")) %>%
  dplyr::left_join(df.size, by=c("BLDGNUM", "retrofit.label")) %>%
  ggplot2::ggplot(ggplot2::aes(x=GROSSSQFT, y=diff.mean.kbtu, color=retrofit.label)) +
  ggplot2::geom_point(size=0.3, alpha=0.3) +
  ggplot2::geom_smooth() +
  ggplot2::facet_wrap(.~variable) +
  ggplot2::xlab("Building Size (sqft)") +
  ggplot2::ylab("kbtu/sqft") +
  ## ggplot2::coord_cartesian(ylim=c(-5, 5)) +
  ggplot2::ggtitle("post minus pre-retrofit average monthly kBtu/sqft for different building size") +
  ## ggplot2::scale_color_brewer(name = "Whether retrofitted") +
  ggplot2::labs(color = "Whether retrofitted") +
  ## ggplot2::theme(legend.position="bottom")
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/diff_mean_eui_gsf.png", imagedir), width=8, height=3)

retro.weather = retrofit.alldata %>%
  dplyr::filter(!(variable %in% c("OIL", "KWDMD"))) %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`,
                is.real.retrofit, `<10`:`>90`) %>%
  dplyr::mutate(retrofit.label = ifelse(BLDGNUM %in% unique(retrofit_from_db$Building_Number),
                                        paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                        paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::distinct() %>%
  {.}

retro.weather.onebin = retrofit.alldata %>%
  dplyr::filter(!(variable %in% c("OIL", "KWDMD"))) %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`,
                is.real.retrofit, `[30-40)`) %>%
  dplyr::mutate(retrofit.label = ifelse(BLDGNUM %in% unique(retrofit_from_db$Building_Number),
                                        paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                        paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::distinct() %>%
  {.}

df.energy.diff %>%
  dplyr::filter(variable %in% c("Electricity", "Natural Gas")) %>%
  dplyr::left_join(retro.weather.onebin, by=c("BLDGNUM", "Substantial_Completion_Date", "retrofit.label")) %>%
  ggplot2::ggplot(ggplot2::aes(x=`[30-40)`, y=diff.mean.kbtu, color=retrofit.label)) +
  ggplot2::geom_point(size=0.3, alpha=0.3) +
  ggplot2::geom_smooth() +
  ggplot2::facet_wrap(.~variable) +
  ggplot2::xlab("Number of days with daily mean temperature between 30F and 40F") +
  ggplot2::ylab("kbtu/sqft") +
  ggplot2::ggtitle("post minus pre-retrofit average monthly kBtu/sqft for different temperature bin") +
  ## ggplot2::scale_color_brewer(name = "Whether retrofitted") +
  ggplot2::labs(color = "Whether retrofitted") +
  ggplot2::theme(legend.position="bottom")
ggplot2::ggsave(sprintf("%s/diff_mean_eui_onebin.png", imagedir), width=7, height=4)

retro.cdd = retrofit.alldata %>%
  dplyr::filter(!(variable %in% c("OIL", "KWDMD"))) %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`,
                is.real.retrofit, `CDD`) %>%
  dplyr::mutate(retrofit.label = ifelse(BLDGNUM %in% unique(retrofit_from_db$Building_Number),
                                        paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                        paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::distinct() %>%
  {.}

df.energy.diff %>%
  dplyr::filter(variable %in% c("Electricity", "Natural Gas")) %>%
  dplyr::left_join(retro.cdd, by=c("BLDGNUM", "Substantial_Completion_Date", "retrofit.label")) %>%
  ggplot2::ggplot(ggplot2::aes(x=CDD, y=diff.mean.kbtu, color=retrofit.label)) +
  ggplot2::geom_point(size=0.3, alpha=0.3) +
  ggplot2::geom_smooth() +
  ggplot2::facet_wrap(.~variable) +
  ggplot2::xlab("CDD") +
  ggplot2::ylab("kbtu/sqft") +
  ## ggplot2::coord_cartesian(ylim=c(-5, 5)) +
  ggplot2::ggtitle("post minus pre-retrofit average monthly kBtu/sqft for different CDD") +
  ## ggplot2::scale_color_brewer(name = "Whether retrofitted") +
  ggplot2::labs(color = "Whether retrofitted") +
  ## ggplot2::theme(legend.position="bottom")
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/diff_mean_eui_cdd.png", imagedir), width=8, height=3)

retro.hdd = retrofit.alldata %>%
  dplyr::filter(!(variable %in% c("OIL", "KWDMD"))) %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`,
                is.real.retrofit, `HDD`) %>%
  dplyr::mutate(retrofit.label = ifelse(BLDGNUM %in% unique(retrofit_from_db$Building_Number),
                                        paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                        paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::distinct() %>%
  {.}

df.energy.diff %>%
  dplyr::filter(variable %in% c("Electricity", "Natural Gas")) %>%
  dplyr::left_join(retro.hdd, by=c("BLDGNUM", "Substantial_Completion_Date", "retrofit.label")) %>%
  ggplot2::ggplot(ggplot2::aes(x=HDD, y=diff.mean.kbtu, color=retrofit.label)) +
  ggplot2::geom_point(size=0.3, alpha=0.3) +
  ggplot2::geom_smooth() +
  ggplot2::facet_wrap(.~variable) +
  ggplot2::xlab("HDD") +
  ggplot2::ylab("kbtu/sqft") +
  ## ggplot2::coord_cartesian(ylim=c(-5, 5)) +
  ggplot2::ggtitle("post minus pre-retrofit average monthly kBtu/sqft for different HDD") +
  ## ggplot2::scale_color_brewer(name = "Whether retrofitted") +
  ggplot2::labs(color = "Whether retrofitted") +
  ## ggplot2::theme(legend.position="bottom")
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/diff_mean_eui_hdd.png", imagedir), width=8, height=3)

retro.pre.energy = retrofit.alldata %>%
  dplyr::filter(!(variable %in% c("KWDMD", "OIL"))) %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(BLDGNUM, is.real.retrofit,
                `Substantial_Completion_Date`, variable, mean.kbtu, GROSSSQFT) %>%
  dplyr::mutate(kbtu.per.sqft = mean.kbtu/GROSSSQFT) %>%
  dplyr::select(-mean.kbtu, -GROSSSQFT) %>%
  tidyr::spread(variable, kbtu.per.sqft, fill=0) %>%
  dplyr::mutate(retrofit.label = ifelse(BLDGNUM %in% unique(retrofit_from_db$Building_Number),
                                        paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                        paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  {.}

df.energy.diff %>%
  dplyr::filter(variable %in% c("Electricity", "Natural Gas")) %>%
  dplyr::left_join(retro.pre.energy, by=c("BLDGNUM", "Substantial_Completion_Date", "retrofit.label")) %>%
  ggplot2::ggplot(ggplot2::aes(x=KWHR, y=diff.mean.kbtu, color=retrofit.label)) +
  ggplot2::geom_point(size=0.3, alpha=0.3) +
  ggplot2::geom_smooth() +
  ggplot2::facet_wrap(.~variable) +
  ggplot2::xlab("Pre retrofit electricity (kBtu/sqft)") +
  ggplot2::ylab("kbtu/sqft") +
  ggplot2::coord_cartesian(ylim=c(-13, 7)) +
  ggplot2::ggtitle("post minus pre-retrofit average monthly kBtu/sqft for different pre-retrofit electricity") +
  ## ggplot2::scale_color_brewer(name = "Whether retrofitted") +
  ggplot2::labs(color = "Whether retrofitted") +
  ## ggplot2::theme(legend.position="bottom")
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/diff_mean_eui_pre_elec.png", imagedir), width=8, height=3)

df.energy.diff %>%
  dplyr::filter(variable %in% c("Electricity", "Natural Gas")) %>%
  dplyr::left_join(retro.pre.energy, by=c("BLDGNUM", "Substantial_Completion_Date", "retrofit.label")) %>%
  ggplot2::ggplot(ggplot2::aes(x=GAS, y=diff.mean.kbtu, color=retrofit.label)) +
  ggplot2::geom_point(size=0.3, alpha=0.3) +
  ggplot2::geom_smooth() +
  ggplot2::facet_wrap(.~variable) +
  ggplot2::xlab("Pre retrofit gas (kBtu/sqft)") +
  ggplot2::ylab("kbtu/sqft") +
  ggplot2::coord_cartesian(ylim=c(-13, 7)) +
  ggplot2::ggtitle("post minus pre-retrofit average monthly kBtu/sqft for different pre-retrofit gas") +
  ## ggplot2::scale_color_brewer(name = "Whether retrofitted") +
  ggplot2::labs(color = "Whether retrofitted") +
  ## ggplot2::theme(legend.position="bottom")
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/diff_mean_eui_pre_gas.png", imagedir), width=8, height=3)

## classify temperature bin
df.energy.diff %>%
  dplyr::left_join(retro.weather, by=c("BLDGNUM", "Substantial_Completion_Date", "variable", "retrofit.label")) %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=diff.mean.kbtu, fill=retrofit.label)) +
  ## ggplot2::geom_violin() +
  ggplot2::geom_boxplot(outlier.size = 1.0) +
  ggplot2::facet_wrap(.~building.size.bin) +
  ggplot2::xlab("fuel type") +
  ggplot2::ylab("kbtu per sqft") +
  ggplot2::ggtitle("post minus pre-retrofit average monthly kBtu/sqft") +
  ggplot2::scale_fill_brewer(name = "Whether retrofitted", palette = "Purples", direction = -1) +
  ggplot2::theme(legend.position="bottom")
ggplot2::ggsave(sprintf("%s/diff_mean_eui_facet_gsf.png", imagedir), width=7, height=4)

retrofit.alldata.highprop %>%
  dplyr::distinct(BLDGNUM, Building_Type) %>%
  dplyr::group_by(`Building_Type`) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  {.}

## summary stats
## elec + gas
retrofit.alldata.highprop %>%
  dplyr::mutate(kbtu.per.sqft = mean.kbtu/GROSSSQFT) %>%
  dplyr::filter(variable %in% c("GAS", "KWHR")) %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::group_by(BLDGNUM, Substantial_Completion_Date, is.real.retrofit) %>%
  dplyr::summarise(kbtu.per.sqft = sum(kbtu.per.sqft)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise_at(vars(kbtu.per.sqft), tibble::lst(mean, median, sd, min, max)) %>%
  dplyr::ungroup() %>%
  {.}

## by elec and gas, annual
energy.summary = retrofit.alldata.highprop %>%
  ## change to annual summary
  dplyr::mutate(kbtu.per.sqft = mean.kbtu/GROSSSQFT * 12) %>%
  dplyr::filter(variable %in% c("GAS", "KWHR")) %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::group_by(is.real.retrofit, variable) %>%
  dplyr::summarise_at(vars(kbtu.per.sqft), tibble::lst(mean, median, sd, min, max)) %>%
  dplyr::ungroup() %>%
  {.}

nonenergy.summary = retrofit.alldata.highprop %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit, HDD, CDD, GROSSSQFT) %>%
  tidyr::gather(variable, value, HDD:GROSSSQFT) %>%
  dplyr::group_by(is.real.retrofit, variable) %>%
  dplyr::summarise_at(vars(value), tibble::lst(mean, median, sd, min, max)) %>%
  dplyr::ungroup() %>%
  {.}

energy.summary %>%
  dplyr::bind_rows(nonenergy.summary) %>%
  dplyr::arrange(desc(is.real.retrofit)) %>%
  readr::write_csv("numeric_summary.csv")

ecm.highlevel = retrofit_from_db %>%
  dplyr::distinct(`Building_Number`, `Substantial_Completion_Date`, `high_level_ECM`) %>%
  {.}

action.ratio = retrofit.alldata.highprop %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit, `Substantial_Completion_Date`) %>%
  dplyr::left_join(ecm.highlevel, by=c("BLDGNUM"="Building_Number",
                                       "Substantial_Completion_Date"="Substantial_Completion_Date")) %>%
  dplyr::group_by(high_level_ECM) %>%
  dplyr::summarise(ratio = round(n() / length(unique(.$BLDGNUM)) * 100, 1), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename(action=high_level_ECM) %>%
  tidyr::replace_na(list(action="None")) %>%
  {.}

action.ratio %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  ggplot2::ggplot(ggplot2::aes(x=action, y=count, label=paste0(round(ratio, 1), "%"))) +
  ggplot2::ggtitle("Building Count by Actions Type") +
  ggplot2::ylab("Building Count") +
  ggplot2::xlab("Actions") +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::geom_text(nudge_y = 10) +
  ggplot2::annotate("text", x=2.5, y=180, label="(percent of buildings in the study set)") +
  ggplot2::theme(axis.text.x = element_text(angle=90))
ggplot2::ggsave(sprintf("%s/action_count_bar.png", imagedir), width=5, height=5)

action.ratio %>%
  readr::write_csv("ecm_ratio.csv")

percent.no.retro = retrofit.alldata.highprop %>%
  dplyr::mutate(is.real.retrofit = as.numeric(is.real.retrofit)) %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(ratio = n() / length(.$BLDGNUM) * 100, count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename(n=is.real.retrofit) %>%
  dplyr::filter(n==0) %>%
  {.}

num.action = retrofit.alldata.highprop %>%
  dplyr::filter(is.real.retrofit) %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit, `Substantial_Completion_Date`) %>%
  dplyr::left_join(ecm.highlevel, by=c("BLDGNUM"="Building_Number",
                                       "Substantial_Completion_Date"="Substantial_Completion_Date")) %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(n) %>%
  dplyr::summarise(ratio = round(n() / length(unique(retrofit.alldata.highprop$BLDGNUM)) * 100, 1),
                   count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(percent.no.retro) %>%
  dplyr::arrange(n) %>%
  {.}

num.action %>%
  dplyr::mutate(n=factor(n)) %>%
  ggplot2::ggplot(ggplot2::aes(x=n, y=count, label=paste0(round(ratio, 1), "%"))) +
  ggplot2::geom_bar(stat="identity", fill="chartreuse3") +
  ggplot2::geom_text(nudge_y = 10, size=2) +
  ## ggplot2::annotate("text", x=3.8, y=280, label="(percent of buildings in the study set)") +
  ggplot2::ggtitle("Building Distribution by Number of Actions") +
  ggplot2::ylab("Building Count") +
  ggplot2::xlab("Number of Actions") +
  ggplot2::theme(title = element_text(size=8),
                 axis.title.x = element_text(size=10),
                 axis.title.y = element_text(size=10))
ggplot2::ggsave(sprintf("%s/num_action_count_bar.png", imagedir), width=3, height=3)

num.action %>%
  readr::write_csv("ratio_num_action.csv")

retrofit.alldata.highprop %>%
  dplyr::distinct(BLDGNUM, Building_Type, is.real.retrofit) %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                         paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::select(-is.real.retrofit) %>%
  dplyr::group_by(retrofit.label, Building_Type) %>%
  dplyr::summarise(count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Building_Type, retrofit.label) %>%
  ggplot2::ggplot(ggplot2::aes(x=retrofit.label, y=count, fill=Building_Type)) +
  ggplot2::geom_bar(stat="identity", position = "dodge") +
  ggplot2::geom_text(ggplot2::aes(label=count), vjust=1.5, position = position_dodge(0.9)) +
  ggplot2::scale_fill_brewer(name = "Building Type", palette = "Set3") +
  ggplot2::xlab("Whether retrofitted") +
  ggplot2::ylab("Building Count") +
  ggplot2::theme_bw()
ggplot2::ggsave(sprintf("%s/retrofit_building_type.png", imagedir), width=7, height=4)

## could control for LEED but may not have very high accuracy
retrofit.alldata.highprop %>%
  dplyr::distinct(BLDGNUM, with.leed, is.real.retrofit) %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                         paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::select(-is.real.retrofit) %>%
  dplyr::group_by(retrofit.label, with.leed) %>%
  dplyr::summarise(count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(with.leed, retrofit.label) %>%
  ggplot2::ggplot(ggplot2::aes(x=retrofit.label, y=count, fill=with.leed)) +
  ggplot2::geom_bar(stat="identity", position = "dodge") +
  ggplot2::geom_text(ggplot2::aes(label=count), vjust=1.5, position = position_dodge(0.9)) +
  ggplot2::xlab("Whether retrofitted") +
  ggplot2::ylab("Building Count") +
  ggplot2::scale_fill_brewer(name = "Have LEED prior to retrofit", palette = "Greens") +
  ggplot2::theme_bw() +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/retrofit_building_leed.png", imagedir), width=7, height=4)

retrofit.alldata.highprop %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::distinct(BLDGNUM, historic, is.real.retrofit) %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                         paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::select(-is.real.retrofit) %>%
  dplyr::group_by(retrofit.label, historic) %>%
  dplyr::summarise(count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(historic, retrofit.label) %>%
  ggplot2::ggplot(ggplot2::aes(x=retrofit.label, y=count, fill=historic)) +
  ggplot2::geom_bar(stat="identity", position = "dodge") +
  ggplot2::geom_text(ggplot2::aes(label=count), vjust=1.5, position = position_dodge(0.9)) +
  ggplot2::xlab("Whether retrofitted") +
  ggplot2::ylab("Building Count") +
  ggplot2::scale_fill_brewer(name = "Historic building", palette = "Blues") +
  ggplot2::theme_bw() +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/retrofit_building_historic.png", imagedir), width=7, height=4)

retrofit.alldata.highprop %>%
  dplyr::distinct(BLDGNUM, `is.real.retrofit`) %>%
  dplyr::group_by(`is.real.retrofit`) %>%
  dplyr::count() %>%
  dplyr::ungroup()

retrofit.alldata.highprop %>%
  dplyr::distinct(BLDGNUM, Substantial_Completion_Date, retro.status, GROSSSQFT, is.real.retrofit) %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                         paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::select(-is.real.retrofit) %>%
  ggplot2::ggplot(ggplot2::aes(x=retrofit.label, y=GROSSSQFT)) +
  ggplot2::geom_boxplot() +
  ggplot2::xlab("Whether retrofitted") +
  ggplot2::ylab("Building Size (sqft)") +
  ggplot2::ggtitle("Building Size Distribution") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/retrofit_building_size.png", imagedir), width=4, height=4)

retrofit.alldata.highprop %>%
  dplyr::filter(retro.status=="pre") %>%
  dplyr::filter(!(variable %in% c("OIL", "KWDMD"))) %>%
  dplyr::mutate_at(vars(variable), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  dplyr::mutate(mean.kbtu.psqft = mean.kbtu/GROSSSQFT) %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                         paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::select(-is.real.retrofit) %>%
  ggplot2::ggplot(ggplot2::aes(x=retrofit.label, y=mean.kbtu.psqft, fill=variable)) +
  ggplot2::geom_boxplot() +
  ggplot2::xlab("Whether retrofitted") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::ggtitle("Pre Retrofit Monthly Average Consumption per sqft") +
  ggplot2::scale_fill_brewer(name = "fuel type", palette = "Set3") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/retrofit_pre_consumption.png", imagedir), width=7, height=4)

## long-term climate normal
retrofit.alldata.highprop %>%
  dplyr::distinct(BLDGNUM, cldd, htdd, is.real.retrofit) %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                         paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::select(-is.real.retrofit) %>%
  tidyr::gather(degree.type, degree.day, cldd:htdd) %>%
  ggplot2::ggplot(ggplot2::aes(x=retrofit.label, y=degree.day, fill=degree.type)) +
  ggplot2::geom_boxplot() +
  ggplot2::xlab("Whether retrofitted") +
  ggplot2::ylab("degree day") +
  ggplot2::ggtitle("HDD and CDD climate normal") +
  ggplot2::scale_fill_manual(name = "climate normal", values = c("#67A9CF", "#EF8A62")) +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(sprintf("%s/retrofit_pre_climate_normal.png", imagedir), width=4, height=4)

## compare temperature bins of retrofitted vs non-retrofitted
retrofit.alldata.highprop %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit, (`<10`:`>90`)) %>%
  dplyr::distinct() %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
  dplyr::ungroup() %>%
  tidyr::gather(temperature.bin, days, -is.real.retrofit) %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         paste0("With retrofit (n=", counts$`n()`[[2]], ")"),
                                         paste0("Without retrofit (n=", counts$`n()`[[1]], ")"))) %>%
  dplyr::mutate(temperature.bin = factor(temperature.bin, levels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=temperature.bin, y=days, group=retrofit.label, fill=retrofit.label)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ## ggplot2::scale_fill_brewer(name = "Whether retrofitted") +
  ggplot2::scale_fill_grey(name = "Whether retrofitted") +
  ggplot2::xlab("Temperature bin") +
  ggplot2::ggtitle("Annual average number of days with temperature in certain range") +
  ggplot2::theme_bw() +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/retrofit_pre_temperature_bin.png", imagedir), width=7, height=4)

retrofit.alldata.highprop %>%
  dplyr::select(`Substantial_Completion_Date`) %>%
  summary()

## compare temperature bins of current vs mid-century
now.weather = retrofit.alldata.highprop %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`, (`<10`:`>90`)) %>%
  dplyr::distinct() %>%
  tidyr::gather(temperature.bin, days, `<10`:`>90`) %>%
  dplyr::mutate(label = "2007-2018") %>%
  dplyr::group_by(label, temperature.bin) %>%
  dplyr::summarise(days = mean(days)) %>%
  dplyr::ungroup() %>%
  {.}

model.weights = readr::read_csv("climate_model_weights.csv") %>%
  dplyr::select(-`Skill Weight`, -`Uniqueness Weight`) %>%
  na.omit()

midcentury.weather = retrofit.alldata.highprop %>%
  distinct(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit) %>%
  dplyr::left_join(cmip5.bin.period, by=c("BLDGNUM")) %>%
  dplyr::filter(period == "2050Jan through 2059Jan") %>%
  dplyr::select(-Latitude, -Longitude) %>%
  dplyr::group_by(period, scenario, model) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
  dplyr::ungroup() %>%
  tidyr::gather(temperature.bin, days, `<10`:`>90`) %>%
  dplyr::mutate(temperature.bin = factor(temperature.bin, levels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90"))) %>%
  dplyr::left_join(model.weights, by="model") %>%
  dplyr::group_by(period, scenario, temperature.bin) %>%
  dplyr::summarise(days = weighted.mean(days, w=Combined)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = ifelse(scenario == "rcp45", "2050-2059 (RCP4.5)", "2050-2059 (RCP8.5)")) %>%
  dplyr::select(label, temperature.bin, days) %>%
  {.}

## 3 bars per bin
now.weather %>%
  dplyr::bind_rows(midcentury.weather) %>%
  dplyr::mutate(label = factor(label, levels = c("2007-2018", "2050-2059 (RCP4.5)", "2050-2059 (RCP8.5)"))) %>%
  dplyr::mutate(temperature.bin = factor(temperature.bin, levels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=temperature.bin, y=days, fill=label)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::scale_fill_brewer(name="scenario", palette = "YlOrRd") +
  ggplot2::ylab("Avg. No. Days in Each Temp. Bin") +
  ggplot2::xlab("Temperature Bin (°F)") +
  ggplot2::ggtitle("Days in Temperature Bin 2007-2018 vs 2050-2059") +
  ggplot2::theme(legend.position="bottom")
ggsave(sprintf("%s/now_mid_century_bin_bar.png", imagedir), width=6, height=4)

## 2 bars per bin
now.weather %>%
  dplyr::bind_rows(midcentury.weather) %>%
  dplyr::group_by(temperature.bin) %>%
  dplyr::mutate(days = days - days[label == "2007-2018"]) %>%
  dplyr::ungroup() %>%
  dplyr::filter(label != "2007-2018") %>%
  dplyr::mutate(temperature.bin = factor(temperature.bin, levels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=temperature.bin, y=days, fill=label)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::scale_fill_manual(name="scenario", values= c("#FEB24C", "#F03B20")) +
  ggplot2::ylab("Avg. No. Days in Each Temp. Bin ") +
  ggplot2::xlab("Temperature Bin (°F)") +
  ggplot2::ggtitle("Difference in Days in Temperature Bin since 2007-2018") +
  ggplot2::theme(legend.position="bottom")
ggsave(sprintf("%s/mid_century_bin_bar_relative.png", imagedir), width=6, height=4)

load("../data/retrofit_prev_actions.rda")

## pre-retrofit actions
prev.action.dfs = retrofit_prev_actions %>%
  dplyr::filter(`Building_Number` %in% unique(retrofit.alldata$BLDGNUM)) %>%
  dplyr::group_by(target.action) %>%
  dplyr::group_split() %>%
  {.}

result = lapply(prev.action.dfs, function(df.plot) {
  action = df.plot$target.action[[1]]
  result.action = df.plot %>%
    dplyr::select(-target.date) %>%
    dplyr::select(-!!rlang::sym(action)) %>%
    tidyr::gather(prev.action, value, -`Building_Number`, -target.action) %>%
    dplyr::group_by(prev.action, target.action) %>%
    dplyr::summarise(n = sum(value)) %>%
    dplyr::ungroup() %>%
    {.}
  return(result.action)
})

result %>%
  dplyr::bind_rows() %>%
  dplyr::mutate_at(dplyr::vars(prev.action, target.action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  ggplot2::ggplot(ggplot2::aes(x=prev.action, y=n, fill=prev.action)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::scale_fill_brewer(name="previous action", palette = "Set3") +
  ggplot2::facet_wrap(.~target.action) +
  ggplot2::xlab("previous action") +
  ggplot2::ylab("building count") +
  ggplot2::theme(axis.text.x = element_text(angle = 90))
ggplot2::ggsave(sprintf("%s/retrofit_pre_action.png", imagedir), width=7, height=4)

## climate
load("../data/cmip5.bin.period.rda")

## for paper
retrofit.alldata %>%
  distinct(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit) %>%
  dplyr::left_join(cmip5.bin.period, by=c("BLDGNUM")) %>%
  dplyr::select(-Latitude, -Longitude) %>%
  dplyr::group_by(period, scenario, model) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
  dplyr::ungroup() %>%
  tidyr::gather(temperature.bin, days, `<10`:`>90`) %>%
  dplyr::mutate(temperature.bin = factor(temperature.bin, levels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=temperature.bin, y=days, group=interaction(scenario, period, model), color=period)) +
  ggplot2::geom_line() +
  ggplot2::scale_color_grey(start=0.7, end=0.1) +
  ggplot2::xlab("Temperature bin") +
  ggplot2::ggtitle("Temperature distribution different scenario and period") +
  ggplot2::facet_grid(model~scenario) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 90),
                 strip.text.y = element_text(angle = 0))
ggplot2::ggsave(sprintf("%s/retrofit_climate_scenario.png", imagedir), width=6, height=9)

## for slide, each model and rcp is plotted in one facet
for (scenario.to.plot in c("rcp45", "rcp85")) {
  retrofit.alldata.highprop %>%
    distinct(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit) %>%
    dplyr::left_join(cmip5.bin.period, by=c("BLDGNUM")) %>%
    dplyr::filter(period != "2090Jan through 2099Jan") %>%
    dplyr::select(-Latitude, -Longitude) %>%
    dplyr::group_by(period, scenario, model) %>%
    dplyr::summarise_if(is.numeric, mean) %>%
    dplyr::ungroup() %>%
    tidyr::gather(temperature.bin, days, `<10`:`>90`) %>%
    dplyr::mutate(temperature.bin = factor(temperature.bin, levels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90"))) %>%
    dplyr::filter(scenario==scenario.to.plot) %>%
    ggplot2::ggplot(ggplot2::aes(x=temperature.bin, y=days, group=interaction(scenario, period, model), color=period)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_grey(start=0.7, end=0.1) +
    ggplot2::xlab("Temperature bin") +
    ggplot2::ggtitle(sprintf("Annual average days with temperature in certain range under %s",
                             toupper(scenario.to.plot))) +
    ggplot2::facet_wrap(.~model, nrow=3) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 90), legend.position="bottom")
  ggplot2::ggsave(sprintf("%s/retrofit_climate_scenario_%s.png", imagedir, scenario.to.plot), width=8, height=4)
}

## for slide, all models are plotted in one plot
scenario.to.plot = "rcp45"
retrofit.alldata.highprop %>%
  distinct(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit) %>%
  dplyr::left_join(cmip5.bin.period, by=c("BLDGNUM")) %>%
  dplyr::filter(period != "2090Jan through 2099Jan") %>%
  dplyr::select(-Latitude, -Longitude) %>%
  dplyr::group_by(period, scenario, model) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
  dplyr::ungroup() %>%
  tidyr::gather(temperature.bin, days, `<10`:`>90`) %>%
  dplyr::mutate(temperature.bin = factor(temperature.bin, levels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90"))) %>%
  dplyr::filter(scenario==scenario.to.plot) %>%
  ggplot2::ggplot(ggplot2::aes(x=temperature.bin, y=days, group=interaction(scenario, period, model), color=period)) +
  ggplot2::geom_line() +
  ggplot2::scale_color_grey(start=0.7, end=0.1) +
  ggplot2::xlab("Temperature bin") +
  ggplot2::ggtitle(sprintf("Annual average days with temperature in certain range under %s",
                            toupper(scenario.to.plot))) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 90), legend.position="bottom",
                 plot.title = element_text(size = 10))
ggplot2::ggsave(sprintf("%s/retrofit_climate_scenario_%s_nofacet.png", imagedir, scenario.to.plot), width=6, height=4)

## plot regression result causal forest
cf.result = readr::read_csv(sprintf("%s/grf_result.csv", tabledir))

cf.result.significant = cf.result %>%
  dplyr::mutate(ci.low = predictions - variance.estimates,
                ci.high = predictions + variance.estimates) %>%
  dplyr::filter(sign(ci.low) == sign(ci.high)) %>%
  {.}

cf.result %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit, Substantial_Completion_Date, action) %>%
  dplyr::filter(action == "GSALink") %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  {.}

## plot savings against sqft
cf.result %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::filter(action=="HVAC") %>%
  dplyr::group_by(BLDGNUM, Substantial_Completion_Date, is.real.retrofit, GROSSSQFT) %>%
  dplyr::summarise(predictions = sum(predictions)) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x=GROSSSQFT, y=predictions)) +
  ggplot2::geom_smooth() +
  ggplot2::geom_point() +
  ggplot2::theme()

df.label = cf.result %>%
  dplyr::filter(is.real.retrofit==1) %>%
  dplyr::group_by(action, fuel) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::mutate(action.label = paste0("n=", n)) %>%
  {.}

## annual
cf.result %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                  "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         "With retrofit",
                                         "Without retrofit")) %>%
  dplyr::filter(`retrofit.label`=="With retrofit") %>%
    ## ggplot2::ggplot(ggplot2::aes(x=predictions, fill=retrofit.label,
    ##                              group=interaction(retrofit.label, action, fuel))) +
  ## annual
  dplyr::mutate(predictions = (-1)*predictions * 12) %>%
  ggplot2::ggplot(ggplot2::aes(x=predictions, group=interaction(action, fuel))) +
  ggplot2::geom_density(alpha=0.3, fill="grey") +
  ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
  ggplot2::geom_rug() +
  ggplot2::facet_grid(fuel ~ action) +
  ggplot2::scale_fill_brewer(name = "Whether retrofitted", palette = "Purples") +
  ggplot2::geom_text(size=3, data = df.label,
                     mapping=ggplot2::aes(x = -Inf, y = Inf, label=action.label),
                     hjust = -0.2, vjust = 1.5) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Distribution of treatment effect for the retrofitted") +
  ggplot2::xlab("Estimated retrofit effect (kBtu/sqft/year)") +
  ggplot2::ylab("Probability density") +
  ggplot2::coord_flip() +
  ggplot2::theme(legend.position="bottom",
                 axis.text.x = element_text(size=6),
                 strip.text.x = element_text(size = 7))
ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_treated_slides.png", imagedir), width=8, height=4)

## in document
cf.result %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                  "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         "With retrofit",
                                         "Without retrofit")) %>%
  dplyr::filter(`retrofit.label`=="With retrofit") %>%
    ## ggplot2::ggplot(ggplot2::aes(x=predictions, fill=retrofit.label,
    ##                              group=interaction(retrofit.label, action, fuel))) +
  ggplot2::ggplot(ggplot2::aes(x=predictions, group=interaction(action, fuel))) +
  ggplot2::geom_density(alpha=0.5) +
  ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
  ggplot2::geom_rug() +
  ggplot2::facet_grid(fuel ~ action) +
  ggplot2::scale_fill_brewer(name = "Whether retrofitted", palette = "Purples") +
  ggplot2::geom_text(size=3, data = df.label,
                     mapping=ggplot2::aes(x = -Inf, y = Inf, label=action.label),
                     hjust = -0.2, vjust = 1.5) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Distribution of treatment effect for the retrofitted") +
  ggplot2::xlab("Estimated retrofit treatment effect") +
  ggplot2::ylab("Probability density") +
  ggplot2::coord_flip() +
  ggplot2::theme(legend.position="bottom",
                 axis.text.x = element_text(size=6),
                 strip.text.x = element_text(size = 7))
ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_treated.png", imagedir), width=7, height=4)

target.action = "HVAC"
cf.result %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         "With retrofit",
                                         "Without retrofit")) %>%
  dplyr::filter(`retrofit.label`=="With retrofit") %>%
  dplyr::filter(action == target.action) %>%
  dplyr::mutate(positive.saving = (predictions < 0)) %>%
  dplyr::group_by(positive.saving) %>%
  {.}

options(tibble.width=NULL)

## in slides
## annual style
cf.result %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                  "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         "With retrofit",
                                         "Without retrofit")) %>%
  dplyr::filter(`retrofit.label`=="With retrofit") %>%
  dplyr::filter(action == target.action) %>%
  ## annual
  dplyr::mutate(predictions = -1 * predictions * 12) %>%
  ggplot2::ggplot(ggplot2::aes(x=predictions, group=interaction(action, fuel))) +
  ggplot2::geom_density(alpha=0.3, fill="grey") +
  ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
  ggplot2::facet_grid(fuel ~ action) +
  ggplot2::geom_rug() +
  ggplot2::geom_text(size=3, data = df.label %>% dplyr::filter(action == target.action),
                     mapping=ggplot2::aes(x = Inf, y = Inf, label=action.label),
                     hjust = 1.5, vjust = 1.5) +
  ## hjust = -0.2, vjust = 1.5) +
  ggplot2::theme_bw() +
  ## to restrict to the same coord range as the other plot
  ggplot2::coord_flip(ylim=c(0, 0.95), xlim=c(-3.4, 6.56)) +
  ggplot2::ggtitle("Distribution of treatment effect for the retrofitted") +
  ggplot2::xlab("Estimated retrofit effect (kBtu/sqft/year)") +
  ggplot2::ylab("Probability density") +
  ggplot2::theme(legend.position="bottom",
                 axis.text.x = element_text(size=8),
                 strip.text.x = element_text(size = 7))
ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_treated_%s_slides.png", imagedir, target.action), width=2.35, height=4)

## in document
target.action = "HVAC"
cf.result %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                  "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         "With retrofit",
                                         "Without retrofit")) %>%
  dplyr::filter(`retrofit.label`=="With retrofit") %>%
  dplyr::filter(action == target.action) %>%
  ggplot2::ggplot(ggplot2::aes(x=predictions, group=interaction(action, fuel))) +
  ggplot2::geom_density(alpha=0.5) +
  ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
  ggplot2::facet_grid(fuel ~ action) +
  ggplot2::scale_fill_brewer(name = "Whether retrofitted", palette = "Purples") +
  ggplot2::geom_text(size=3, data = df.label %>% dplyr::filter(action == target.action),
                     mapping=ggplot2::aes(x = -Inf, y = Inf, label=action.label),
                     hjust = -0.2, vjust = 1.5) +
  ggplot2::theme_bw() +
  ggplot2::coord_cartesian(ylim=c(0, 11)) +
  ggplot2::ggtitle("Distribution of treatment effect for the retrofitted") +
  ggplot2::xlab("Estimated retrofit treatment effect") +
  ggplot2::ylab("Probability density") +
  ggplot2::theme(legend.position="bottom",
                 axis.text.x = element_text(size=8),
                 strip.text.x = element_text(size = 7))
ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_treated_%s.png", imagedir, target.action), width=2.35, height=4)

## annual
cf.result %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         "With retrofit",
                                         "Without retrofit")) %>%
  dplyr::filter(`retrofit.label`=="With retrofit") %>%
  dplyr::filter(action == target.action) %>%
  ## sum up all fuels
  ## dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit, action) %>%
  ## dplyr::summarise(predictions = sum(predictions) * 12) %>%
  ## dplyr::ungroup() %>%
  ## annual
  dplyr::mutate(predictions = (-1) * predictions * 12) %>%
  dplyr::group_by(is.real.retrofit, fuel) %>%
  dplyr::summarise_at(vars(predictions), tibble::lst(min, median, mean, max)) %>%
  dplyr::ungroup() %>%
  {.}

dfs = cf.result.significant %>%
  ## dfs = cf.result %>%
  dplyr::select(-debiased.error, -excess.error) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         "With retrofit",
                                         "Without retrofit")) %>%
  dplyr::filter(`retrofit.label`=="With retrofit") %>%
  dplyr::mutate(reducer = (predictions < 0)) %>%
  dplyr::group_by(action, fuel) %>%
  ## remove the all T or all F case
  dplyr::filter(5 < sum(reducer), 5 < sum(1 - reducer)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(action, fuel) %>%
  dplyr::group_split() %>%
  {.}

## check counts of reducer and non-reducer
counts.reducer.nonreducer = cf.result.significant %>%
  ## counts.reducer.nonreducer = cf.result %>%
  dplyr::select(-debiased.error, -excess.error) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         "With retrofit",
                                         "Without retrofit")) %>%
  dplyr::filter(`retrofit.label`=="With retrofit") %>%
  dplyr::mutate(reducer = (predictions < 0)) %>%
  dplyr::group_by(action, fuel) %>%
  dplyr::summarise(num.reducer = sum(reducer), num.non.reducer = sum(1 - reducer)) %>%
  dplyr::ungroup() %>%
  ## dplyr::mutate(cnt.label = paste0("reducer: n=", num.reducer, "\nnon-reducer: n=", num.non.reducer)) %>%
  {.}

numeric.vars = c("GROSSSQFT", "cldd", "htdd", c("<10",
                                                sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)),
                                                ">90"), "CHILLWTR", "GAS",
                 "KWHR", "STEAM")

options(tibble.width=NULL)

outer.result = lapply(dfs, function(df.fuel.action) {
  result = lapply(numeric.vars, function(v) {
    tresult <- t.test(as.formula(sprintf("`%s` ~ reducer", v)), data = df.fuel.action, alternative = "two.sided")
    normalizer = sd(df.fuel.action[[v]])
    mu0 <- tresult$estimate[[1]]
    mu1 <- tresult$estimate[[2]]
    dif <- mu1 - mu0
    p <- tresult$p.value
    p.with.sig <- ifelse(p < 0.05, ifelse(p >= 0.01, sprintf("%.2f*", p), sprintf("%.2f**", p)), sprintf("%.2f", p))
    ci.low <- tresult$conf.int[[1]]
    ci.high <- tresult$conf.int[[2]]
    se <- tresult$stderr
    var.label = ifelse(stringr::str_detect(v, "[0-9]"), paste0("$", v, "$"), v)
    return(list("variable" = var.label, "mu0" = mu0, "mu1" = mu1, "dif" = dif, "p" = p, "p.with.sig" = p.with.sig, "ci.low"=ci.low, "ci.high"=ci.high, "normalizer"=normalizer, "se"=se))
  })
  action <- df.fuel.action$action[[1]]
  fuel <- df.fuel.action$fuel[[1]]
  print(sprintf("%s %s", action, fuel))
  df = dplyr::bind_rows(result) %>%
    dplyr::mutate(fuel = fuel, action=action)
})

## difference in each covariate
dfs = outer.result %>%
  dplyr::bind_rows() %>%
  dplyr::left_join(counts.reducer.nonreducer, by=c("action", "fuel")) %>%
  na.omit() %>%
  dplyr::group_by(action, fuel) %>%
  dplyr::group_split()

lapply(dfs, function(df.plot) {
  fuel.type = df.plot$fuel[[1]]
  action.type = df.plot$action[[1]]
  num.reducer = df.plot$num.reducer[[1]]
  num.non.reducer = df.plot$num.non.reducer[[1]]
  print(sprintf("%s, %s", action.type, fuel.type))
  df.plot %>%
    dplyr::filter(action == action.type, fuel==fuel.type) %>%
    dplyr::mutate(dif.norm = dif / normalizer, se.norm = se / normalizer) %>%
    tibble::rowid_to_column("ordering") %>%
    ggplot2::ggplot(ggplot2::aes(x=reorder(variable, -ordering), y=dif.norm)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=dif.norm - se.norm, ymax=dif.norm + se.norm), width=.2,) +
    ggplot2::coord_flip() +
    ggplot2::labs(title=sprintf("%s Retrofit %s", action.type, fuel.type),
                  subtitle=sprintf("Reducer (n=%d) vs Non-Reducer (n=%d)", num.reducer, num.non.reducer)) +
    ggplot2::xlab("variable") +
    ggplot2::ylab("mean of reducer minus mean of non-reducer") +
    ggplot2::theme(axis.text.x = element_text(angle = 90, size=5),
                   plot.title = element_text(size=12),
                   axis.title.x = element_text(size=8))
  ggplot2::ggsave(sprintf("%s/%s_%s_reducer_v_non.png", imagedir, action.type, fuel.type), width=4, height=4)
})

cf.var.importance = readr::read_csv(sprintf("%s/grf_var_importance.csv", tabledir))

## check building counts of one action
## cf.result %>%
##   dplyr::filter(action == "Advanced Metering", fuel=="KWHR") %>%
##   dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
##   dplyr::group_by(is.real.retrofit) %>%
##   dplyr::count() %>%
##   dplyr::ungroup() %>%
##   {.}

counts.action.fuel = cf.result %>%
  dplyr::select(-debiased.error, -excess.error) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::mutate(`retrofit.label`= ifelse(is.real.retrofit,
                                         "With retrofit",
                                         "Without retrofit")) %>%
  dplyr::group_by(action, fuel, retrofit.label) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  tidyr::spread(retrofit.label, n) %>%
  dplyr::mutate(action.fuel.label = paste0("With retrofit (n=", `With retrofit`, ")", ", without retrofit (n=", `Without retrofit`, ")")) %>%
  {.}

dfs = cf.var.importance %>%
  dplyr::mutate_at(dplyr::vars(action, variable), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel, variable), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::left_join(counts.action.fuel, by=c("action", "fuel")) %>%
  dplyr::group_by(action, fuel) %>%
  dplyr::group_split()

lapply(dfs, function(df.group) {
  current.action = df.group$action[[1]]
  current.fuel = df.group$fuel[[1]]
  df.group %>%
    dplyr::mutate(`variable.type`= dplyr::case_when(variable %in% c("GROSSSQFT", "historic", "with.leed", "is.office") ~ "static",
                                                    variable %in% c("CDD", "HDD", "<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90") ~ "weather",
                                                    variable %in% c("cldd", "htdd") ~ "climate",
                                                    variable %in% c("Gas", "Electricity", "STEAM", "CHILLWTR") ~ "pre-retrofit energy",
                                                    stringr::str_detect(variable, "Region") ~ "region",
                                                    TRUE ~ "previous action")) %>%
    dplyr::arrange(action, fuel, variable.type, variable) %>%
    ggplot2::ggplot(ggplot2::aes(x=reorder(variable, -importance), y=importance, fill=variable.type)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::labs(title = sprintf("Variable importance: %s retrofit on %s saving",
                                  current.action, current.fuel),
                  subtitle = df.group$action.fuel.label[[1]]) +
    ggplot2::xlab("variable") +
    ggplot2::scale_fill_brewer(name = "variable type", palette = "Set3") +
    ggplot2::theme(axis.text.x = element_text(angle = 90, size=5), legend.position="bottom")
  ggplot2::ggsave(sprintf("%s/var_importance_cf_%s_%s.png", imagedir,
                          paste(current.action, sep="_"), current.fuel), width=7, height=4)
  return(NULL)
})

## according to https://portfoliomanager.zendesk.com/hc/en-us/articles/216670148-What-are-the-Site-to-Source-Conversion-Factors-
mult.to.source = tibble::tibble(fuel = c("Electricity", "Gas", "STEAM", "CHILLWTR"),
                                mult=c(2.8, 1.05, 1.2, 0.91))
## according to https://www.epa.gov/energy/greenhouse-gases-equivalencies-calculator-calculations-and-references
mult.to.co2 = tibble::tibble(fuel = c("Electricity", "Gas"),
                             mult=c(1/3.412*7.07e-4, 1/1.031/1000*0.0549))

## social cost of carbon: 38 dollar per ton, from "Developing a Social Cost of Carbon for US Regulatory Analysis: A Methodology and Interpretation"
## social marginal cost, electricity mult is according to notes in Fig. 10 in (Knittel and Stopler, 2019)
## mult.to.smc = tibble::tibble(variable = c(variable = c("Electricity", "Gas")),
##                             mult = c(1/3412.14*0.21*0.065, ))

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
  dplyr::mutate(fuel="Gas")
  {.}

util.rate = electric.rate %>%
  dplyr::bind_rows(gas.rate)

## check which has different rank orders, none found
cf.result %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::select(BLDGNUM, Substantial_Completion_Date, is.real.retrofit, action, fuel, predictions) %>%
  dplyr::left_join(mult.to.source, by="fuel") %>%
  dplyr::mutate(predictions.source = predictions * mult) %>%
  dplyr::select(-mult) %>%
  ## get tons of co2
  dplyr::left_join(mult.to.co2, by="fuel") %>%
  dplyr::mutate(predictions.co2 = predictions * mult) %>%
  dplyr::select(-mult) %>%
  dplyr::group_by(BLDGNUM, action) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::ungroup() %>%
  ## dplyr::filter(action != "GSALink") %>%
  dplyr::group_by(BLDGNUM) %>%
  ## best retrofit is the same
  dplyr::filter(action[which(predictions==min(predictions))] != action[which(predictions.co2==min(predictions.co2))]) %>%
  ## rankings of retrofits
  ## dplyr::mutate(rank.site = rank(predictions), rank.source = rank(predictions.source)) %>%
  ## dplyr::filter(sum(rank.site != rank.source) > 3) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(BLDGNUM) %>%
  {.}

options(tibble.width=Inf)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## individual building optimal treatment
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
target.building = "CA0053ZZ"
to.plot = cf.result %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning",
                   "Advanced Metering"="Advanced\nMetering",
                   "Building Envelope"="Building\nEnvelope") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::filter(BLDGNUM == target.building) %>%
  dplyr::select(BLDGNUM, predictions, action, fuel) %>%
  ## not analyze for now, as the pre-retrofit period might be different from the
  ## other ones, no need to block out two years
  ## dplyr::filter(action != "GSALink") %>%
  dplyr::left_join(mult.to.source, by="fuel") %>%
  dplyr::mutate(predictions.source = predictions * mult) %>%
  dplyr::select(-mult) %>%
  ## get tons of co2
  dplyr::left_join(mult.to.co2, by="fuel") %>%
  dplyr::mutate(predictions.co2 = predictions * mult) %>%
  dplyr::select(-mult) %>%
  dplyr::mutate(predictions.scc = predictions.co2 * 38) %>%
  dplyr::group_by(BLDGNUM, action) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::ungroup() %>%
  {.}

to.plot %>%
  ggplot2::ggplot(ggplot2::aes(x=action, y=predictions)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::ggtitle("Effect on site electricity + gas") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_building_effect_site.png", imagedir), width=6, height=3)

to.plot %>%
  ggplot2::ggplot(ggplot2::aes(x=action, y=predictions.source)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::ggtitle("Effect on source electricity + gas") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_building_effect_source.png", imagedir), width=6, height=3)

to.plot %>%
  ggplot2::ggplot(ggplot2::aes(x=action, y=predictions.co2)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::ylab("Tons/sqft") +
  ggplot2::ggtitle("Effect on CO2 emissions") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_building_effect_co2.png", imagedir), width=6, height=3)

to.plot %>%
  ggplot2::ggplot(ggplot2::aes(x=action, y=predictions.scc)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::ylab("Dollar/sqft") +
  ggplot2::ggtitle("Effect on social cost of carbon") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_building_effect_scc.png", imagedir), width=6, height=3)

## for climate change
to.plot.climate = under.climate.change %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning",
                   "Advanced Metering"="Advanced\nMetering",
                   "Building Envelope"="Building\nEnvelope") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::filter(BLDGNUM == target.building) %>%
  dplyr::select(BLDGNUM, predictions, action, fuel) %>%
  ## not analyze for now, as the pre-retrofit period might be different from the
  ## other ones, no need to block out two years
  ## dplyr::filter(action != "GSALink") %>%
  dplyr::left_join(mult.to.source, by="fuel") %>%
  dplyr::mutate(predictions.source = predictions * mult) %>%
  dplyr::select(-mult) %>%
  ## get tons of co2
  dplyr::left_join(mult.to.co2, by="fuel") %>%
  dplyr::mutate(predictions.co2 = predictions * mult) %>%
  dplyr::select(-mult) %>%
  dplyr::mutate(predictions.scc = predictions.co2 * 38) %>%
  dplyr::group_by(BLDGNUM, action) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::ungroup() %>%
  {.}

to.plot.climate %>%
  ggplot2::ggplot(ggplot2::aes(x=action, y=predictions)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::ggtitle("Effect on site electricity + gas with mid-century climate") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_building_effect_site_future.png", imagedir), width=6, height=3)

to.plot.climate %>%
  ggplot2::ggplot(ggplot2::aes(x=action, y=predictions.source)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::ggtitle("Effect on source electricity + gas with mid-century climate") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_building_effect_source_future.png", imagedir), width=6, height=3)

to.plot.climate %>%
  ggplot2::ggplot(ggplot2::aes(x=action, y=predictions.co2)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::ylab("Tons/sqft") +
  ggplot2::ggtitle("Effect on CO2 emissions with mid-century climate") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_building_effect_co2_future.png", imagedir), width=6, height=3)

to.plot.climate %>%
  ggplot2::ggplot(ggplot2::aes(x=action, y=predictions.scc)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::ylab("Dollar/sqft") +
  ggplot2::ggtitle("Effect on social cost of carbon with mid-century climate") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_building_effect_scc_future.png", imagedir), width=6, height=3)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## policy optimal treatment
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
to.plot = cf.result %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::select(BLDGNUM, predictions, action, fuel) %>%
  ## not analyze for now, as the pre-retrofit period might be different from the
  ## other ones, no need to block out two years
  dplyr::left_join(mult.to.source, by="fuel") %>%
  dplyr::mutate(predictions.source = predictions * mult) %>%
  dplyr::select(-mult) %>%
  ## get tons of co2
  dplyr::left_join(mult.to.co2, by="fuel") %>%
  dplyr::mutate(predictions.co2 = predictions * mult) %>%
  dplyr::select(-mult) %>%
  dplyr::mutate(predictions.scc = predictions.co2 * 38) %>%
  dplyr::filter(action == "HVAC") %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::ungroup() %>%
  {.}

to.plot %>%
  dplyr::arrange(predictions) %>%
  dplyr::slice(1:30) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(BLDGNUM, -predictions), y=predictions)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by site electricity + gas") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_action_many_building_site_elecgas.png", imagedir), width=4, height=7)

to.plot %>%
  dplyr::arrange(predictions.source) %>%
  dplyr::slice(1:30) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(BLDGNUM, -predictions.source), y=predictions.source)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by source electricity + gas") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_action_many_building_source_elecgas.png", imagedir), width=4, height=7)

to.plot %>%
  dplyr::arrange(predictions.co2) %>%
  dplyr::slice(1:30) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(BLDGNUM, -predictions.co2), y=predictions.co2)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building") +
  ggplot2::ylab("Tons/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by CO2 of electricity and gas") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_action_many_building_co2_elecgas.png", imagedir), width=4, height=7)

to.plot %>%
  dplyr::arrange(predictions.scc) %>%
  dplyr::slice(1:30) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(BLDGNUM, -predictions.scc), y=predictions.scc)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building") +
  ggplot2::ylab("Dollar/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by SCC for total electricity and gas") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_action_many_building_scc_elecgas.png", imagedir), width=4, height=7)

## for climate change
to.plot.climate = under.climate.change %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::select(BLDGNUM, predictions, action, fuel) %>%
  ## not analyze for now, as the pre-retrofit period might be different from the
  ## other ones, no need to block out two years
  dplyr::left_join(mult.to.source, by="fuel") %>%
  dplyr::mutate(predictions.source = predictions * mult) %>%
  dplyr::select(-mult) %>%
  ## get tons of co2
  dplyr::left_join(mult.to.co2, by="fuel") %>%
  dplyr::mutate(predictions.co2 = predictions * mult) %>%
  dplyr::select(-mult) %>%
  dplyr::mutate(predictions.scc = predictions.co2 * 38) %>%
  dplyr::filter(action == "HVAC") %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::ungroup() %>%
  {.}

to.plot.climate %>%
  dplyr::arrange(predictions) %>%
  dplyr::slice(1:30) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(BLDGNUM, -predictions), y=predictions)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by site electricity + gas \nwith mid-century climate") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_action_many_building_site_elecgas_future.png", imagedir), width=4, height=7)

to.plot.climate %>%
  dplyr::arrange(predictions.source) %>%
  dplyr::slice(1:30) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(BLDGNUM, -predictions.source), y=predictions.source)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by source electricity + gas \nwith mid-century climate") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_action_many_building_source_elecgas_future.png", imagedir), width=4, height=7)

to.plot.climate %>%
  dplyr::arrange(predictions.co2) %>%
  dplyr::slice(1:30) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(BLDGNUM, -predictions.co2), y=predictions.co2)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building") +
  ggplot2::ylab("Tons/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by CO2 of electricity and gas \nwith mid-century climate") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_action_many_building_co2_elecgas_future.png", imagedir), width=4, height=7)

to.plot.climate %>%
  dplyr::arrange(predictions.scc) %>%
  dplyr::slice(1:30) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(BLDGNUM, -predictions.scc), y=predictions.scc)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building") +
  ggplot2::ylab("Dollar/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by SCC for total electricity and gas \nwith mid-century climate") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/one_action_many_building_scc_elecgas_future.png", imagedir), width=4, height=7)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## portfolio owner ranking
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
static.info = cf.result %>%
  distinct(BLDGNUM, historic, GROSSSQFT) %>%
  {.}

building.state = building_location %>%
  dplyr::distinct(BLDGNUM, State) %>%
  {.}

to.plot = cf.result %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::select(BLDGNUM, predictions, action, fuel) %>%
  dplyr::left_join(building.state, by="BLDGNUM") %>%
  ## not analyze for now, as the pre-retrofit period might be different from the
  ## other ones, no need to block out two years
  ## dplyr::filter(action != "GSALink") %>%
  dplyr::left_join(util.rate, by=c("State"="abbr", "fuel"="fuel")) %>%
  dplyr::mutate(predictions.dollar = predictions * dollar.kbtu) %>%
  dplyr::left_join(mult.to.source, by="fuel") %>%
  dplyr::mutate(predictions.source = predictions * mult) %>%
  dplyr::select(-mult) %>%
  ## get tons of co2
  dplyr::left_join(mult.to.co2, by="fuel") %>%
  dplyr::mutate(predictions.co2 = predictions * mult) %>%
  dplyr::select(-mult) %>%
  ## dplyr::mutate(predictions.scc = predictions.co2 * 38) %>%
  dplyr::group_by(BLDGNUM, action) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(static.info, by="BLDGNUM") %>%
  {.}

action.cost.per.sqft = readr::read_csv("action_cost.csv") %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::rename(imp.cost.per.sqft = Estimate) %>%
  {.}

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

retroed.building.action.lifespan = retrofit_from_db %>%
  dplyr::distinct(`Building_Number`, `Substantial_Completion_Date`, `high_level_ECM`, `detail_level_ECM`) %>%
  tidyr::separate(detail_level_ECM, c("pref", "suf"), "_") %>%
  dplyr::mutate_at(dplyr::vars(high_level_ECM), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(vars(suf), recode, "Indoor Daylighting or Lighting Strategies"="Daylighting",
                   "Retrofit or Replacement"="LED fixture",
                   "Commissioning Measures"="Commissioning") %>%
  dplyr::left_join(action.lifespan, by=c("high_level_ECM"="Category", "suf"="Sub-category")) %>%
  dplyr::group_by(`Building_Number`, `Substantial_Completion_Date`, `high_level_ECM`) %>%
  dplyr::summarise(lifespan.year = mean(`Lifespan (years)`)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-`Substantial_Completion_Date`) %>%
  {.}

high.level.ecm.lifespan = retroed.building.action.lifespan %>%
  dplyr::group_by(high_level_ECM) %>%
  dplyr::summarise(lifespan.year = mean(lifespan.year)) %>%
  {.}

options(tibble.width=Inf)

get.irr <- function(initial.cost, ann.save, life) {
  cf = crep(ann.save, as.integer(round(life, 0))) * (-1)
  cf[[1]] <- cf[[1]] - initial.cost
  return(jrvFinance::irr(cf))
}

v.get.irr <- Vectorize(get.irr)

v.get.irr(c(8014941, 18361105), c(65178, -119139), c(6, 30.6))

## get.irr(8014941, 65178, 6)
## get.irr(18361105, -119139, 30.6)

## ROI targeting (positive ROI)
building.action.roi.now = to.plot %>%
  tidyr::gather(variable, value.per.sqft, starts_with("predictions")) %>%
  dplyr::filter(variable == "predictions") %>%
  dplyr::mutate(value = value.per.sqft * GROSSSQFT) %>%
  dplyr::left_join(action.cost.per.sqft, by=c("action", "historic")) %>%
  dplyr::mutate(implement.cost = imp.cost.per.sqft * GROSSSQFT) %>%
  dplyr::select(-GROSSSQFT, -imp.cost.per.sqft) %>%
  dplyr::left_join(high.level.ecm.lifespan, by=c("action"="high_level_ECM")) %>%
  dplyr::mutate(implement.cost = ifelse(action == "GSALink", (48000 + 155000) / 2, implement.cost)) %>%
  dplyr::mutate(annual.value = value * 12) %>%
  dplyr::mutate(ROI = ((-1) * annual.value * lifespan.year - implement.cost) / implement.cost) %>%
  dplyr::select(BLDGNUM, action, implement.cost, lifespan.year, annual.value, ROI) %>%
  {.}

all.no.targeting.roi = building.action.roi.now %>%
  dplyr::mutate(label = "No targeting") %>%
  {.}

all.with.targeting.roi = building.action.roi.now %>%
  dplyr::filter(ROI > 0) %>%
  dplyr::mutate(label = "With targeting") %>%
  {.}

all.no.targeting.roi %>%
  dplyr::bind_rows(all.with.targeting.roi) %>%
  dplyr::group_by(label) %>%
  dplyr::summarise(ROI = sum((-1) * annual.value * lifespan.year - implement.cost) / sum(implement.cost)) %>%
  dplyr::mutate(variable="ROI") %>%
  {.}

all.no.targeting.roi %>%
  dplyr::bind_rows(all.with.targeting.roi) %>%
  dplyr::group_by(label) %>%
  dplyr::summarise(ROI = sum((-1) * annual.value * lifespan.year - implement.cost) / sum(implement.cost)) %>%
  dplyr::mutate(variable="ROI") %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=ROI, fill=label)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::ggtitle("Targeting ones with positive ROI \nvs all buildings taking all 6 retrofits ") +
  ggplot2::scale_fill_grey(start=0.8, end=0.2, name="") +
  ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9)) +
  ggplot2::expand_limits(y=4.5) +
  ggplot2::scale_y_reverse() +
  ggplot2::coord_flip() +
  ggplot2::theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank())
ggplot2::ggsave(sprintf("%s/targeting_all_roi.png", imagedir), width=5, height=3)

## ROI targeting by action now
by.action.no.targeting.roi = building.action.roi.now %>%
  dplyr::group_by(action) %>%
  dplyr::summarise(ROI = sum((-1) * annual.value * lifespan.year - implement.cost) / sum(implement.cost)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "No targeting") %>%
  {.}

by.action.with.targeting.roi = building.action.roi.now %>%
  dplyr::filter(ROI > 0) %>%
  dplyr::group_by(action) %>%
  dplyr::summarise(ROI = sum((-1) * annual.value * lifespan.year - implement.cost) / sum(implement.cost)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "With targeting") %>%
  {.}

by.action.no.targeting.roi %>%
  dplyr::bind_rows(by.action.with.targeting.roi) %>%
  tidyr::spread(label, ROI) %>%
  dplyr::mutate(diff = `With targeting` - `No targeting`) %>%
  {.}

by.action.no.targeting.roi %>%
  dplyr::bind_rows(by.action.with.targeting.roi) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate(variable="ROI") %>%
  ## dplyr::filter(action != "GSALink") %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=ROI, fill=label)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::ggtitle("Targeting ones with positive ROI vs all buildings taking an action") +
  ggplot2::scale_fill_grey(start=0.8, end=0.2) +
  ggplot2::facet_wrap(.~action, ncol=2) +
  ggplot2::coord_flip(ylim=c(7, -2)) +
  ggplot2::scale_y_reverse() +
  ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9), size=3) +
  ggplot2::theme(legend.position="none", title=element_text(size=9),
                 axis.title.y=element_blank(), axis.title.x=element_blank())
ggplot2::ggsave(sprintf("%s/targeting_by_action_roi.png", imagedir), width=7, height=3)

roi.metric = to.plot %>%
  tidyr::gather(variable, value.per.sqft, starts_with("predictions")) %>%
  dplyr::filter(variable == "predictions") %>%
  dplyr::mutate(value = value.per.sqft * GROSSSQFT) %>%
  dplyr::left_join(action.cost.per.sqft, by=c("action", "historic")) %>%
  dplyr::mutate(implement.cost = imp.cost.per.sqft * GROSSSQFT) %>%
  dplyr::select(-GROSSSQFT, -imp.cost.per.sqft) %>%
  dplyr::left_join(high.level.ecm.lifespan, by=c("action"="high_level_ECM")) %>%
  dplyr::mutate(implement.cost = ifelse(action == "GSALink", (48000 + 155000) / 2, implement.cost)) %>%
  dplyr::mutate(annual.value = value * 12) %>%
  dplyr::mutate(ROI = ((-1) * annual.value * lifespan.year - implement.cost) / implement.cost) %>%
  dplyr::mutate(neg.ROI = (-1) * ROI) %>%
  dplyr::mutate(variable = "neg.ROI", value = neg.ROI) %>%
  dplyr::select(BLDGNUM, action, variable, value) %>%
  {.}

multiple.metric = to.plot %>%
  tidyr::gather(variable, value.per.sqft, starts_with("predictions")) %>%
  dplyr::mutate(value = value.per.sqft * GROSSSQFT) %>%
  dplyr::select(-historic, -GROSSSQFT, -dollar.kbtu) %>%
  dplyr::bind_rows(roi.metric) %>%
  {.}

base.vars <- c("predictions", "predictions.dollar", "predictions.source",
               "predictions.co2", "neg.ROI")

rank.cmp.value = lapply(base.vars, function(base.var) {
  df.result = multiple.metric %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(ranking = rank(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(BLDGNUM, action) %>%
    dplyr::mutate(ranking.diff = ranking - ranking[variable == base.var]) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(action, variable) %>%
    dplyr::summarise(ranking.diff = mean(ranking.diff)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(variable, ranking.diff) %>%
    {.}
})

rank.cmp.value %>%
  dplyr::bind_rows() %>%
  dplyr::select("action", "predictions", "predictions.dollar", "predictions.source", "predictions.co2", "neg.ROI") %>%
  readr::write_csv("avg_action_rank_value.csv")

rank.value = multiple.metric %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(ranking = rank(value)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(action, variable) %>%
  dplyr::summarise(ranking = mean(ranking)) %>%
  dplyr::ungroup() %>%
  {.}

rank.value %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(norm.ranking = rank(ranking)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(vars(variable), recode, "predictions"="Site\nkbtu",
                   "predictions.dollar"="Expense\ndollar", "predictions.source"="Source\nkbtu",
                   "predictions.co2"="Emissions\nTon", "neg.ROI"="ROI") %>%
  dplyr::mutate(variable=factor(variable,
                                levels=c("Site\nkbtu", "Source\nkbtu",
                                         "Expense\ndollar", "Emissions\nTon",
                                         "ROI"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=action, fill=ranking, label=round(ranking, 1))) +
  ## ggplot2::ggplot(ggplot2::aes(x=variable, y=action, fill=norm.ranking, label=round(ranking, 1))) +
  ggplot2::geom_tile(alpha=0.5) +
  ggplot2::geom_text() +
  ggplot2::xlab("Evaluation metric") +
  ggplot2::ggtitle("Total-building-based Average Ranking") +
  ggplot2::scale_fill_distiller(palette = "RdYlGn") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggplot2::ggsave(sprintf("%s/building_action_rank_tile_value.png", imagedir), width=5, height=6)

rank.value.per.sqft = multiple.metric %>%
  dplyr::filter(variable != "neg.ROI") %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(ranking = rank(value.per.sqft)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(action, variable) %>%
  dplyr::summarise(ranking = mean(ranking)) %>%
  dplyr::ungroup() %>%
  {.}

rank.value.per.sqft %>%
  dplyr::filter(variable != "neg.ROI") %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(norm.ranking = rank(ranking)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(vars(variable), recode, "predictions"="Site\nkbtu",
                   "predictions.dollar"="Expense\ndollar", "predictions.source"="Source\nkbtu",
                   "predictions.co2"="Emissions\nTon") %>%
  dplyr::mutate(variable=factor(variable,
                                levels=c("Site\nkbtu", "Source\nkbtu",
                                         "Expense\ndollar", "Emissions\nTon"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=action, fill=ranking, label=round(ranking, 1))) +
  ## ggplot2::ggplot(ggplot2::aes(x=variable, y=action, fill=norm.ranking, label=round(ranking, 1))) +
  ggplot2::geom_tile(alpha=0.5) +
  ggplot2::geom_text() +
  ggplot2::ggtitle("Per-sqft-based Average Ranking") +
  ggplot2::scale_fill_distiller(palette = "RdYlGn") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggplot2::ggsave(sprintf("%s/building_action_rank_tile_per_sqft.png", imagedir), width=4.4, height=6)

base.vars <- c("predictions", "predictions.dollar", "predictions.source",
               "predictions.co2")

rank.cmp.per.sqft = lapply(base.vars, function(base.var) {
  df.result = multiple.metric %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(ranking = rank(value.per.sqft)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(BLDGNUM, action) %>%
    dplyr::mutate(ranking.diff = ranking - ranking[variable == base.var]) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(action, variable) %>%
    dplyr::summarise(ranking.diff = mean(ranking.diff)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(variable, ranking.diff) %>%
    {.}
})

rank.cmp.per.sqft %>%
  dplyr::bind_rows() %>%
  dplyr::select("action", "predictions", "predictions.dollar", "predictions.source", "predictions.co2") %>%
  readr::write_csv("avg_action_rank_value_per_sqft.csv")

to.plot %>%
  dplyr::arrange(predictions) %>%
  dplyr::slice(1:40) %>%
  tidyr::unite("label", BLDGNUM, action) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(label, -predictions), y=predictions)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building Action Pair") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by site electricity + gas") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/many_action_many_building_site_elecgas.png", imagedir), width=4.5, height=7)

to.plot %>%
  dplyr::arrange(predictions.source) %>%
  dplyr::slice(1:40) %>%
  tidyr::unite("label", BLDGNUM, action) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(label, -predictions.source), y=predictions.source)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building Action Pair") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by source electricity + gas") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/many_action_many_building_source_elecgas.png", imagedir), width=4.5, height=7)

to.plot %>%
  dplyr::arrange(predictions.co2) %>%
  dplyr::slice(1:40) %>%
  tidyr::unite("label", BLDGNUM, action) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(label, -predictions.co2), y=predictions.co2)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building Action Pair") +
  ggplot2::ylab("Ton/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by CO2 of electricity and gas") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/many_action_many_building_co2_elecgas.png", imagedir), width=5, height=7)

## to.plot %>%
##   dplyr::arrange(predictions.scc) %>%
##   dplyr::slice(1:40) %>%
##   tidyr::unite("label", BLDGNUM, action) %>%
##   ggplot2::ggplot(ggplot2::aes(x=reorder(label, -predictions.scc), y=predictions.scc)) +
##   ggplot2::geom_bar(stat="identity") +
##   ggplot2::xlab("Building Action Pair") +
##   ggplot2::ylab("Dollar/sqft") +
##   ggplot2::ggtitle("Rank by SCC of electricity and gas") +
##   ggplot2::coord_flip() +
##   ggplot2::theme()
## ggplot2::ggsave(sprintf("%s/many_action_many_building_scc_elecgas.png", imagedir), width=4, height=7)

## for climate change
to.plot.climate = under.climate.change %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::select(BLDGNUM, predictions, action, fuel) %>%
  dplyr::left_join(building.state, by="BLDGNUM") %>%
  ## not analyze for now, as the pre-retrofit period might be different from the
  ## other ones, no need to block out two years
  ## dplyr::filter(action != "GSALink") %>%
  dplyr::left_join(util.rate, by=c("State"="abbr", "fuel"="fuel")) %>%
  dplyr::mutate(predictions.dollar = predictions * dollar.kbtu) %>%
  dplyr::left_join(mult.to.source, by="fuel") %>%
  dplyr::mutate(predictions.source = predictions * mult) %>%
  dplyr::select(-mult) %>%
  ## get tons of co2
  dplyr::left_join(mult.to.co2, by="fuel") %>%
  dplyr::mutate(predictions.co2 = predictions * mult) %>%
  dplyr::select(-mult) %>%
  ## dplyr::mutate(predictions.scc = predictions.co2 * 38) %>%
  dplyr::group_by(BLDGNUM, action) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(static.info, by="BLDGNUM") %>%
  {.}

building.action.roi.future = to.plot.climate %>%
  tidyr::gather(variable, value.per.sqft, starts_with("predictions")) %>%
  dplyr::filter(variable == "predictions") %>%
  dplyr::mutate(value = value.per.sqft * GROSSSQFT) %>%
  dplyr::left_join(action.cost.per.sqft, by=c("action", "historic")) %>%
  dplyr::mutate(implement.cost = imp.cost.per.sqft * GROSSSQFT) %>%
  dplyr::select(-GROSSSQFT, -imp.cost.per.sqft) %>%
  dplyr::left_join(high.level.ecm.lifespan, by=c("action"="high_level_ECM")) %>%
  dplyr::mutate(implement.cost = ifelse(action == "GSALink", (48000 + 155000) / 2, implement.cost)) %>%
  dplyr::mutate(annual.value = value * 12) %>%
  dplyr::mutate(ROI = ((-1) * annual.value * lifespan.year - implement.cost) / implement.cost) %>%
  dplyr::select(BLDGNUM, action, implement.cost, lifespan.year, annual.value, ROI) %>%
  {.}

all.no.targeting.roi = building.action.roi.future %>%
  dplyr::mutate(label = "No targeting") %>%
  {.}

all.with.targeting.roi = building.action.roi.future %>%
  dplyr::filter(ROI > 0) %>%
  dplyr::mutate(label = "With targeting") %>%
  {.}

all.no.targeting.roi %>%
  dplyr::bind_rows(all.with.targeting.roi) %>%
  dplyr::group_by(label) %>%
  dplyr::summarise(ROI = sum((-1) * annual.value * lifespan.year - implement.cost) / sum(implement.cost)) %>%
  dplyr::mutate(variable="ROI") %>%
  {.}

all.no.targeting.roi %>%
  dplyr::bind_rows(all.with.targeting.roi) %>%
  dplyr::group_by(label) %>%
  dplyr::summarise(ROI = sum((-1) * annual.value * lifespan.year - implement.cost) / sum(implement.cost)) %>%
  dplyr::mutate(variable="ROI") %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=ROI, fill=label)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::ggtitle("2050-2059: Targeting ones with positive ROI \nvs all buildings taking all 6 retrofits ") +
  ggplot2::scale_fill_grey(start=0.8, end=0.2, name="") +
  ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9)) +
  ggplot2::expand_limits(y=4.5) +
  ggplot2::scale_y_reverse() +
  ggplot2::coord_flip() +
  ggplot2::theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank())
ggplot2::ggsave(sprintf("%s/targeting_all_roi_future.png", imagedir), width=5, height=3)

## ROI targeting by action mid-century
by.action.no.targeting.roi = building.action.roi.future %>%
  dplyr::group_by(action) %>%
  dplyr::summarise(ROI = sum((-1) * annual.value * lifespan.year - implement.cost) / sum(implement.cost)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "No targeting") %>%
  {.}

by.action.with.targeting.roi = building.action.roi.future %>%
  dplyr::filter(ROI > 0) %>%
  dplyr::group_by(action) %>%
  dplyr::summarise(ROI = sum((-1) * annual.value * lifespan.year - implement.cost) / sum(implement.cost)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "With targeting") %>%
  {.}

by.action.no.targeting.roi %>%
  dplyr::bind_rows(by.action.with.targeting.roi) %>%
  tidyr::spread(label, ROI) %>%
  dplyr::mutate(diff = `With targeting` - `No targeting`) %>%
  {.}

by.action.no.targeting.roi %>%
  dplyr::bind_rows(by.action.with.targeting.roi) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate(variable="ROI") %>%
  ## dplyr::filter(action != "GSALink") %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=ROI, fill=label)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::ggtitle("2050-2059: Targeting ones with positive ROI vs all buildings taking an action") +
  ggplot2::scale_fill_grey(start=0.8, end=0.2) +
  ggplot2::facet_wrap(.~action, ncol=2) +
  ggplot2::coord_flip(ylim=c(7.5, -1)) +
  ggplot2::scale_y_reverse() +
  ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9), size=3) +
  ggplot2::theme(legend.position="none", title=element_text(size=9),
                 axis.title.y=element_blank(), axis.title.x=element_blank())
ggplot2::ggsave(sprintf("%s/targeting_by_action_roi_future.png", imagedir), width=7, height=3)

roi.metric.climate = to.plot.climate %>%
  tidyr::gather(variable, value.per.sqft, starts_with("predictions")) %>%
  dplyr::filter(variable == "predictions") %>%
  dplyr::mutate(value = value.per.sqft * GROSSSQFT) %>%
  dplyr::left_join(action.cost.per.sqft, by=c("action", "historic")) %>%
  dplyr::mutate(implement.cost = imp.cost.per.sqft * GROSSSQFT) %>%
  dplyr::select(-GROSSSQFT, -imp.cost.per.sqft) %>%
  dplyr::left_join(high.level.ecm.lifespan, by=c("action"="high_level_ECM")) %>%
  dplyr::mutate(implement.cost = ifelse(action == "GSALink", (48000 + 155000) / 2, implement.cost)) %>%
  dplyr::mutate(annual.value = value * 12) %>%
  dplyr::mutate(ROI = ((-1) * annual.value * lifespan.year - implement.cost) / implement.cost) %>%
  dplyr::mutate(neg.ROI = (-1) * ROI) %>%
  dplyr::mutate(variable = "neg.ROI", value = neg.ROI) %>%
  dplyr::select(BLDGNUM, action, variable, value) %>%
  {.}

multiple.metric.climate = to.plot.climate %>%
  tidyr::gather(variable, value.per.sqft, starts_with("predictions")) %>%
  dplyr::mutate(value = value.per.sqft * GROSSSQFT) %>%
  dplyr::select(-historic, -GROSSSQFT, -dollar.kbtu) %>%
  dplyr::bind_rows(roi.metric.climate) %>%
  {.}

rank.value.climate = multiple.metric.climate %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(ranking = rank(value)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(action, variable) %>%
  dplyr::summarise(ranking = mean(ranking)) %>%
  dplyr::ungroup() %>%
  {.}

rank.value <- rank.value %>%
  dplyr::mutate(period = "now")

rank.value.climate %>%
  dplyr::mutate(period = "mid-century") %>%
  dplyr::bind_rows(rank.value) %>%
  tidyr::spread(period, ranking) %>%
  dplyr::mutate(change = `mid-century`-now) %>%
  dplyr::mutate_at(vars(variable), recode, "predictions"="Site\nkbtu",
                   "predictions.dollar"="Expense\ndollar", "predictions.source"="Source\nkbtu",
                   "predictions.co2"="Emissions\nTon", "neg.ROI"="ROI") %>%
  dplyr::mutate(variable=factor(variable,
                                levels=c("Site\nkbtu", "Source\nkbtu",
                                         "Expense\ndollar", "Emissions\nTon",
                                         "ROI"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=action, fill=change, label=round(change, 1))) +
  ggplot2::geom_tile(alpha=0.7) +
  ggplot2::geom_text() +
  ggplot2::xlab("Evaluation metric") +
  ggplot2::ggtitle("2050-2059 Minus Now Total-building Ranking") +
  ggplot2::scale_fill_distiller(palette = "PRGn") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom", panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), ,
                 title=element_text(size=9))
ggplot2::ggsave(sprintf("%s/building_action_rank_tile_value_change_midcentury.png", imagedir), width=5, height=6)

rank.value.climate %>%
  dplyr::mutate_at(vars(variable), recode, "predictions"="Site\nkbtu",
                   "predictions.dollar"="Expense\ndollar", "predictions.source"="Source\nkbtu",
                   "predictions.co2"="Emissions\nTon", "neg.ROI"="ROI") %>%
  dplyr::mutate(variable=factor(variable,
                                levels=c("Site\nkbtu", "Source\nkbtu",
                                         "Expense\ndollar", "Emissions\nTon",
                                         "ROI"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=action, fill=ranking, label=round(ranking, 1))) +
  ## ggplot2::ggplot(ggplot2::aes(x=variable, y=action, fill=norm.ranking, label=round(ranking, 1))) +
  ggplot2::geom_tile(alpha=0.5) +
  ggplot2::geom_text() +
  ggplot2::xlab("Evaluation metric") +
  ggplot2::ggtitle("2050-2059: Total-building-based Average Ranking") +
  ggplot2::scale_fill_distiller(palette = "RdYlGn") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom", panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), ,
                 title=element_text(size=9))
ggplot2::ggsave(sprintf("%s/building_action_rank_tile_value_midcentury.png", imagedir), width=5, height=6)

rank.value.per.sqft.climate = multiple.metric.climate %>%
  dplyr::filter(variable != "neg.ROI") %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(ranking = rank(value.per.sqft)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(action, variable) %>%
  dplyr::summarise(ranking = mean(ranking)) %>%
  dplyr::ungroup() %>%
  {.}

rank.value.per.sqft.climate %>%
  dplyr::mutate(period = "mid-century") %>%
  dplyr::bind_rows(rank.value) %>%
  dplyr::filter(variable != "neg.ROI") %>%
  tidyr::spread(period, ranking) %>%
  dplyr::mutate(change = `mid-century`-now) %>%
  dplyr::mutate_at(vars(variable), recode, "predictions"="Site\nkbtu",
                   "predictions.dollar"="Expense\ndollar", "predictions.source"="Source\nkbtu",
                   "predictions.co2"="Emissions\nTon", "neg.ROI"="ROI") %>%
  dplyr::mutate(variable=factor(variable,
                                levels=c("Site\nkbtu", "Source\nkbtu",
                                         "Expense\ndollar", "Emissions\nTon",
                                         "ROI"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=action, fill=change, label=round(change, 1))) +
  ggplot2::geom_tile(alpha=0.7) +
  ggplot2::geom_text() +
  ggplot2::xlab("Evaluation metric") +
  ggplot2::ggtitle("2050-2059 Minus Now Per-sqft Ranking") +
  ggplot2::scale_fill_distiller(palette = "PRGn") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom", panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), ,
                 title=element_text(size=9))
ggplot2::ggsave(sprintf("%s/building_action_rank_tile_per_sqft_change_midcentury.png", imagedir), width=5, height=6)

rank.value.per.sqft.climate %>%
  dplyr::filter(variable != "neg.ROI") %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(norm.ranking = rank(ranking)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(vars(variable), recode, "predictions"="Site\nkbtu",
                   "predictions.dollar"="Expense\ndollar", "predictions.source"="Source\nkbtu",
                   "predictions.co2"="Emissions\nTon") %>%
  dplyr::mutate(variable=factor(variable,
                                levels=c("Site\nkbtu", "Source\nkbtu",
                                         "Expense\ndollar", "Emissions\nTon"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=action, fill=ranking, label=round(ranking, 1))) +
  ## ggplot2::ggplot(ggplot2::aes(x=variable, y=action, fill=norm.ranking, label=round(ranking, 1))) +
  ggplot2::geom_tile(alpha=0.5) +
  ggplot2::geom_text() +
  ggplot2::ggtitle("2050-2059:Per-sqft-based Average Ranking") +
  ggplot2::scale_fill_distiller(palette = "RdYlGn") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom", panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), title=element_text(size=9))
ggplot2::ggsave(sprintf("%s/building_action_rank_tile_per_sqft_midcentury.png", imagedir), width=4.4, height=6)

to.plot.climate %>%
  dplyr::arrange(predictions) %>%
  dplyr::slice(1:40) %>%
  tidyr::unite("label", BLDGNUM, action) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(label, -predictions), y=predictions)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building Action Pair") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by site electricity + gas \nwith mid-century climate") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/many_action_many_building_site_elecgas_future.png", imagedir), width=4.5, height=7)

to.plot.climate %>%
  dplyr::arrange(predictions.source) %>%
  dplyr::slice(1:40) %>%
  tidyr::unite("label", BLDGNUM, action) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(label, -predictions.source), y=predictions.source)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building Action Pair") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by source electricity + gas \nwith mid-century climate") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/many_action_many_building_source_elecgas_future.png", imagedir), width=4.5, height=7)

to.plot.climate %>%
  dplyr::arrange(predictions.co2) %>%
  dplyr::slice(1:40) %>%
  tidyr::unite("label", BLDGNUM, action) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(label, -predictions.co2), y=predictions.co2)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building Action Pair") +
  ggplot2::ylab("Ton/sqft") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("Rank by CO2 of electricity and gas \nwith mid-century climate") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/many_action_many_building_co2_elecgas_future.png", imagedir), width=5, height=7)

to.plot.climate %>%
  dplyr::arrange(predictions.scc) %>%
  dplyr::slice(1:40) %>%
  tidyr::unite("label", BLDGNUM, action) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(label, -predictions.scc), y=predictions.scc)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building Action Pair") +
  ggplot2::ylab("Dollar/sqft") +
  ggplot2::ggtitle("Rank by SCC of electricity and gas \nwith mid-century climate") +
  ggplot2::coord_flip() +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/many_action_many_building_scc_elecgas_future.png", imagedir), width=4, height=7)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## under climate change
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
under.climate.change = readr::read_csv("prediction_under_climate_change.csv") %>%
  dplyr::group_by(BLDGNUM, is.real.retrofit, Substantial_Completion_Date, action, fuel) %>%
  dplyr::summarize(weighted.cmip5.predictions = weighted.mean(x=cmip5.predictions, w=Combined)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scenario = "RCP45 2050-2059") %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::rename(predictions=weighted.cmip5.predictions) %>%
  {.}

## in slides
## annual
to.plot.slides = cf.result %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::select(BLDGNUM, is.real.retrofit, Substantial_Completion_Date, action, fuel, predictions) %>%
  dplyr::mutate(scenario = "now") %>%
  dplyr::bind_rows(under.climate.change) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                  "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  ## annual
  dplyr::mutate(predictions = (-1)*predictions * 12) %>%
  ## dplyr::filter(action != "GSALink") %>%
  {.}

df.label.future = to.plot.slides %>%
  dplyr::distinct(BLDGNUM, action, fuel, scenario) %>%
  dplyr::group_by(action, fuel, scenario) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::mutate(action.label = paste0("n=", n)) %>%
  {.}

to.plot.slides %>%
  ggplot2::ggplot(ggplot2::aes(x=predictions, fill=scenario,
                               color=scenario,
                               group=interaction(action, fuel, scenario))) +
  ggplot2::geom_density(alpha=0.5) +
  ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
  ggplot2::facet_grid(fuel ~ action) +
  geom_rug(data = to.plot.slides[to.plot.slides$scenario %in% "now",]) +
  geom_rug(data = to.plot.slides[to.plot.slides$scenario %in% "RCP45 2050-2059",], sides = "t") +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::scale_color_brewer(palette = "Set2") +
  ggplot2::ylab("Probability Density") +
  ggplot2::coord_flip(ylim=c(0, 3)) +
  ggplot2::theme_bw() +
  ggplot2::xlab("Estimated retrofit effect (kBtu/sqft/year)") +
  ggplot2::ggtitle("Distribution of treatment effect for the un-retrofitted current vs mid century") +
  ggplot2::theme(legend.position="bottom", axis.text.x=element_text(size=5),
                 strip.text.x = element_text(size = 7))
ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_control_rcp45_mid_slides.png", imagedir), width=8, height=4)

## in document
cf.result %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::select(BLDGNUM, is.real.retrofit, Substantial_Completion_Date, action, fuel, predictions) %>%
  dplyr::mutate(scenario = "now") %>%
  dplyr::bind_rows(under.climate.change) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                  "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  ## dplyr::filter(action != "GSALink") %>%
  ggplot2::ggplot(ggplot2::aes(x=predictions, fill=scenario,
                               group=interaction(action, fuel, scenario))) +
  ggplot2::geom_density(alpha=0.5) +
  ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
  ggplot2::facet_grid(fuel ~ action) +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::coord_cartesian(ylim=c(0, 30)) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Distribution of treatment effect for the un-retrofitted current vs mid century") +
  ggplot2::xlab("Estimated retrofit treatment effect") +
  ggplot2::theme(legend.position="bottom", axis.text.x=element_text(size=5))
ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_control_rcp45_mid.png", imagedir), width=7, height=4)

## in slides
target.action = "HVAC"
to.plot.slides.action = cf.result %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::select(BLDGNUM, is.real.retrofit, Substantial_Completion_Date, action, fuel, predictions) %>%
  dplyr::mutate(scenario = "now") %>%
  dplyr::bind_rows(under.climate.change) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                  "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::filter(action == target.action) %>%
  ## annual
  dplyr::mutate(predictions = -1 * predictions * 12) %>%
  {.}

to.plot.slides.action %>%
  dplyr::group_by(is.real.retrofit, scenario, fuel) %>%
  dplyr::summarise_at(vars(predictions), tibble::lst(min, median, mean, max)) %>%
  {.}

## annual target action
to.plot.slides.action %>%
  ggplot2::ggplot(ggplot2::aes(x=predictions, fill=scenario,
                               color=scenario,
                               group=interaction(action, fuel, scenario))) +
  ggplot2::geom_density(alpha=0.5) +
  ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
  geom_rug(data = to.plot.slides.action[to.plot.slides.action$scenario %in% "now" & to.plot.slides.action$action == "HVAC",]) +
  geom_rug(data = to.plot.slides.action[to.plot.slides.action$scenario %in% "RCP45 2050-2059"& to.plot.slides.action$action == "HVAC",], sides = "t") +
  ggplot2::facet_grid(fuel ~ action) +
  ggplot2::geom_text(size=3, data = df.label.future %>% dplyr::filter(action == target.action),
                     mapping=ggplot2::aes(x = Inf, y = Inf, label=action.label),
                     hjust = 1.5, vjust = 1.5, color="black") +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::scale_color_brewer(palette = "Set2") +
  ggplot2::coord_flip(ylim=c(0, 0.95)) +
  ggplot2::ylab("Probability density") +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Distribution of treatment effect for the un-retrofitted current vs mid century") +
  ggplot2::xlab("Estimated retrofit treatment effect") +
  ggplot2::theme(axis.text.x=element_text(size=8))
ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_control_rcp45_mid_%s_slides.png", imagedir, target.action),
                width=4.1, height=4)

## annual current weather target action
to.plot.slides.action %>%
  dplyr::filter(scenario == "now") %>%
  ggplot2::ggplot(ggplot2::aes(x=predictions, fill=scenario,
                               color=scenario,
                               group=interaction(action, fuel, scenario))) +
  ggplot2::geom_density(alpha=0.5) +
  ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
  geom_rug(data = to.plot.slides.action[to.plot.slides.action$scenario %in% "now" & to.plot.slides.action$action == "HVAC",]) +
  ggplot2::facet_grid(fuel ~ action) +
  ggplot2::geom_text(size=3, data = df.label.future %>% dplyr::filter(action == target.action),
                     mapping=ggplot2::aes(x = Inf, y = Inf, label=action.label),
                     hjust = 1.5, vjust = 1.5, color="black") +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::scale_color_brewer(palette = "Set2") +
  ggplot2::coord_flip(ylim=c(0, 0.95)) +
  ggplot2::ylab("Probability density") +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Distribution of treatment effect for the un-retrofitted, current climate") +
  ggplot2::xlab("Estimated retrofit treatment effect (kBtu/sqft/year)") +
  ggplot2::theme(axis.text.x=element_text(size=8))
ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_control_rcp45_mid_%s_slides_now.png", imagedir, target.action),
                width=4.1, height=4)

to.plot.slides.action %>%
  dplyr::filter(action =="HVAC", scenario == "RCP45 2050-2059", fuel=="Electricity") %>%
  ggplot2::ggplot(ggplot2::aes(x=predictions)) +
  ggplot2::geom_density()

  {.}

## annual future weather target action
to.plot.slides.action %>%
  dplyr::group_by(scenario, action, fuel) %>%
  dplyr::summarise_at(vars(predictions), list(min, max, mean, median)) %>%
  dplyr::ungroup()

  dplyr::filter(scenario == "RCP45 2050-2059") %>%
  ggplot2::ggplot(ggplot2::aes(x=predictions, fill=scenario,
                               color=scenario
                               ## group=interaction(action, fuel, scenario)
                               )) +
  ggplot2::geom_density(alpha=0.5) +
  ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
  geom_rug(data = to.plot.slides.action[to.plot.slides.action$scenario %in% "RCP45 2050-2059" & to.plot.slides.action$action == "HVAC",]) +
  ggplot2::facet_grid(fuel ~ action) +
  ggplot2::geom_text(size=3, data = df.label.future %>% dplyr::filter(action == target.action),
                     mapping=ggplot2::aes(x = Inf, y = Inf, label=action.label),
                     hjust = 1.5, vjust = 1.5, color="black") +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::scale_color_brewer(palette = "Set2") +
  ggplot2::coord_flip(ylim=c(0, 0.95), xlim=c(-3.4, 6.56)) +
  ggplot2::ylab("Probability density") +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Distribution of treatment effect for the un-retrofitted, 2050-2059 climate") +
  ggplot2::xlab("Estimated retrofit treatment effect (kBtu/sqft/year)") +
  ggplot2::theme(axis.text.x=element_text(size=8))

ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_control_rcp45_mid_%s_slides_future.png", imagedir, target.action),
                width=4.1, height=4)

## annual in slides
to.plot.slides %>%
  dplyr::filter(scenario == "now") %>%
  ggplot2::ggplot(ggplot2::aes(x=predictions, fill=scenario,
                               color=scenario,
                               group=interaction(action, fuel, scenario))) +
  ggplot2::geom_density(alpha=0.5) +
  ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
  geom_rug(data = to.plot.slides[to.plot.slides$scenario %in% "now",]) +
  ggplot2::facet_grid(fuel ~ action) +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::scale_color_brewer(palette = "Set2") +
  ggplot2::coord_flip(ylim=c(0, 0.95)) +
  ggplot2::ylab("Probability density") +
  ggplot2::geom_text(size=3, data = df.label.future %>% dplyr::filter(action == target.action),
                     mapping=ggplot2::aes(x = Inf, y = Inf, label=action.label),
                     hjust = 1.5, vjust = 1.5) +
  geom_rug(data = to.plot.slides[to.plot.slides$scenario %in% "RCP45 2050-2059",], sides = "t") +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Distribution of treatment effect for the un-retrofitted, current climate") +
  ggplot2::xlab("Estimated retrofit treatment effect") +
  ggplot2::theme(axis.text.x=element_text(size=8))

ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_control_rcp45_mid_%s_slides_now.png", imagedir, target.action),
                width=3.3, height=4)

## in document
target.action = "HVAC"
cf.result %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::select(BLDGNUM, is.real.retrofit, Substantial_Completion_Date, action, fuel, predictions) %>%
  dplyr::mutate(scenario = "now") %>%
  dplyr::bind_rows(under.climate.change) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                  "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::filter(action == target.action) %>%
  ggplot2::ggplot(ggplot2::aes(x=predictions, fill=scenario,
                               group=interaction(action, fuel, scenario))) +
  ggplot2::geom_density(alpha=0.5) +
  ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
  ggplot2::facet_grid(fuel ~ action) +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::coord_cartesian(ylim=c(0, 11)) +
  ggplot2::ylab("Probability density") +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Distribution of treatment effect for the un-retrofitted current vs mid century") +
  ggplot2::xlab("Estimated retrofit treatment effect") +
  ggplot2::theme(axis.text.x=element_text(size=8))
ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_control_rcp45_mid_%s.png", imagedir, target.action),
                width=4, height=4)

## annual
cf.result %>%
  dplyr::mutate(scenario = "now") %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::select(BLDGNUM, is.real.retrofit, Substantial_Completion_Date, action, fuel, predictions) %>%
  dplyr::mutate(scenario = "now") %>%
  dplyr::bind_rows(under.climate.change) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::filter(action == target.action) %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit, action, scenario) %>%
  ## annual
  dplyr::summarise(predictions = sum(predictions) * 12) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scenario) %>%
  dplyr::summarise_at(vars(predictions), tibble::lst(min, median, mean, max)) %>%
  dplyr::ungroup() %>%
  {.}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## building action pair ranking under climate change
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
to.plot = under.climate.change %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity") %>%
  dplyr::select(BLDGNUM, predictions, action, fuel) %>%
  ## not analyze for now, as the pre-retrofit period might be different from the
  ## other ones, no need to block out two years
  dplyr::filter(action != "GSALink") %>%
  dplyr::left_join(mult.to.source, by="fuel") %>%
  dplyr::mutate(predictions.source = predictions * mult) %>%
  dplyr::select(-mult) %>%
  ## get tons of co2
  dplyr::left_join(mult.to.co2, by="fuel") %>%
  dplyr::mutate(predictions.co2 = predictions * mult) %>%
  dplyr::select(-mult) %>%
  dplyr::mutate(predictions.scc = predictions.co2 * 38) %>%
  dplyr::group_by(BLDGNUM, action) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::ungroup() %>%
  {.}

to.plot %>%
  dplyr::arrange(predictions.source) %>%
  dplyr::slice(1:40) %>%
  tidyr::unite("label", BLDGNUM, action) %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(label, -predictions.source), y=predictions.source)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::xlab("Building Action Pair") +
  ggplot2::ylab("kBtu/sqft") +
  ggplot2::coord_flip() +
  ggplot2::labs(title="Rank by source electricity + gas",
                subtitle="RCP45 2050-2059") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/many_action_many_building_source_elecgas_rcp45_mid.png", imagedir), width=4.5, height=7.5)

load("../data/building_location.rda")

building_location <- building_location %>%
  dplyr::select(BLDGNUM, Latitude, Longitude) %>%
  {.}

howmany = 300
## map the highest saver to lowest saver
top.n.now = cf.result %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::select(BLDGNUM, predictions, action, fuel) %>%
  dplyr::group_by(BLDGNUM, action) %>%
  dplyr::summarise(predictions = sum(predictions)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(building_location, by="BLDGNUM") %>%
  dplyr::arrange(predictions) %>%
  dplyr::slice(1:howmany) %>%
  dplyr::group_by(action) %>%
  dplyr::mutate(n=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scenario = "now") %>%
  {.}

top.n.future = under.climate.change %>%
  dplyr::select(BLDGNUM, predictions, action, fuel) %>%
  dplyr::group_by(BLDGNUM, action) %>%
  dplyr::summarise(predictions = sum(predictions)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(building_location, by="BLDGNUM") %>%
  dplyr::arrange(predictions) %>%
  dplyr::slice(1:howmany) %>%
  dplyr::group_by(action) %>%
  dplyr::mutate(n=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scenario = "now") %>%
  dplyr::mutate(scenario = "RCP45 2050-2059") %>%
  {.}

state_shape = sf::st_read("geographical/cb_2017_us_state_20m.shp")

df.label = top.n.now %>%
  dplyr::bind_rows(top.n.future) %>%
  dplyr::distinct(action, scenario, n) %>%
  dplyr::arrange(action, scenario, n) %>%
  dplyr::mutate(action.label = paste0("n=", n)) %>%
  dplyr::select(-n) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  {.}

top.n.now %>%
  dplyr::bind_rows(top.n.future) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(data=state_shape[!(state_shape$STUSPS) %in% c("AK", "HI", "PR"),], fill=NA, size=0.5) +
  ggplot2::geom_point(aes(x = Longitude, y = Latitude, color = predictions)) +
  viridis::scale_color_viridis(name = "kBtu/sqft") +
  ggplot2::ggtitle("Estimated site electricity + gas saving, now vs mid century for the top 300 retrofit") +
  ggplot2::facet_grid(scenario~action) +
  ggplot2::geom_text(size=3, data = df.label,
                     mapping=ggplot2::aes(x = -Inf, y = -Inf, label=action.label),
                     hjust = 0, vjust = -1) +
  ggplot2::theme(strip.text.y = element_text(size = 7),
                 axis.text.x = element_text(size=6),
                 axis.text.y = element_text(size=6))
ggplot2::ggsave(sprintf("%s/effect_now_vs_future_loc.png", imagedir), width=10, height=6)

building.sqft = cf.result %>%
  dplyr::distinct(BLDGNUM, GROSSSQFT) %>%
  {.}

## pre.mmbtu = cf.result %>%
##   dplyr::filter(!is.real.retrofit) %>%
##   dplyr::select(BLDGNUM, `Substantial_Completion_Date`, KWHR, GAS, GROSSSQFT) %>%
##   dplyr::distinct() %>%
##   tidyr::gather(fuel, kbtu.per.sqft, KWHR:GAS) %>%
##   dplyr::mutate(mmbtu =  kbtu.per.sqft * GROSSSQFT / 1000) %>%
##   dplyr::group_by(fuel) %>%
##   dplyr::summarise(mmbtu=sum(mmbtu)) %>%
##   dplyr::ungroup() %>%
##   {.}

## how much targeting buys
building.action.saving.now = cf.result %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::select(BLDGNUM, predictions, action, fuel, GROSSSQFT) %>%
  dplyr::mutate(predictions.kbtu = predictions * GROSSSQFT) %>%
  dplyr::group_by(BLDGNUM, action, fuel) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), predictions = sum(predictions)) %>%
  dplyr::ungroup() %>%
  {.}

pre.energy = cf.result %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::distinct(BLDGNUM, Substantial_Completion_Date, KWHR, GAS, GROSSSQFT) %>%
  tidyr::gather(fuel, value, KWHR:GAS) %>%
  dplyr::mutate(kbtu = value * GROSSSQFT) %>%
  dplyr::group_by(fuel) %>%
  dplyr::summarise(pre.kbtu = sum(kbtu)) %>%
  {.}

building.action.saving.now %>%
  dplyr::distinct(BLDGNUM, fuel) %>%
  dplyr::group_by(fuel) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  {.}

all.no.targeting = building.action.saving.now %>%
  dplyr::group_by(fuel) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "No targeting") %>%
  {.}

all.with.targeting = building.action.saving.now %>%
  dplyr::filter(predictions < 0) %>%
  dplyr::group_by(fuel) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "With targeting") %>%
  {.}

all.no.targeting %>%
  dplyr::bind_rows(all.with.targeting) %>%
  dplyr::mutate(mmbtu = predictions.kbtu / 1000) %>%
  dplyr::select(fuel, label, mmbtu) %>%
  tidyr::spread(label, mmbtu) %>%
  dplyr::mutate(percent = `With targeting` / `No targeting` * 100) %>%
  {.}

all.no.targeting %>%
  dplyr::bind_rows(all.with.targeting) %>%
  dplyr::left_join(pre.energy, by="fuel") %>%
  dplyr::mutate(percent.save = predictions.kbtu/pre.kbtu * 100) %>%
  dplyr::arrange(fuel, label) %>%
  {.}

## in slides
all.no.targeting %>%
  dplyr::bind_rows(all.with.targeting) %>%
  dplyr::mutate(mmbtu = predictions.kbtu / 1000) %>%
  dplyr::mutate_at(vars(fuel), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  dplyr::mutate(fuel=factor(fuel, levels=c("Natural Gas", "Electricity"))) %>%
  dplyr::mutate(mmbtu=(-1) * mmbtu) %>%
  ## ggplot2::ggplot(ggplot2::aes(x=fuel, y=mmbtu, fill=label, label=count)) +
  ggplot2::ggplot(ggplot2::aes(x=fuel, y=mmbtu, fill=interaction(label, fuel), label=count)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::ggtitle("Targeting only the reducers \nvs all buildings taking all 6 retrofits ") +
  ## ggplot2::scale_fill_grey(start=0.8, end=0.2, name="") +
  ggplot2::scale_fill_manual(values=RColorBrewer::brewer.pal(8, "Paired")[1:4], name="") +
  ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9)) +
  ggplot2::expand_limits(y=7e4) +
  ## ggplot2::expand_limits(y=-7e7) +
  ggplot2::ylab("MMBtu") +
  ggplot2::scale_y_reverse() +
  ggplot2::coord_flip() +
  ggplot2::theme(legend.position="none", axis.title.y=element_blank())
ggplot2::ggsave(sprintf("%s/targeting_all_slides.png", imagedir), width=5, height=4)

## in document
all.no.targeting %>%
  dplyr::bind_rows(all.with.targeting) %>%
  dplyr::mutate(mmbtu = predictions.kbtu / 1000) %>%
  dplyr::mutate_at(vars(fuel), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  dplyr::mutate(fuel=factor(fuel, levels=c("Natural Gas", "Electricity"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=fuel, y=mmbtu, fill=label, label=count)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::ggtitle("Targeting only the reducers \nvs all buildings taking all 6 retrofits ") +
  ggplot2::scale_fill_grey(start=0.8, end=0.2, name="") +
  ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9)) +
  ggplot2::expand_limits(y=-7e4) +
  ## ggplot2::expand_limits(y=-7e7) +
  ggplot2::ylab("MMBtu") +
  ggplot2::coord_flip() +
  ggplot2::theme(legend.position="none", axis.title.y=element_blank())
ggplot2::ggsave(sprintf("%s/targeting_all.png", imagedir), width=5, height=4)

## by action now
by.action.no.targeting = building.action.saving.now %>%
  dplyr::group_by(action, fuel) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "No targeting") %>%
  {.}

by.action.with.targeting = building.action.saving.now %>%
  dplyr::filter(predictions < 0) %>%
  dplyr::group_by(action, fuel) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "With targeting") %>%
  {.}

## in slides
by.action.no.targeting %>%
  dplyr::bind_rows(by.action.with.targeting) %>%
  dplyr::mutate(mmbtu = predictions.kbtu / 1000) %>%
  dplyr::mutate_at(vars(fuel), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  dplyr::mutate(fuel=factor(fuel, levels=c("Natural Gas", "Electricity"))) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate(mmbtu = -1 * mmbtu) %>%
  ## ggplot2::ggplot(ggplot2::aes(x=fuel, y=mmbtu, fill=label, label=count)) +
  ggplot2::ggplot(ggplot2::aes(x=fuel, y=mmbtu, fill=interaction(label, fuel), label=count)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::ggtitle("Targeting only the reducers vs all buildings taking an action") +
  ## ggplot2::scale_fill_grey(start=0.8, end=0.2) +
  ggplot2::scale_fill_manual(values=RColorBrewer::brewer.pal(8, "Paired")[1:4], name="") +
  ggplot2::facet_wrap(.~action, ncol=2) +
  ggplot2::coord_flip() +
  ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9), size=3) +
  ggplot2::expand_limits(y=3.5e4) +
  ggplot2::scale_y_reverse() +
  ggplot2::ylab("MMBtu") +
  ggplot2::theme(legend.position="none", title=element_text(size=9), axis.title.y=element_blank())
ggplot2::ggsave(sprintf("%s/targeting_by_action_slides.png", imagedir), width=7, height=4)

## in document
by.action.no.targeting %>%
  dplyr::bind_rows(by.action.with.targeting) %>%
  dplyr::mutate(mmbtu = predictions.kbtu / 1000) %>%
  dplyr::mutate_at(vars(fuel), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  dplyr::mutate(fuel=factor(fuel, levels=c("Natural Gas", "Electricity"))) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  ggplot2::ggplot(ggplot2::aes(x=fuel, y=mmbtu, fill=label, label=count)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::ggtitle("Targeting only the reducers vs all buildings taking an action") +
  ggplot2::scale_fill_grey(start=0.8, end=0.2) +
  ggplot2::facet_wrap(.~action, ncol=2) +
  ggplot2::coord_flip() +
  ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9), size=3) +
  ggplot2::expand_limits(y=-3.5e4) +
  ggplot2::ylab("MMBtu") +
  ggplot2::theme(legend.position="none", title=element_text(size=9), axis.title.y=element_blank())
ggplot2::ggsave(sprintf("%s/targeting_by_action.png", imagedir), width=7, height=4)

building.size = cf.result %>%
  dplyr::distinct(BLDGNUM, GROSSSQFT) %>%
  {.}

under.climate.change = readr::read_csv("prediction_under_climate_change.csv") %>%
  dplyr::group_by(BLDGNUM, is.real.retrofit, Substantial_Completion_Date, action, fuel) %>%
  dplyr::summarize(weighted.cmip5.predictions = weighted.mean(x=cmip5.predictions, w=Combined)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scenario = "RCP45 2050-2059") %>%
  dplyr::filter(!is.real.retrofit) %>%
  dplyr::rename(predictions=weighted.cmip5.predictions) %>%
  {.}

building.action.saving.future = under.climate.change %>%
  dplyr::filter(is.real.retrofit==0) %>%
  dplyr::select(BLDGNUM, predictions, action, fuel) %>%
  dplyr::left_join(building.size, by="BLDGNUM") %>%
  dplyr::mutate(predictions.kbtu = predictions * GROSSSQFT) %>%
  dplyr::group_by(BLDGNUM, action, fuel) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), predictions = sum(predictions)) %>%
  dplyr::ungroup() %>%
  {.}

all.no.targeting.future = building.action.saving.future %>%
  dplyr::group_by(fuel) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "No targeting") %>%
  {.}

all.with.targeting.future = building.action.saving.future %>%
  dplyr::filter(predictions < 0) %>%
  dplyr::group_by(fuel) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "With targeting") %>%
  {.}

all.no.targeting.future %>%
  dplyr::bind_rows(all.with.targeting.future) %>%
  dplyr::mutate(mmbtu = predictions.kbtu / 1000) %>%
  dplyr::select(fuel, label, mmbtu) %>%
  tidyr::spread(label, mmbtu) %>%
  dplyr::mutate(percent = `With targeting` / `No targeting` * 100 - 100) %>%
  {.}

all.no.targeting.future %>%
  dplyr::bind_rows(all.with.targeting.future) %>%
  dplyr::mutate(mmbtu = predictions.kbtu / 1000) %>%
  dplyr::mutate_at(vars(fuel), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  dplyr::mutate(fuel=factor(fuel, levels=c("Natural Gas", "Electricity"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=fuel, y=mmbtu, fill=label, label=count)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::ggtitle("Mid-century: All buildings taking all 6 retrofits \nvs targeting only the reducers") +
  ggplot2::scale_fill_grey(start=0.8, end=0.2, name="") +
  ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9)) +
  ggplot2::expand_limits(y=-9e4) +
  ggplot2::ylab("MMBtu") +
  ggplot2::coord_flip() +
  ggplot2::theme(legend.position="none", axis.title.y=element_blank())
ggplot2::ggsave(sprintf("%s/targeting_all_future.png", imagedir), width=5, height=4)

## by action future
by.action.no.targeting.future = building.action.saving.future %>%
  dplyr::group_by(action, fuel) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "No targeting") %>%
  {.}

by.action.with.targeting.future = building.action.saving.future %>%
  dplyr::filter(predictions < 0) %>%
  dplyr::group_by(action, fuel) %>%
  dplyr::summarise(predictions.kbtu = sum(predictions.kbtu), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = "With targeting") %>%
  {.}

## in slides
by.action.no.targeting.future %>%
  dplyr::bind_rows(by.action.with.targeting.future) %>%
  dplyr::mutate(mmbtu = predictions.kbtu / 1000) %>%
  dplyr::mutate_at(vars(fuel), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  dplyr::mutate(fuel=factor(fuel, levels=c("Natural Gas", "Electricity"))) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate(mmbtu = -1 * mmbtu) %>%
  ggplot2::ggplot(ggplot2::aes(x=fuel, y=mmbtu, fill=interaction(label, fuel), label=count)) +
  ## ggplot2::ggplot(ggplot2::aes(x=fuel, y=mmbtu, fill=label, label=count)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::ggtitle("All buildings taking an action vs targeting only reducers") +
  ## ggplot2::scale_fill_grey(start=0.8, end=0.2) +
  ggplot2::scale_fill_manual(values=RColorBrewer::brewer.pal(8, "Paired")[5:8], name="") +
  ggplot2::facet_wrap(.~action, ncol=2) +
  ggplot2::scale_y_reverse() +
  ggplot2::coord_flip() +
  ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9), size=3) +
  ggplot2::expand_limits(y=3.5e4) +
  ggplot2::ylab("MMBtu") +
  ggplot2::theme(legend.position="none", title=element_text(size=9), axis.title.y=element_blank())
ggplot2::ggsave(sprintf("%s/targeting_by_action_future_slides.png", imagedir), width=7, height=4)

## in document
by.action.no.targeting.future %>%
  dplyr::bind_rows(by.action.with.targeting.future) %>%
  dplyr::mutate(mmbtu = predictions.kbtu / 1000) %>%
  dplyr::mutate_at(vars(fuel), dplyr::recode,
                   "CHILLWTR"="Chilled Water", "GAS"="Natural Gas",
                   "KWHR"="Electricity", "STEAM"="Steam") %>%
  dplyr::mutate(fuel=factor(fuel, levels=c("Natural Gas", "Electricity"))) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  ggplot2::ggplot(ggplot2::aes(x=fuel, y=mmbtu, fill=label, label=count)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::ggtitle("All buildings taking an action vs targeting only reducers") +
  ggplot2::scale_fill_grey(start=0.8, end=0.2) +
  ggplot2::facet_wrap(.~action, ncol=2) +
  ggplot2::coord_flip() +
  ggplot2::geom_text(ggplot2::aes(label=label), hjust=1.05, position = position_dodge(0.9), size=3) +
  ggplot2::expand_limits(y=-3.5e4) +
  ggplot2::ylab("MMBtu") +
  ggplot2::theme(legend.position="none", title=element_text(size=9), axis.title.y=element_blank())
ggplot2::ggsave(sprintf("%s/targeting_by_action_future.png", imagedir), width=7, height=4)

retrofit.alldata.highprop %>%
  distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by( is.real.retrofit) %>%
  dplyr::count() %>%
  dplyr::ungroup()

## illustrations
## build a tree illustration with cdd and pre-retrofit elec
ill.outcome = retrofit.alldata.highprop %>%
  dplyr::filter(!(variable %in% c("KWDMD", "OIL"))) %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(BLDGNUM, is.real.retrofit,
                `Substantial_Completion_Date`, variable, mean.kbtu, GROSSSQFT) %>%
  dplyr::mutate(kbtu.per.sqft = mean.kbtu/GROSSSQFT) %>%
  dplyr::select(-mean.kbtu, -GROSSSQFT) %>%
  tidyr::spread(variable, kbtu.per.sqft, fill=0) %>%
  dplyr::select(-CHILLWTR, -GAS, -STEAM) %>%
  {.}

ill.covariate = retrofit.alldata.highprop %>%
  dplyr::filter(variable == "KWHR", retro.status == "pre") %>%
  dplyr::mutate(pre.elec = mean.kbtu / GROSSSQFT) %>%
  dplyr::distinct(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit, cldd, pre.elec, htdd, GROSSSQFT) %>%
  {.}

inputs = ill.covariate %>%
  dplyr::left_join(ill.outcome, by=c("BLDGNUM", "Substantial_Completion_Date", "is.real.retrofit")) %>%
  {.}

set.seed(0)
inputs = tibble::tibble(cldd = runif(300, 500, 4000), sqft = runif(300, 2000, 3500000), KWHR=rnorm(300, 5, 3))

library("tree")

## reg.tree = tree::tree(KWHR ~ cldd + sqft, data=inputs)
restrict.inputs = inputs %>%
  ## dplyr::filter(htdd < 2300) %>%
  {.}

reg.tree = tree::tree(KWHR ~ cldd + htdd, data=restrict.inputs)

png(sprintf("%s/ill_tree.png", imagedir))
plot(reg.tree )
text(reg.tree )
dev.off()

png(sprintf("%s/ill_part.png", imagedir))
is.retro = restrict.inputs %>%
  dplyr::filter(is.real.retrofit)
no.retro = restrict.inputs %>%
  dplyr::filter(!is.real.retrofit)
tree::partition.tree(reg.tree)
points(x=is.retro$cldd, y=is.retro$htdd, col="red")
points(x=no.retro$cldd, y=no.retro$htdd, col="blue")
dev.off()

set.seed(0)
tibble::tibble(value = rnorm(20, 1, 1.1)) %>%
  dplyr::arrange(desc(value)) %>%
  tibble::rowid_to_column("rank") %>%
  dplyr::arrange(value) %>%
  ## dplyr::mutate(rank = factor(rank)) %>%
  ggplot2::ggplot(ggplot2::aes(x=rank, y=value, label=rank)) +
  ggplot2::geom_bar(stat="identity") +
  ggpubr::rotate() +
  ggplot2::scale_x_reverse() +
  ggplot2::ylab("Saving") +
  ggplot2::theme_bw() +
  ggplot2::theme(text=element_text(size=20))
ggplot2::ggsave(sprintf("%s/ill_rank_energy.png", imagedir), width=4, height=7)

readr::read_csv("energy_by_sector.csv") %>%
  tidyr::gather(year, consumption, `2019`:`2015`) %>%
    dplyr::rename(`End Use`=`End–Use Sector`) %>%
    dplyr::filter(`End Use`!= "Primary Total") %>%
    dplyr::mutate(`End Use`=factor(`End Use`,
                                   levels=c("Commercial", "Residential", "Industrial", "Transportation"))) %>%
    ggplot2::ggplot(ggplot2::aes(x=year, y=consumption, fill=`End Use`)) +
    ggplot2::ylab("trillion Btu") +
    ggplot2::ggtitle("Energy consumption by sector") +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::scale_fill_manual(values=c(RColorBrewer::brewer.pal(12, "Paired")[3:4], RColorBrewer::brewer.pal(12, "Dark2")[2:3])) +
    ggplot2::theme()
ggplot2::ggsave(sprintf("%s/energy_by_sector_eia.png", imagedir), width=4, height=3)

## https://www.eia.gov/consumption/
readr::read_csv("energy_by_sector.csv") %>%
  tidyr::gather(year, consumption, `2019`:`2015`) %>%
    dplyr::rename(`End Use`=`End–Use Sector`) %>%
    ## dplyr::mutate(`End Use`=factor(`End Use`,
    ##                                levels=c("Commercial", "Residential", "Industrial", "Transportation"))) %>%
    tidyr::spread(`End Use`, consumption) %>%
    dplyr::mutate(ratio = (Residential + Commercial) / `Primary Total` * 100) %>%
    dplyr::mutate(commercial.ratio = Commercial / `Primary Total` * 100) %>%
    dplyr::select(`year`, ratio, commercial.ratio) %>%
    {.}

readr::read_csv("emissions_by_sector.csv") %>%
  tidyr::gather(sector, emissions, Residential:`Electric power`) %>%
  dplyr::mutate(`sector`=factor(`sector`,
                                levels=c("Commercial", "Residential",
                                          "Industrial", "Transportation",
                                          "Electric power"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=`Source`, y=emissions, fill=sector)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::ylab("million metric tons") +
  ggplot2::scale_fill_manual(values=c(RColorBrewer::brewer.pal(12, "Paired")[3:4],
                                      RColorBrewer::brewer.pal(12, "Dark2")[2:4])) +
  ggplot2::ggtitle("CO2 emissions by sector") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/emissions_by_sector_eia.png", imagedir), width=5, height=3)

readr::read_csv("emissions_by_sector.csv") %>%
  dplyr::select(-`Source total`) %>%
  tidyr::gather(sector, emissions, Residential:`Electric power`) %>%
  dplyr::mutate(`sector`=factor(`sector`,
                                levels=c("Commercial", "Residential",
                                         "Industrial", "Transportation",
                                         "Electric power"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=`Source`, y=emissions, fill=sector)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::ylab("million metric tons") +
  ggplot2::ggtitle("CO2 emissions by sector") +
  ggplot2::scale_fill_brewer(palette="Set3") +
  ggplot2::theme()
ggplot2::ggsave(sprintf("%s/emissions_by_sector_eia.png", imagedir), width=6, height=3)

## 2018: https://www.eia.gov/tools/faqs/faq.php?id=75&t=11
readr::read_csv("emissions_by_sector.csv") %>%
  ## dplyr::select(-`Source total`) %>%
  ## tidyr::gather(sector, emissions, Residential:`Electric power`) %>%
    dplyr::mutate(ratio = (Residential + Commercial) / `Source total` * 100) %>%
    dplyr::mutate(commercial.ratio = Commercial / `Source total` * 100) %>%
    dplyr::select(`Source`, ratio, commercial.ratio)

  {.}
