library("sf")
library("ggplot2")
library("dplyr")

load("../data/building_location.rda")
load("../data/energy_monthly_web_withloc.rda")
load("../data/energy_90_to_18.rda")

imagedir = "~/Dropbox/thesis/writeups/plan/images"
tabledir = "~/Dropbox/thesis/writeups/plan/tables"

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

df = energy_monthly_web_withloc %>%
  distinct(BLDGNUM, Year) %>%
  dplyr::left_join(building_location, by=c("BLDGNUM")) %>%
  dplyr::select(BLDGNUM, Year, Latitude, Longitude)

longterm = energy_90_to_18 %>%
  distinct(BLDGNUM) %>%
  dplyr::left_join(building_location, by=c("BLDGNUM")) %>%
  dplyr::select(BLDGNUM, Latitude, Longitude)

state_shape = sf::st_read("geographical/cb_2017_us_state_20m.shp")

p <- state_shape %>%
  dplyr::filter(!`STUSPS` %in% c("AK", "HI", "VI", "GU", "PR", "VI")) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill="grey")

year = 1990
dfyear = df %>%
  dplyr::filter(Year==year)

p +
  ggplot2::ggtitle(sprintf("GSA buildings in %s", year)) +
  ggplot2::geom_point(ggplot2::aes(y=`Latitude`, x=`Longitude`), data=dfyear, size=0.5) +
  ggplot2::theme()
ggsave(sprintf("%s/buildingloc_%d.png", imagedir, year), width=6, height=3)

year = 2019
dfyear = df %>%
  dplyr::filter(Year==year)

p +
  ggplot2::ggtitle(sprintf("GSA buildings in %s", year)) +
  ggplot2::geom_point(ggplot2::aes(y=`Latitude`, x=`Longitude`), data=dfyear, size=0.5) +
  ggplot2::theme()
ggsave(sprintf("%s/buildingloc_%d.png", imagedir, year), width=6, height=3)

p +
  ggplot2::ggtitle("GSA buildings with data from 1990 to 2018") +
  ggplot2::geom_point(ggplot2::aes(y=`Latitude`, x=`Longitude`), data=longterm, size=0.5) +
  ggplot2::theme()
ggsave(sprintf("%s/buildingloc_30year.png", imagedir), width=6, height=3)

load("../data/building_ann_cldd_normal.rda")

p +
  ggplot2::geom_point(ggplot2::aes(y=`latitude`, x=`longitude`, color=cldd), data=building_ann_cldd_normal) +
  ggplot2::scale_color_gradient(low="deepskyblue", high="orangered") +
  ggplot2::theme()
ggsave(sprintf("%s/buildingloc_30year_cddnormal.png", imagedir), width=6, height=3)

building_ann_cldd_normal <- building_ann_cldd_normal %>%
  dplyr::mutate(climate.verbose=factor(climate.verbose, levels=c("cold:[5,745)", "mild:[745,1322)", "hot:[1322,4838]")))

p +
  ggplot2::ggtitle("Climate classification") +
  ggplot2::geom_point(ggplot2::aes(y=`latitude`, x=`longitude`, color=climate.verbose), data=building_ann_cldd_normal) +
  ggplot2::scale_color_manual(values = RColorBrewer::brewer.pal(3, "RdBu")[c(3, 2, 1)], name="Climate") +
  ggplot2::theme()
ggsave(sprintf("%s/buildingloc_30year_climate.png", imagedir), width=6, height=3)

load("../data/retrofit.rda")

retrofit %>%
  filter(`Building ID` %in% unique(longterm$BLDGNUM)) %>%
  dplyr::arrange(`ARRA Substantial Completion Date`) %>%
  dplyr::select(`ARRA Substantial Completion Date`) %>%
  tail() %>%
  {.}

load("../data/retrofit_from_db.rda")

retrofit_from_db %>%
  dplyr::rename(building=`Building_Number`) %>%
  dplyr::distinct(building, high_level_ECM) %>%
  dplyr::group_by(high_level_ECM) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup() %>%
  readr::write_csv(sprintf("%s/retrofit_count_highlevel.csv", tabledir))

retrofit_from_db %>%
  dplyr::rename(building=`Building_Number`) %>%
  dplyr::distinct(building, high_level_ECM, detail_level_ECM) %>%
  dplyr::group_by(high_level_ECM, detail_level_ECM) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup() %>%
  readr::write_csv(sprintf("%s/retrofit_count_detaillevel.csv", tabledir))

## check retrofit dates
## 1 CA0168ZZ 2010-03-01 00:00:00
## 1 OR0045ZZ 2015-10-16 00:00:00
retrofit_from_db %>%
  dplyr::rename(`BLDGNUM`=`Building_Number`) %>%
  dplyr::distinct(BLDGNUM, Substantial_Completion_Date) %>%
  dplyr::arrange((`Substantial_Completion_Date`)) %>%
  {.}

load("../data/energy_monthly_web_withloc.rda")
load("../data/energy_monthly_web_continental.rda")

energy_during_retro_period = energy_monthly_web_withloc %>%
  dplyr::filter(2005 < Year,  Year < 2019) %>%
  {.}

counts = energy_during_retro_period %>%
  dplyr::mutate(retrofitted = (BLDGNUM %in% unique(retrofit_from_db$`Building_Number`))) %>%
  dplyr::distinct(retrofitted, BLDGNUM) %>%
  dplyr::group_by(retrofitted) %>%
  dplyr::summarise(n()) %>%
  {.}
## retrofitted `n()`
## <lgl>       <int>
## 1 FALSE        1207
## 2 TRUE          299

## plot number of buildings with energy data in each fiscal year
energy_during_retro_period %>%
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
  ggplot2::geom_vline(xintercept=zoo::as.yearmon("2010-10", format="%Y-%m"), linetype="dashed") +
  ggplot2::geom_vline(xintercept=zoo::as.yearmon("2015-09", format="%Y-%m"), linetype="dashed") +
  ggplot2::ylab("Monthly average kbtu/sqft") +
  ggplot2::xlab("Time") +
  ggplot2::theme()
ggsave(sprintf("%s/energy_retrofit_status.png", imagedir), width=6, height=3)

## no buildings have the same type of action at multiple time
## retro_long %>%
##   dplyr::group_by(`Building_Number`,  high_level_ECM) %>%
##   dplyr::filter(n()>1) %>%
##   {.}



  dplyr::
  dplyr::rename(`BLDGNUM`=`Building_Number`) %>%
  dplyr::distinct(BLDGNUM, Substantial_Completion_Date) %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  {.}
  dplyr::mutate(post.start = as.POSIXct(`Substantial_Completion_Date`),
                pre.end = as.POSIXct(`Substantial_Completion_Date`) - lubridate::years(2),
                pre.start = pre.end - lubridate::years(3),
                post.end = post.start + lubridate::years(3)) %>%
  ## dplyr::select(starts_with("post.")) %>%
  {.}


energy_during_retro_period %>%
  dplyr::select(BLDGNUM, Year, Month, ends_with(".kbtu"), GROSSSQFT) %>%
  dplyr::mutate(total.kbtu = Electricity.kbtu + Steam.kbtu + Gas.kbtu + Coal.kbtu + Oil.kbtu + Chilledwater.kbtu) %>%
  dplyr::mutate_at(vars(ends_with(".kbtu")), ~replace(., is.na(.), 0)) %>%
  dplyr::mutate_at(vars(ends_with(".kbtu")), list("persqft" = function(x) {x/.$GROSSSQFT})) %>%
  dplyr::left_join(retro_date, by="BLDGNUM") %>%
  dplyr::arrange(BLDGNUM, Year, Month) %>%
  {.}

  dplyr::group_by()

  dplyr::distinct(BLDGNUM, GROSSSQFT, BLDGCAT) %>%
  dplyr::mutate(`retrofit status`= BLDGNUM %in% unique(retrofit_from_db$`Building_Number`)) %>%
  dplyr::group_by(BLDGNUM) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  distinct(BLDGCAT) %>%
  {.}

  dplyr::distinct()

