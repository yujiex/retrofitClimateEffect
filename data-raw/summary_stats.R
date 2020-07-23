library("dplyr")
library("ggplot2")

imagedir = "../images"

load("../data/retrofit.alldata.rda")

load("../data/retrofit_from_db.rda")

load("../data/retrofit_prev_actions_toplevel.rda")
load("../data/retrofit_prev_actions_highlevel.rda")
load("../data/retrofit_prev_actions_joint_highlevel.rda")
load("../data/retrofit_prev_actions_detaillevel.rda")


## helpers
## from https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
## get scale_fill_hue hex values
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## building count by action type
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

kw = "detaillevel"

if (kw == "toplevel") {
  df.action = retrofit_prev_actions_toplevel %>%
    {.}
  image.width = 5
  titlekw = "Joint Level-1"
} else if (kw == "highlevel") {
  df.action = retrofit_prev_actions_highlevel %>%
    {.}
  image.width = 6
  titlekw = "Level-2"
} else if (kw == "detaillevel") {
  df.action = retrofit_prev_actions_detaillevel %>%
    {.}
  image.width = 10
  titlekw = "Level-3"
}

df.action <- df.action %>%
  dplyr::select(`Building_Number`:target.date) %>%
  {.}

action.ratio = retrofit.alldata %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit, `Substantial_Completion_Date`) %>%
  dplyr::left_join(df.action, by=c("BLDGNUM"="Building_Number",
                                   "Substantial_Completion_Date"="target.date")) %>%
  dplyr::group_by(target.action) %>%
  dplyr::summarise(ratio = round(n() / length(unique(.$BLDGNUM)) * 100, 1), count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename(action=target.action) %>%
  tidyr::replace_na(list(action="None")) %>%
  {.}

n.distinct.action = action.ratio %>%
  distinct(action) %>%
  nrow()

if (kw == "toplevel") {
  action.ratio <- action.ratio %>%
    dplyr::mutate(action = factor(action, levels=c("Capital", "Capital and Operational", "Operational", "None"))) %>%
    {.}
} else if (kw == "highlevel") {
  action.ratio <- action.ratio %>%
    dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                     "Building Tuneup or Utility Improvements"="Commissioning") %>%
    {.}
  action.ratio <- action.ratio %>%
    dplyr::mutate(action = stringr::str_wrap(action, 15)) %>%
    {.}
} else if (kw == "detaillevel") {
  action.ratio <- action.ratio %>%
    tidyr::separate(action, into=c("l1", "l2", "l3"), sep="_") %>%
    dplyr::arrange(l1, l3, l2) %>%
    tidyr::unite("action", l2:l3) %>%
    dplyr::mutate(action = gsub("_NA", "", action)) %>%
    dplyr::mutate(action = gsub("_", " ", action)) %>%
    dplyr::mutate_at(dplyr::vars(l1), dplyr::recode,
                     "Building Tuneup or Utility Improvements"="Commissioning") %>%
    dplyr::mutate(action = ifelse(action == "NA", "Unknown", action)) %>%
    {.}
}

if (kw != "detaillevel") {
  action.ratio %>%
    ggplot2::ggplot(ggplot2::aes(x=action, y=count, fill=action, label=paste0(round(ratio, 1), "%"))) +
    ggplot2::ggtitle(sprintf("Building Count by %s Actions Type", titlekw)) +
    ggplot2::ylab("Building Count") +
    ggplot2::xlab("Actions") +
    ggplot2::geom_bar(stat="identity", alpha=0.7) +
    ggplot2::geom_text(nudge_y = 10) +
    ggplot2::scale_fill_manual(values = c(gg_color_hue(n.distinct.action - 1),
                                          "#AAAAAA")) +
    ## ggplot2::annotate("text", x=2.5, y=190, label="(percent of buildings in the study set)") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
  ggplot2::ggsave(sprintf("%s/%s_action_count_bar.png", imagedir, kw), width=image.width, height=5)
} else {
  dfs.plot = action.ratio %>%
    dplyr::group_by(l1) %>%
    dplyr::filter(n()>1) %>%
    dplyr::group_split()
  lapply(dfs.plot, function(df) {
    highlevel = df$l1[[1]]
    naction = length(unique(df$action))
    print(naction)
    if (highlevel == "Lighting") {
      action.levels = c("Indoor Lighting Controls",
                        "Outdoor Lighting Controls",
                        "Indoor Retrofit or Replacement",
                        "Outdoor Retrofit or Replacement",
                        "Indoor Daylighting",
                        "Indoor Daylighting or Lighting Strategies",
                        "Unknown")
    } else {
      action.levels = df %>%
        distinct(action) %>%
        .$action %>%
        {.}
    }
    action.levels <- stringr::str_wrap(action.levels, 20)
    df %>%
      dplyr::mutate(action = stringr::str_wrap(action, 20)) %>%
      dplyr::mutate(action = factor(action, levels = action.levels)) %>%
      ggplot2::ggplot(ggplot2::aes(x=action, y=count, fill=action, label=paste0(round(ratio, 1), "%"))) +
      ggplot2::ggtitle(sprintf("Building Count by %s Sub Actions", highlevel)) +
        ggplot2::ylab("Building Count") +
        ggplot2::xlab("Actions") +
        ggplot2::geom_bar(stat="identity", alpha=0.7) +
        ggplot2::geom_text(nudge_y = 5) +
        ggplot2::scale_fill_brewer(palette = "Paired") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none",
                       plot.title = element_text(size=max(naction * 2, 9)))
    ggplot2::ggsave(sprintf("%s/%s_%s_action_count_bar.png", imagedir, kw, highlevel), width=max(1.5 * naction, 4), height=5)
  })
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## building count by number of actions
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
percent.no.retro = retrofit.alldata %>%
  dplyr::mutate(is.real.retrofit = as.numeric(is.real.retrofit)) %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit) %>%
  dplyr::group_by(is.real.retrofit) %>%
  dplyr::summarise(ratio = n() / length(.$BLDGNUM) * 100, count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename(n=is.real.retrofit) %>%
  dplyr::filter(n==0) %>%
  {.}

kw = "toplevel"

for (kw in c("toplevel", "highlevel", "detaillevel")) {
  if (kw == "toplevel") {
    df.action = retrofit_prev_actions_highlevel %>%
      dplyr::mutate(target.action = ifelse(target.action %in% c("Building Envelope", "HVAC", "Lighting"), "Capital", "Operational")) %>%
      distinct(`Building_Number`, target.action, target.date) %>%
        {.}
    image.width = 5
    titlekw = "Joint Level-1"
  } else if (kw == "highlevel") {
    df.action = retrofit_prev_actions_highlevel %>%
      {.}
    image.width = 6
    titlekw = "Level-2"
  } else if (kw == "detaillevel") {
    df.action = retrofit_prev_actions_detaillevel %>%
      {.}
    image.width = 10
    titlekw = "Level-3"
  }
  num.project = retrofit.alldata %>%
    dplyr::distinct(BLDGNUM, Substantial_Completion_Date) %>%
    nrow()
  num.action = retrofit.alldata %>%
    dplyr::filter(is.real.retrofit) %>%
    dplyr::distinct(BLDGNUM, is.real.retrofit, `Substantial_Completion_Date`) %>%
    dplyr::left_join(df.action, by=c("BLDGNUM"="Building_Number",
                                    "Substantial_Completion_Date"="target.date")) %>%
    dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(n) %>%
    dplyr::summarise(ratio = round(n() / num.project * 100, 1),
                    count=n()) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(percent.no.retro) %>%
    dplyr::arrange(n) %>%
    {.}
  p <- num.action %>%
    dplyr::mutate(n=factor(n)) %>%
    ggplot2::ggplot(ggplot2::aes(x=n, y=count, label=paste0(round(ratio, 1), "%"))) +
    ggplot2::geom_bar(stat="identity", fill="chartreuse3") +
    ggplot2::geom_text(nudge_y = 10) +
    ## ggplot2::annotate("text", x=3.8, y=280, label="(percent of buildings in the study set)") +
    ggplot2::ggtitle(sprintf("Ratio of Projects with x %s Actions", titlekw)) +
    ggplot2::ylab("Building Count") +
    ggplot2::xlab("Number of Actions")
  if (kw == "toplevel") {
    p <- p + ggplot2::theme(plot.title = element_text(size=10))
  }
  ggplot2::ggsave(sprintf("%s/%s_num_action_count_bar.png", imagedir, kw),
                  width=min(max(max(num.action$n) * 1, 4), 10), height=3)
}


dplyr::inner_join(cmip5.bin.period.threeyear, by="BLDGNUM") %>%

load("../data/cmip5.bin.period.threeyear.rda")

cmip5.bin.period %>%
  {.}

## temperature distribution
retrofit.alldata %>%
  dplyr::filter()
  {.}

midcentury.weather = retrofit.alldata %>%
  distinct(BLDGNUM, `Substantial_Completion_Date`, is.real.retrofit) %>%
  dplyr::left_join(cmip5.bin.period.threeyear, by=c("BLDGNUM")) %>%
  dplyr::filter(period == "2050Jan through 2059Jan") %>%
  dplyr::select(-Latitude, -Longitude, -Missing) %>%
  dplyr::group_by(period, scenario, model) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
  dplyr::ungroup() %>%
  tidyr::gather(temperature.bin, days, `<10`:`>90`) %>%
  dplyr::mutate(temperature.bin = factor(temperature.bin, levels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90"))) %>%
  ## dplyr::left_join(model.weights, by="model") %>%
  dplyr::group_by(period, scenario, temperature.bin) %>%
  ## dplyr::summarise(days = weighted.mean(days, w=Combined)) %>%
  dplyr::summarise(days = mean(days)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = ifelse(scenario == "rcp45", "2054-2056 (RCP4.5)", "2054-2056 (RCP8.5)")) %>%
  dplyr::select(label, temperature.bin, days) %>%
  {.}

now.weather = retrofit.alldata %>%
  dplyr::filter(retro.status == "pre", scenario == "measured") %>%
  dplyr::select(BLDGNUM, `Substantial_Completion_Date`, (`<10`:`>90`)) %>%
  dplyr::distinct() %>%
  tidyr::gather(temperature.bin, days, `<10`:`>90`) %>%
  dplyr::mutate(label = "3 year pre-retrofit") %>%
  dplyr::group_by(label, temperature.bin) %>%
  dplyr::summarise(days = mean(days)) %>%
  dplyr::ungroup() %>%
  {.}

retrofit.alldata %>%
  dplyr::filter(retro.status == "pre", scenario == "measured") %>%
  select(`Substantial_Completion_Date`) %>%
  summary() %>%
  {.}

## 3 bars per bin
now.weather %>%
  dplyr::bind_rows(midcentury.weather) %>%
  dplyr::mutate(label = factor(label, levels = c("3 year pre-retrofit", "2054-2056 (RCP4.5)", "2054-2056 (RCP8.5)"))) %>%
  dplyr::mutate(temperature.bin = factor(temperature.bin, levels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=temperature.bin, y=days, fill=label)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::scale_fill_brewer(name="scenario", palette = "YlOrRd") +
  ggplot2::ylab("Avg. No. Days in Each Temp. Bin") +
  ggplot2::xlab("Temperature Bin (°F)") +
  ggplot2::ggtitle("Days in Temperature Bin Pre-retrofit (2005-2014) vs 2054-2056") +
  ggplot2::theme_light() +
  ggplot2::theme(legend.position="bottom")
ggsave(sprintf("%s/now_mid_century_bin_bar.png", imagedir), width=6, height=4)

## 2 bars per bin
now.weather %>%
  dplyr::bind_rows(midcentury.weather) %>%
  dplyr::group_by(temperature.bin) %>%
  dplyr::mutate(days = days - days[label == "3 year pre-retrofit"]) %>%
  dplyr::ungroup() %>%
  dplyr::filter(label != "3 year pre-retrofit") %>%
  dplyr::mutate(temperature.bin = factor(temperature.bin, levels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=temperature.bin, y=days, fill=label)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::scale_fill_manual(name="scenario", values= c("#FEB24C", "#F03B20")) +
  ggplot2::ylab("Avg. No. Days in Each Temp. Bin ") +
  ggplot2::xlab("Temperature Bin (°F)") +
  ggplot2::ggtitle("Difference in Days in Temperature Bin since 2005-2014") +
  ggplot2::theme_light() +
  ggplot2::theme(legend.position="bottom")
ggsave(sprintf("%s/mid_century_bin_bar_relative.png", imagedir), width=6, height=4)

load("../data/retrofit.energy.rda")

retrofit.energy %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(Year) %>%
  summary() %>%
  {.}
