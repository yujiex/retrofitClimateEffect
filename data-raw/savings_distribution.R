library("ggplot2")
library("dplyr")
library("kableExtra")

## helpers
## ratio: percent of subsample points in mid and late-century
violin.distribution.untreated <- function(df.plot.control, imagename,
                                          image.width, image.height=4, ratio=0.1) {
  title.string = sprintf("Effect distribution for the un-retrofitted under %s over different period", toupper(target.scenario))
  if (image.width < 5) {
    title.string = stringr::str_wrap(title.string, width=45)
  }
  points.now = df.plot.control %>%
    dplyr::filter(period == "now") %>%
    {.}
  subsample.points = df.plot.control %>%
    dplyr::filter(period != "now") %>%
    dplyr::group_by(action, fuel, period) %>%
    dplyr::slice(sample(1:n(), size=ratio * n(), replace=FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(points.now) %>%
    {.}
  if (stringr::str_detect(imagename, "_leed")) {
    ylabtext = "Estimated retrofit effect on LEED"
  } else {
    ylabtext = "Estimated effect (kBtu/sqft/year)"
  }
  df.plot.control %>%
    ggplot2::ggplot(ggplot2::aes(x = period, y=predictions, fill=period,
                                  group = interaction(action, period))) +
    ggplot2::geom_point(ggplot2::aes(x = period, y = predictions,
                                      fill=period, color=period,
                                      group=interaction(action, period)),
                        data=subsample.points, alpha = 0.3,
                        shape = 21, position = position_jitter(width=0.4),
                        size=0.01) +
    ggplot2::geom_violin(alpha=0.3, size=0.4,
                          position=position_dodge(width=0.5)) +
    ggplot2::geom_boxplot(width=0.1, position=position_dodge(width=0.5),
                          outlier.shape = NA) +
    ggplot2::geom_hline(yintercept=0, linetype = "dashed") +
    ggplot2::facet_grid(fuel~action) +
    ggplot2::scale_fill_brewer(palette = pal) +
    ggplot2::scale_color_brewer(palette = pal) +
    ggplot2::ylab(ylabtext) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(title.string) +
    ggplot2::theme(legend.position="bottom",
                    axis.text.x=element_blank(),
                    strip.text.x = element_text(size = 7),
                    plot.title=element_text(size=11))
  ggplot2::ggsave(imagename,
                  width=image.width, height=image.height)
}

## global settings
imagedir = "~/Dropbox/thesis/code/retrofitClimateEffect/images"
tabledir = "~/Dropbox/thesis/code/retrofitClimateEffect/tables"

## color palette for the plot of different time period
pal = "Dark2"

## suf = "_measured_input"
## suf = "_measured_input_leed"
suf = "_measured_input_lean"
plotkind = "vio"

## kw = "toplevel_bp"
## kw = "highlevel_bp"
## kw = "detaillevel_bp"
kw = "joint_highlevel_bp"
kw = paste0(kw, suf)

cf.result = readr::read_csv(sprintf("%s/grf_result_fewcol_%s.csv", tabledir, kw)) %>%
  dplyr::mutate_at(vars(period), recode,
                   "2050Jan through 2059Jan"="mid-century",
                   "2090Jan through 2099Jan"="late-century") %>%
  {.}

cf.result.significant = cf.result %>%
  dplyr::mutate(ci.low = predictions - variance.estimates,
                ci.high = predictions + variance.estimates) %>%
  dplyr::filter(sign(ci.low) == sign(ci.high)) %>%
  {.}

df.label = cf.result %>%
  dplyr::filter(period == "now") %>%
  dplyr::filter(is.real.retrofit==1) %>%
  dplyr::distinct(action, fuel, BLDGNUM, Substantial_Completion_Date) %>%
  dplyr::group_by(action, fuel) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity",
                   "baseload_GAS"="baseload gas",
                   "baseload_KWHR"="baseload electricity",
                   "htcl_GAS"="heating or cooling gas",
                   "htcl_KWHR"="heating or cooling electricity") %>%
  dplyr::mutate(action.label = paste0("n=", n),
                model = NA) %>%
  {.}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## for highlevel and toplevel
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
plotkind = "vio"
## distributions of the treated under current climate, using NOAA inputs
df.plot.treated = cf.result %>%
  dplyr::filter(period == "now") %>%
  dplyr::filter(is.real.retrofit==1) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                  "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity",
                   "baseload_GAS"="baseload gas",
                   "baseload_KWHR"="baseload electricity",
                   "htcl_GAS"="heating or cooling gas",
                   "htcl_KWHR"="heating or cooling electricity") %>%
  {.}
if (!stringr::str_detect(kw, "_leed")) {
  df.plot.treated <- df.plot.treated %>%
    ## annual
    dplyr::mutate(predictions = (-1)*predictions * 12) %>%
    {.}
}
if (stringr::str_detect(kw, "highlevel_bp")) {
  df.plot.treated <- df.plot.treated %>%
    dplyr::mutate(action = factor(action, levels = c("Advanced Metering", "GSALink", "Commissioning", "Building Envelope", "HVAC", "Lighting"))) %>%
    {.}
}
if (stringr::str_detect(kw, "_leed")) {
  titletext = "Effect distribution on LEED for the retrofitted \nwith measured weather input"
  axislabel = "Estimated retrofit effect on LEED"
} else if (stringr::str_detect(kw, "_lean")){
  titletext = "Effect distribution on dis-aggregated energy for the retrofitted \nwith measured weather input"
  axislabel = "Estimated retrofit effect (kBtu/sqft/year)"
} else {
  titletext = "Effect distribution on energy for the retrofitted \nwith measured weather input"
  axislabel = "Estimated retrofit effect (kBtu/sqft/year)"
}
if (plotkind == "vio") {
  if (stringr::str_detect(kw, "highlevel_bp")) {
    num.row = 2
    if (yvar.suf == "_leed") {
      image.width = 8
      image.height = 4
      facet.title.size = 7
      axis.x.size = 6
      title.size = 10
    } else if (yvar.suf == "_lean") {
      image.width = 12
      image.height = 6
      facet.title.size = 14
      axis.x.size = 8
      title.size = 16
    } else {
      image.width = 8
      image.height = 6
      facet.title.size = 7
      axis.x.size = 6
      title.size = 10
    }
  } else {
    image.width = 6
    if (yvar.suf == "_lean") {
      num.row = 2
      image.height = 5
    } else {
      num.row = 1
      image.height = 4
    }
  }
  imagename = sprintf("%s/retrofit_effect_cf_treated_slides_vio_%s.png",
                      imagedir, kw)
  print(image.width)
  print(image.height)
  df.plot.treated %>%
    ggplot2::ggplot(ggplot2::aes(x = action, y=predictions,
                                 group=interaction(action, fuel),
                                 fill=action)) +
    ggplot2::geom_point(shape = 21, position = position_jitter(), size=0.5)+
    ggplot2::geom_violin(alpha=0.3, size=0.4) +
    ggplot2::geom_boxplot(width=0.05, outlier.shape=NA) +
    ggplot2::geom_hline(yintercept=0, linetype = "dashed") +
    ## ggplot2::geom_rug() +
    ggplot2::facet_wrap(fuel~., nrow=num.row) +
    ggplot2::geom_text(size=3, data = df.label,
                       mapping=ggplot2::aes(x = action, y = 0, label=action.label),
                       hjust = -0.5, vjust = 1.2) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(titletext) +
    ggplot2::ylab(axislabel) +
    ggplot2::theme(legend.position="bottom",
                   title = element_text(size=title.size),
                   axis.text.x = element_text(size=axis.x.size),
                   strip.text.x = element_text(size = facet.title.size))
  ggplot2::ggsave(imagename, width=image.width, height=image.height)
} else {
  image.height = 4
  imagename = sprintf("%s/retrofit_effect_cf_treated_slides_%s.png",
                      imagedir, kw)
  df.plot.treated %>%
    ggplot2::ggplot(ggplot2::aes(x=predictions, group=interaction(action, fuel))) +
    ggplot2::geom_density(alpha=0.3, fill="grey", size=0.4) +
    ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
    ## ggplot2::geom_rug() +
    ggplot2::facet_grid(fuel ~ action) +
    ggplot2::scale_fill_brewer(name = "Whether retrofitted", palette = "Purples") +
    ggplot2::geom_text(size=3, data = df.label,
                      mapping=ggplot2::aes(x = Inf, y = -Inf, label=action.label),
                      hjust = -0.5, vjust = 1.2) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(titletext) +
    ggplot2::xlab(axislabel) +
    ggplot2::ylab("Probability density") +
    ggplot2::coord_flip() +
    ggplot2::theme(legend.position="bottom",
                  axis.text.x = element_text(size=6),
                  strip.text.x = element_text(size = 7))
  ggplot2::ggsave(imagename, width=image.width, height=image.height)
}

## distributions of the treated under current climate, using cmip5 inputs
for (target.scenario in c("rcp45", "rcp85")) {
  for (stacked in c(TRUE, FALSE)) {
    df.plot.treated = cf.result %>%
      dplyr::filter(scenario == target.scenario, period == "now") %>%
      dplyr::filter(is.real.retrofit==1) %>%
      dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                      "Building Tuneup or Utility Improvements"="Commissioning") %>%
      dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                      "GAS"="Gas", "KWHR"="Electricity") %>%
      ## annual
      dplyr::mutate(predictions = (-1)*predictions * 12) %>%
      {.}
    if (stacked) {
      imagename = sprintf("%s/retrofit_effect_cf_treated_slides_%s_%s_stack.png",
                          imagedir, target.scenario, kw)
      p <- df.plot.treated %>%
        ggplot2::ggplot(ggplot2::aes(x=predictions, group=interaction(action, fuel))) +
        ggplot2::geom_density(alpha=0.3, fill="grey", size=0.4)
    } else {
      imagename = sprintf("%s/retrofit_effect_cf_treated_slides_%s_%s.png",
                          imagedir, target.scenario, kw)
      p <- df.plot.treated %>%
        ggplot2::ggplot(ggplot2::aes(x=predictions, group=interaction(action, fuel, model))) +
        ggplot2::geom_density(alpha=0.3, fill="grey", size=0.2)
    }
    p +
      ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
      ## ggplot2::geom_rug() +
      ggplot2::facet_grid(fuel ~ action) +
      ggplot2::scale_fill_brewer(name = "Whether retrofitted", palette = "Purples") +
      ggplot2::geom_text(size=3, data = df.label,
                        mapping=ggplot2::aes(x = Inf, y = -Inf, label=action.label),
                        hjust = -0.5, vjust = 1.2) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(sprintf("Effect distribution for the retrofitted under %s", toupper(target.scenario))) +
      ggplot2::xlab("Estimated retrofit effect (kBtu/sqft/year)") +
      ggplot2::ylab("Probability density") +
      ggplot2::coord_flip() +
      ggplot2::theme(legend.position="bottom",
                    axis.text.x = element_text(size=6),
                    strip.text.x = element_text(size = 7))
    if (stringr::str_detect(kw, "highlevel_bp")) {
      image.width = 8
    } else if (stringr::str_detect(kw, "toplevel_bp")) {
      image.width = 6
    }
    ## many models
    ggplot2::ggsave(imagename, width=image.width, height=4)
  }
}

## distributions of the control under current, mid and late-century climate,
## using cf learned with NOAA weather input
for (target.scenario in c("rcp45", "rcp85")) {
    for (stacked in c(TRUE, FALSE)) {
    ## in sample prediction of the control, using NOAA inputs
    df.insample = cf.result %>%
      dplyr::filter(period == "now", scenario == "measured",
                    is.real.retrofit == 0) %>%
      dplyr::mutate(scenario = target.scenario) %>%
      {.}
    df.plot.control = cf.result %>%
      dplyr::filter(scenario == target.scenario, is.real.retrofit == 0) %>%
      dplyr::bind_rows(df.insample) %>%
      dplyr::mutate(period = factor(period, levels=c("now", "mid-century", "late-century"))) %>%
      dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                       "Building Tuneup or Utility Improvements"="Commissioning") %>%
      dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                       "GAS"="Gas", "KWHR"="Electricity",
                       "baseload_GAS"="baseload gas",
                       "baseload_KWHR"="baseload electricity",
                       "htcl_GAS"="heating or cooling gas",
                       "htcl_KWHR"="heating or cooling electricity") %>%
      {.}
    if (!stringr::str_detect(kw, "_leed")) {
      df.plot.control <- df.plot.control %>%
        ## annual
        dplyr::mutate(predictions = (-1)*predictions * 12) %>%
        {.}
    }
    if (stacked) {
      if (stringr::str_detect(kw, "highlevel_bp")) {
        image.width = 8
      } else if (stringr::str_detect(kw, "toplevel_bp")) {
        image.width = 6
      }
      if (stringr::str_detect(kw, "_lean")) {
        image.height = 8
      } else {
        image.height = 4
      }
      if (plotkind == "vio") {
        imagename = sprintf("%s/retrofit_effect_cf_control_slides_vio_%s_%s_stack.png", imagedir, target.scenario, kw)
        set.seed(0)
        violin.distribution.untreated(df.plot.control, imagename, image.width, image.height,
                                      ratio=0.1)
      } else {
        imagename = sprintf("%s/retrofit_effect_cf_control_slides_%s_%s_stack.png", imagedir, target.scenario, kw)
        df.plot.control %>%
          ggplot2::ggplot(ggplot2::aes(x=predictions, fill=period, color=period,
                                       group=interaction(action, fuel, period))) +
          ggplot2::geom_density(alpha=0.2, size=0.4) +
          ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
          ggplot2::facet_grid(fuel ~ action) +
          ggplot2::scale_fill_brewer(palette = pal) +
          ggplot2::scale_color_brewer(palette = pal) +
          ggplot2::ylab("Probability Density") +
          ggplot2::coord_flip() +
          ggplot2::theme_bw() +
          ggplot2::xlab(axislabel) +
          ggplot2::ggtitle(titletext) +
          ggplot2::theme(legend.position="bottom", axis.text.x=element_text(size=5),
                        strip.text.x = element_text(size = 7),
                        plot.title=element_text(size=11))
          ggplot2::ggsave(imagename,
                          width=image.width, height=image.height)
      }
    } else {
      image.width = 6
      actions = unique(cf.result$action)
      lapply(actions, function(target.action) {
        if (target.action =="Building Tuneup or Utility Improvements") {
          target.action = "Commissioning"
        }
        df.plot.control %>%
          dplyr::filter(action == target.action) %>%
          ## annual
          ggplot2::ggplot(ggplot2::aes(x=predictions, fill=period, color=period,
                                       group=interaction(action, fuel, model, period))) +
          ggplot2::geom_density(alpha=0.1, size=0.2) +
          ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
          ggplot2::facet_grid(fuel ~ period) +
          ggplot2::scale_fill_brewer(palette = pal) +
          ggplot2::scale_color_brewer(palette = pal) +
          ggplot2::ylab("Probability Density") +
          ggplot2::coord_flip() +
          ## ggplot2::coord_flip(ylim=c(0, 3)) +
          ggplot2::theme_bw() +
          ggplot2::ggtitle(titletext) +
          ggplot2::xlab(axislabel) +
          ## ggplot2::theme(legend.position="bottom", axis.text.x=element_text(size=5),
          ##               strip.text.x = element_text(size = 7),
          ##               plot.title=element_text(size=9))
          ggplot2::theme(legend.position="bottom",
                        axis.text.x = element_text(size=6),
                        strip.text.x = element_text(size = 6),
                        plot.title = element_text(size = 10),
                        legend.text = element_text(size=6))
        ## many models
        imagename = sprintf("%s/retrofit_effect_cf_control_slides_%s_%s_%s.png",
                            imagedir, target.action, target.scenario, kw)
        imagename <- gsub(" ", "-", imagename)
        ggplot2::ggsave(imagename, width=image.width, height=image.height)
      })
    }
  }
}

## distributions of the control under current, mid and late-century climate,
## using cf learned with cmip5 weather input
for (target.scenario in c("rcp45", "rcp85")) {
  for (stacked in c(TRUE, FALSE)) {
    df.plot.control = cf.result %>%
      dplyr::filter(scenario == target.scenario, is.real.retrofit == 0) %>%
      dplyr::mutate(period = factor(period, levels=c("now", "mid-century", "late-century"))) %>%
      dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                      "Building Tuneup or Utility Improvements"="Commissioning") %>%
      dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                      "GAS"="Gas", "KWHR"="Electricity") %>%
      dplyr::mutate(predictions = (-1)*predictions * 12) %>%
      {.}
    if (stacked) {
      df.plot.control %>%
        ggplot2::ggplot(ggplot2::aes(x=predictions, fill=period, color=period,
                                    group=interaction(action, fuel, period))) +
        ggplot2::geom_density(alpha=0.2, size=0.4) +
        ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
        ggplot2::facet_grid(fuel ~ action) +
        ggplot2::scale_fill_brewer(palette = pal) +
        ggplot2::scale_color_brewer(palette = pal) +
        ggplot2::ylab("Probability Density") +
        ggplot2::coord_flip() +
        ## ggplot2::coord_flip(ylim=c(0, 3)) +
        ggplot2::theme_bw() +
        ggplot2::xlab("Estimated effect (kBtu/sqft/year)") +
        ggplot2::ggtitle(sprintf("Effect distribution for the un-retrofitted under %s over different period",
                                toupper(target.scenario))) +
        ggplot2::theme(legend.position="bottom", axis.text.x=element_text(size=5),
                      strip.text.x = element_text(size = 7),
                      plot.title=element_text(size=11))
      if (kw == "highlevel_bp") {
        image.width = 8
      } else if (kw == "toplevel_bp") {
        image.width = 6
      }
      ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_control_slides_%s_%s_stack.png", imagedir, target.scenario, kw),
                      width=image.width, height=4)
    } else {
      actions = unique(cf.result$action)
      image.width = 6
      lapply(actions, function(target.action) {
        if (target.action =="Building Tuneup or Utility Improvements") {
          target.action = "Commissioning"
        }
        df.plot.control %>%
          dplyr::filter(action == target.action) %>%
          ## annual
          ggplot2::ggplot(ggplot2::aes(x=predictions, fill=period,
                                      color=period,
                                      group=interaction(action, fuel, model, period))) +
          ggplot2::geom_density(alpha=0.2, size=0.2) +
          ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
          ggplot2::facet_grid(fuel ~ period) +
          ggplot2::scale_fill_brewer(palette = pal) +
          ggplot2::scale_color_brewer(palette = pal) +
          ggplot2::ylab("Probability Density") +
          ggplot2::coord_flip() +
          ## ggplot2::coord_flip(ylim=c(0, 3)) +
          ggplot2::theme_bw() +
          ggplot2::ggtitle(sprintf("%s effect distribution for the un-retrofitted under %s over different period",
                                  target.action, toupper(target.scenario))) +
          ggplot2::xlab("Estimated effect (kBtu/sqft/year)") +
          ggplot2::theme(legend.position="bottom", axis.text.x=element_text(size=5),
                        strip.text.x = element_text(size = 7),
                        plot.title=element_text(size=9))
        ## many models
        imagename = sprintf("%s/retrofit_effect_cf_control_slides_%s_%s_%s.png",
                            imagedir, target.action, target.scenario, kw)
        imagename <- gsub(" ", "-", imagename)
        ggplot2::ggsave(imagename, width=image.width, height=4)
      })
    }
  }
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## for detail level
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## distributions of the treated under current climate, using NOAA inputs
if (stringr::str_detect(kw, "detaillevel")) {
  ## for detailed action pairs
  dfs.detailed.action = df.label %>%
    ## dplyr::filter(n>20) %>%
    tidyr::separate(action, into=c("l1", "l2", "l3"), sep="_") %>%
    dplyr::filter(l2 != "NA") %>%
    dplyr::group_by(l1, l3, fuel) %>%
    dplyr::filter(n()>1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(l1, l3, l2) %>%
    dplyr::group_by(l1, l3) %>%
    dplyr::group_split() %>%
    {.}
  lapply(dfs.detailed.action, function(action.to.plot) {
    image.suf <- paste(action.to.plot$l1[[1]], action.to.plot$l3[[1]], sep= "_")
    image.suf <- gsub(" ", "-", image.suf)
    print(image.suf)
    df.label.oneaction <- action.to.plot %>%
      tidyr::unite("action", l2:l3) %>%
      dplyr::mutate(action = gsub("_NA", "", action)) %>%
      {.}
    df.plot.treated = cf.result %>%
      dplyr::filter(period == "now") %>%
      dplyr::filter(is.real.retrofit==1) %>%
      dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                      "Building Tuneup or Utility Improvements"="Commissioning") %>%
      dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                       "GAS"="Gas", "KWHR"="Electricity",
                       "baseload_GAS"="baseload gas",
                       "baseload_KWHR"="baseload electricity",
                       "htcl_GAS"="heating or cooling gas",
                       "htcl_KWHR"="heating or cooling electricity") %>%
      tidyr::separate(action, into=c("l1", "l2", "l3"), sep="_") %>%
      dplyr::inner_join(action.to.plot %>% dplyr::select(-model), by=c("l1", "l2", "l3", "fuel")) %>%
      tidyr::unite("action", l2:l3) %>%
      ## annual
      dplyr::mutate(action = gsub("_NA", "", action)) %>%
      {.}
    if (!stringr::str_detect(kw, "_leed")) {
      df.plot.treated <- df.plot.treated %>%
        ## annual
        dplyr::mutate(predictions = (-1)*predictions * 12) %>%
        {.}
    }
    if (plotkind == "vio") {
      df.label.oneaction <- df.label.oneaction %>%
        dplyr::mutate(action = gsub("_", " ", action)) %>%
        dplyr::mutate(action = ifelse(stringr::word(action) %in% c("New", "Repairs", "Indoor", "Outdoor"), paste0(stringr::word(action), "\n", substr(action, stringr::str_locate(action, " ")[,1] + 1, nchar(action))), action)) %>%
        {.}
      imagename = sprintf("%s/retrofit_effect_cf_treated_slides_vio_%s_%s.png",
                          imagedir, image.suf, kw)
      df.plot.treated %>%
        dplyr::mutate(action = gsub("_", " ", action)) %>%
        dplyr::mutate(action = ifelse(stringr::word(action) %in% c("New", "Repairs", "Indoor", "Outdoor"), paste0(stringr::word(action), "\n", substr(action, stringr::str_locate(action, " ")[,1] + 1, nchar(action))), action)) %>%
        ggplot2::ggplot(ggplot2::aes(x = action, y=predictions,
                                     group=interaction(action, fuel),
                                     fill=action)) +
        ggplot2::geom_point(shape = 21, position = position_jitter(), size=0.5)+
        ggplot2::geom_violin(alpha=0.3, size=0.4) +
        ggplot2::geom_boxplot(width=0.05, outlier.shape=NA) +
        ggplot2::geom_hline(yintercept=0, linetype = "dashed") +
        ggplot2::facet_wrap(fuel~.) +
        ggplot2::geom_text(size=3, data = df.label.oneaction,
                           mapping=ggplot2::aes(x = action, y = 0, label=action.label),
                           hjust = -0.5, vjust = 1.2) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(titletext) +
        ggplot2::ylab(axislabel) +
        ggplot2::theme(legend.position="bottom",
                       legend.text = element_text(size = 8),
                       ## axis.text.x = element_text(size=6),
                       axis.text.x = element_blank(),
                       strip.text.x = element_text(size = 7),
                       plot.title = element_text(size = 8))
    } else {
      imagename = sprintf("%s/retrofit_effect_cf_treated_slides_%s_%s.png",
                          imagedir, image.suf, kw)
      df.plot.treated %>%
        ggplot2::ggplot(ggplot2::aes(x=predictions, group=interaction(action, fuel))) +
        ggplot2::geom_density(alpha=0.3, fill="grey", size=0.4) +
        ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
        ## ggplot2::geom_rug() +
        ggplot2::facet_grid(fuel ~ action) +
        ggplot2::geom_text(size=3, data =df.label.oneaction,
                           mapping=ggplot2::aes(x = Inf, y = -Inf, label=action.label),
                           hjust = -0.5, vjust = 1.2) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(titletext) +
        ggplot2::xlab(axislabel) +
        ggplot2::ylab("Probability density") +
        ggplot2::coord_flip() +
        ggplot2::theme(legend.position="bottom",
                       axis.text.x = element_text(size=6),
                       strip.text.x = element_text(size = 7),
                       plot.title = element_text(size = 8))
    }
    ggplot2::ggsave(imagename, width=4, height=4)
    return(NULL)
  })
}

## distributions of the treated under current climate, using cmip5 inputs
if (stringr::str_detect(kw, "detaillevel")) {
  ## for detailed action pairs
  dfs.detailed.action = df.label %>%
    dplyr::filter(n>20) %>%
    tidyr::separate(action, into=c("l1", "l2", "l3"), sep="_") %>%
    dplyr::filter(l2 != "NA") %>%
    dplyr::group_by(l1, l3, fuel) %>%
    dplyr::filter(n()>1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(l1, l3, l2) %>%
    dplyr::group_by(l1, l3) %>%
    dplyr::group_split() %>%
    {.}
  for (target.scenario in c("rcp45", "rcp85")) {
    for (stacked in c(TRUE, FALSE)) {
      print(length(dfs.detailed.action))
      lapply(dfs.detailed.action, function(action.to.plot) {
        image.suf <- paste(action.to.plot$l1[[1]], action.to.plot$l3[[1]], sep= "_")
        image.suf <- gsub(" ", "-", image.suf)
        df.label.oneaction <- action.to.plot %>%
          tidyr::unite("action", l2:l3) %>%
          dplyr::mutate(action = gsub("_NA", "", action)) %>%
          {.}
        df.plot.treated = cf.result %>%
          dplyr::filter(scenario == target.scenario, period == "now") %>%
          dplyr::filter(is.real.retrofit==1) %>%
          dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                          "Building Tuneup or Utility Improvements"="Commissioning") %>%
          dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                          "GAS"="Gas", "KWHR"="Electricity") %>%
          tidyr::separate(action, into=c("l1", "l2", "l3"), sep="_") %>%
          dplyr::inner_join(action.to.plot %>% dplyr::select(-model), by=c("l1", "l2", "l3", "fuel")) %>%
          tidyr::unite("action", l2:l3) %>%
          ## annual
          dplyr::mutate(predictions = (-1)*predictions * 12) %>%
          dplyr::mutate(action = gsub("_NA", "", action)) %>%
          {.}
        if (stacked) {
          imagename = sprintf("%s/retrofit_effect_cf_treated_slides_%s_%s_%s_stack.png",
                               imagedir, image.suf, target.scenario, kw)
          print(imagename)
          p <- df.plot.treated %>%
            ggplot2::ggplot(ggplot2::aes(x=predictions, group=interaction(action, fuel))) +
            ggplot2::geom_density(alpha=0.3, fill="grey", size=0.4)
        } else {
          imagename = sprintf("%s/retrofit_effect_cf_treated_slides_%s_%s_%s.png",
                               imagedir, image.suf, target.scenario, kw)
          print(imagename)
          p <- df.plot.treated %>%
            ggplot2::ggplot(ggplot2::aes(x=predictions, group=interaction(action, fuel, model))) +
            ggplot2::geom_density(alpha=0.3, fill="grey", size=0.2)
        }
        p +
          ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
          ## ggplot2::geom_rug() +
          ggplot2::facet_grid(fuel ~ action) +
          ggplot2::geom_text(size=3, data =df.label.oneaction,
                            mapping=ggplot2::aes(x = Inf, y = -Inf, label=action.label),
                            hjust = -0.5, vjust = 1.2) +
          ggplot2::theme_bw() +
          ggplot2::ggtitle(sprintf("Effect distribution on the retrofitted under %s",
                          toupper(target.scenario))) +
          ggplot2::xlab("Estimated retrofit effect (kBtu/sqft/year)") +
          ggplot2::ylab("Probability density") +
          ggplot2::coord_flip() +
          ggplot2::theme(legend.position="bottom",
                        axis.text.x = element_text(size=6),
                        strip.text.x = element_text(size = 7),
                        plot.title = element_text(size = 11))
        ggplot2::ggsave(imagename, width=4, height=4)
        return(NULL)
      })
    }
  }
}

## distributions of the control under current, mid and late-century climate
## using cf learned with NOAA weather input
if (stringr::str_detect(kw, "detaillevel")) {
  ## for detailed action pairs
  dfs.detailed.action = df.label %>%
    ## dplyr::filter(n>20) %>%
    tidyr::separate(action, into=c("l1", "l2", "l3"), sep="_") %>%
    dplyr::filter(l2 != "NA") %>%
    dplyr::group_by(l1, l3, fuel) %>%
    dplyr::filter(n()>1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(l1, l3, l2) %>%
    dplyr::group_by(l1, l3) %>%
    dplyr::group_split() %>%
    {.}
  print(length(dfs.detailed.action))
  for (target.scenario in c("rcp45", "rcp85")) {
    for (stacked in c(TRUE, FALSE)) {
      lapply(dfs.detailed.action, function(action.to.plot) {
        image.suf <- paste(action.to.plot$l1[[1]], action.to.plot$l3[[1]], sep= "_")
        image.suf <- gsub(" ", "-", image.suf)
        print(image.suf)
        df.label.oneaction <- action.to.plot %>%
          tidyr::unite("action", l2:l3) %>%
          dplyr::mutate(action = gsub("_NA", "", action)) %>%
          dplyr::mutate(period = NA) %>%
          dplyr::mutate(action = gsub("_", " ", action)) %>%
          {.}
        df.insample = cf.result %>%
          dplyr::filter(period == "now", scenario == "measured",
                        is.real.retrofit == 0) %>%
          dplyr::mutate(scenario = target.scenario) %>%
          {.}
        df.plot.control = cf.result %>%
          dplyr::filter(scenario == target.scenario) %>%
          dplyr::filter(is.real.retrofit==0) %>%
          dplyr::bind_rows(df.insample) %>%
          dplyr::mutate(period = factor(period, levels=c("now", "mid-century", "late-century"))) %>%
          dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                          "Building Tuneup or Utility Improvements"="Commissioning") %>%
          dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                           "GAS"="Gas", "KWHR"="Electricity",
                           "baseload_GAS"="baseload gas",
                           "baseload_KWHR"="baseload electricity",
                           "htcl_GAS"="heating or cooling gas",
                           "htcl_KWHR"="heating or cooling electricity") %>%
          tidyr::separate(action, into=c("l1", "l2", "l3"), sep="_") %>%
          dplyr::inner_join(action.to.plot %>% dplyr::select(-model), by=c("l1", "l2", "l3", "fuel")) %>%
          tidyr::unite("action", l2:l3) %>%
          dplyr::mutate(action = gsub("_NA", "", action)) %>%
          dplyr::mutate(action = gsub("_", " ", action)) %>%
          dplyr::arrange(period) %>%
          {.}
        if (!stringr::str_detect(kw, "_leed")) {
          df.plot.control <- df.plot.control %>%
            ## annual
            dplyr::mutate(predictions = (-1)*predictions * 12) %>%
            {.}
        }
        if (stringr::str_detect(kw, "_lean")) {
          image.height = 8
        } else {
          image.height = 4
        }
        if (stacked) {
          if (plotkind == "vio") {
            imagename =
              sprintf("%s/retrofit_effect_cf_control_slides_vio_%s_%s_%s_stack.png",
                      imagedir, image.suf, target.scenario, kw)
            violin.distribution.untreated(df.plot.control, imagename,
                                          image.width=4, image.height, ratio=0.1)
          } else {
            imagename =
              sprintf("%s/retrofit_effect_cf_control_slides_%s_%s_%s_stack.png",
                      imagedir, image.suf, target.scenario, kw)
            df.plot.control %>%
              ggplot2::ggplot(ggplot2::aes(x=predictions,
                                          fill=period,
                                          color=period,
                                          group=interaction(action, fuel, period))) +
              ggplot2::geom_density(alpha=0.2, size=0.4) +
              ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
              ggplot2::facet_grid(fuel ~ action) +
              ggplot2::theme_bw() +
              ggplot2::ggtitle(titletext) +
              ggplot2::scale_fill_brewer(palette=pal) +
              ggplot2::scale_color_brewer(palette=pal) +
              ggplot2::ylab(axislabel) +
              ggplot2::coord_flip() +
              ggplot2::theme(legend.position="bottom",
                            axis.text.x = element_text(size=6),
                            strip.text.x = element_text(size = 7),
                            plot.title = element_text(size = 11),
                            legend.text = element_text(size=6))
              ggplot2::theme()
            ggplot2::ggsave(imagename, width=4, height=4)
          }
        } else {
          allactions = unique(df.plot.control$action)
          lapply(allactions, function(target.action) {
            imagename =
              sprintf("%s/retrofit_effect_cf_control_slides_%s_%s_%s.png",
                      imagedir, gsub(" / ", "-or-", target.action), target.scenario, kw)
            imagename <- gsub(" ", "-", imagename)
            df.plot.control %>%
              dplyr::filter(action == target.action) %>%
              ggplot2::ggplot(ggplot2::aes(x=predictions, fill=period, color=period,
                                        group=interaction(action, fuel, model,
                                                          period))) +
              ggplot2::geom_density(alpha=0.1, size=0.2) +
              ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
              ggplot2::facet_grid(fuel ~ period) +
              ggplot2::theme_bw() +
              ggplot2::ggtitle(titletext) +
              ggplot2::scale_fill_brewer(palette=pal) +
              ggplot2::scale_color_brewer(palette=pal) +
              ggplot2::xlab(axislabel) +
              ggplot2::ylab("Probability density") +
              ggplot2::coord_flip() +
              ggplot2::theme(legend.position="bottom",
                             axis.text.x = element_text(size=6),
                             strip.text.x = element_text(size = 6),
                             plot.title = element_text(size = 10),
                             legend.text = element_text(size=6))
            ggplot2::ggsave(imagename, width=6, height=image.height)
            return(NULL)
          })
        }
      })
    }
  }
}

## distributions of the control under current, mid and late-century climate
## using cf learned with cmip5 weather input
if (stringr::str_detect(kw, "detaillevel")) {
  ## for detailed action pairs
  dfs.detailed.action = df.label %>%
    dplyr::filter(n>20) %>%
    tidyr::separate(action, into=c("l1", "l2", "l3"), sep="_") %>%
    dplyr::filter(l2 != "NA") %>%
    dplyr::group_by(l1, l3, fuel) %>%
    dplyr::filter(n()>1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(l1, l3, l2) %>%
    dplyr::group_by(l1, l3) %>%
    dplyr::group_split() %>%
    {.}
  print(length(dfs.detailed.action))
  for (target.scenario in c("rcp45", "rcp85")) {
      for (stacked in c(TRUE, FALSE)) {
      lapply(dfs.detailed.action, function(action.to.plot) {
        image.suf <- paste(action.to.plot$l1[[1]], action.to.plot$l3[[1]], sep= "_")
        image.suf <- gsub(" ", "-", image.suf)
        print(image.suf)
        df.label.oneaction <- action.to.plot %>%
          tidyr::unite("action", l2:l3) %>%
          dplyr::mutate(action = gsub("_NA", "", action)) %>%
          dplyr::mutate(period = NA) %>%
          {.}
        df.plot = cf.result %>%
          dplyr::filter(scenario == target.scenario) %>%
          dplyr::filter(is.real.retrofit==0) %>%
          dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                          "Building Tuneup or Utility Improvements"="Commissioning") %>%
          dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                          "GAS"="Gas", "KWHR"="Electricity") %>%
          tidyr::separate(action, into=c("l1", "l2", "l3"), sep="_") %>%
          dplyr::inner_join(action.to.plot %>% dplyr::select(-model), by=c("l1", "l2", "l3", "fuel")) %>%
          tidyr::unite("action", l2:l3) %>%
          ## annual
          dplyr::mutate(predictions = (-1)*predictions * 12) %>%
          dplyr::mutate(action = gsub("_NA", "", action)) %>%
          dplyr::mutate(period = ordered(period,
                                        levels=c("now", "mid-century", "late-century"))) %>%
          dplyr::arrange(period) %>%
          {.}
        if (stacked) {
          imagename =
            sprintf("%s/retrofit_effect_cf_control_slides_%s_%s_%s_stack.png",
                    imagedir, image.suf, target.scenario, kw)
          df.plot %>%
            ggplot2::ggplot(ggplot2::aes(x=predictions,
                                        fill=period,
                                        color=period,
                                        group=interaction(action, fuel, period))) +
            ggplot2::geom_density(alpha=0.2, size=0.4) +
            ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
            ggplot2::facet_grid(fuel ~ action) +
            ggplot2::theme_bw() +
            ggplot2::ggtitle(sprintf("Effect distribution on the retrofitted under %s",
                                    toupper(target.scenario))) +
            ggplot2::scale_fill_brewer(palette=pal) +
            ggplot2::scale_color_brewer(palette=pal) +
            ggplot2::xlab("Estimated retrofit effect (kBtu/sqft/year)") +
            ggplot2::ylab("Probability density") +
            ggplot2::coord_flip() +
            ggplot2::theme(legend.position="bottom",
                          axis.text.x = element_text(size=6),
                          strip.text.x = element_text(size = 7),
                          plot.title = element_text(size = 11),
                          legend.text = element_text(size=6))
            ggplot2::theme()
          ggplot2::ggsave(imagename, width=4, height=4)
        } else {
          allactions = unique(df.plot$action)
          lapply(allactions, function(target.action) {
            imagename =
              sprintf("%s/retrofit_effect_cf_control_slides_%s_%s_%s.png",
                      imagedir, gsub(" / ", "-or-", target.action), target.scenario, kw)
            imagename <- gsub(" ", "-", imagename)
            df.plot %>%
              dplyr::filter(action == target.action) %>%
              ggplot2::ggplot(ggplot2::aes(x=predictions, fill=period, color=period,
                                        group=interaction(action, fuel, model,
                                                          period))) +
              ggplot2::geom_density(alpha=0.2, size=0.4) +
              ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
              ggplot2::facet_grid(fuel ~ period) +
              ggplot2::theme_bw() +
              ggplot2::labs(title = gsub("_", " ", target.action),
                            subtitle = sprintf("Effect distribution on the retrofitted under %s",
                                               toupper(target.scenario))) +
              ggplot2::scale_fill_brewer(palette=pal) +
              ggplot2::scale_color_brewer(palette=pal) +
              ggplot2::xlab("Estimated retrofit effect (kBtu/sqft/year)") +
              ggplot2::ylab("Probability density") +
              ggplot2::coord_flip() +
              ggplot2::theme(legend.position="bottom",
                            axis.text.x = element_text(size=6),
                            strip.text.x = element_text(size = 6),
                            plot.title = element_text(size = 11),
                            legend.text = element_text(size=6))
              ggplot2::theme()
            ggplot2::ggsave(imagename, width=4, height=4)
            return(NULL)
          })
        }
      })
    }
  }
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## joint high-level
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
if (stringr::str_detect(kw, "joint_highlevel")) {
  cutoff = 20
  actions.to.plot = df.label %>%
    dplyr::filter(n>cutoff) %>%
    dplyr::filter(stringr::str_detect(action, " and ")) %>%
    dplyr::arrange(action, fuel) %>%
    dplyr::mutate(action = gsub(" and ", " & ", action)) %>%
    dplyr::mutate(action = gsub("Advanced Metering", "A", action)) %>%
    dplyr::mutate(action = gsub("Building Envelope", "B", action)) %>%
    dplyr::mutate(action = gsub("Building Tuneup or Utility Improvements", "C", action)) %>%
    dplyr::mutate(action = gsub("HVAC", "H", action)) %>%
    dplyr::mutate(action = gsub("Lighting", "L", action)) %>%
    distinct(action) %>%
    {.}
  joint.to.plot = df.label %>%
    dplyr::mutate(action = gsub(" and ", " & ", action)) %>%
    dplyr::mutate(action = gsub("Advanced Metering", "A", action)) %>%
    dplyr::mutate(action = gsub("Building Envelope", "B", action)) %>%
    dplyr::mutate(action = gsub("Building Tuneup or Utility Improvements", "C", action)) %>%
    dplyr::mutate(action = gsub("HVAC", "H", action)) %>%
    dplyr::mutate(action = gsub("Lighting", "L", action)) %>%
    dplyr::inner_join(actions.to.plot, by="action") %>%
    {.}
  df.plot = cf.result %>%
    dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                     "GAS"="Gas", "KWHR"="Electricity",
                     "baseload_GAS"="baseload gas",
                     "baseload_KWHR"="baseload electricity",
                     "htcl_GAS"="heating or cooling gas",
                     "htcl_KWHR"="heating or cooling electricity") %>%
    dplyr::mutate(action = gsub(" and ", " & ", action)) %>%
    dplyr::mutate(action = gsub("Advanced Metering", "A", action)) %>%
    dplyr::mutate(action = gsub("Building Envelope", "B", action)) %>%
    dplyr::mutate(action = gsub("Building Tuneup or Utility Improvements", "C", action)) %>%
    dplyr::mutate(action = gsub("HVAC", "H", action)) %>%
    dplyr::mutate(action = gsub("Lighting", "L", action)) %>%
    dplyr::inner_join(joint.to.plot %>% dplyr::select(-model),
                      by=c("action", "fuel")) %>%
    dplyr::mutate(action = gsub("_NA", "", action)) %>%
    {.}
  if (!stringr::str_detect(kw, "_leed")) {
    df.plot <- df.plot %>%
      ## annual
      dplyr::mutate(predictions = (-1)*predictions * 12) %>%
      {.}
  }
}

## distributions of the treated under current climate, using NOAA inputs
if (stringr::str_detect(kw, "joint_highlevel")) {
  df.plot.treated <- df.plot %>%
    dplyr::filter(is.real.retrofit==1) %>%
    dplyr::filter(period == "now") %>%
    {.}
  if (stringr::str_detect(kw, "_leed")) {
    image.width = 4
  } else {
    image.width = 6
  }
  if (stringr::str_detect(kw, "_lean")) {
    image.height = 5
  } else {
    image.height = 4
  }
  if (plotkind == "vio") {
    imagename = sprintf("%s/retrofit_effect_cf_treated_slides_vio_%s.png", imagedir, kw)
    df.plot.treated %>%
      ggplot2::ggplot(ggplot2::aes(x = action, y=predictions,
                                  group=interaction(action, fuel),
                                  fill=action)) +
      ggplot2::geom_point(shape = 21, position = position_jitter(), size=0.5)+
      ggplot2::geom_violin(alpha=0.3, size=0.4) +
      ggplot2::geom_boxplot(width=0.05, outlier.shape=NA) +
      ggplot2::geom_hline(yintercept=0, linetype = "dashed") +
      ggplot2::facet_wrap(fuel~.) +
      ggplot2::geom_text(size=3, data = joint.to.plot,
                         mapping=ggplot2::aes(x = action, y = 0, label=action.label),
                         hjust = -0.5, vjust = 1.2) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(titletext) +
      ggplot2::ylab(axislabel) +
      guides(fill=ggplot2::guide_legend(ncol=2)) +
      ggplot2::theme(legend.position="bottom",
                     axis.text.x = element_text(size=6),
                     strip.text.x = element_text(size = 7),
                     plot.title = element_text(size = 12))
    ggplot2::ggsave(imagename, width=image.width, height=image.height)
  } else {
    imagename = sprintf("%s/retrofit_effect_cf_treated_slides_%s.png", imagedir, kw)
    linesize = 0.4
    df.plot.treated %>%
      ggplot2::ggplot(ggplot2::aes(x=predictions,
                                  group=interaction(action, fuel))) +
      ggplot2::geom_density(alpha=0.3, fill="grey", size=linesize) +
      ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
      ## ggplot2::geom_rug() +
      ggplot2::facet_grid(fuel ~ action) +
      ggplot2::scale_fill_brewer(name = "Whether retrofitted", palette = "Purples") +
      ggplot2::geom_text(size=3, data =joint.to.plot,
                        mapping=ggplot2::aes(x = Inf, y = -Inf, label=action.label),
                        hjust = -0.5, vjust = 1.2) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(titletext) +
      ggplot2::xlab(axislabel) +
      ggplot2::ylab("Probability density") +
      ggplot2::coord_flip() +
      ggplot2::theme(legend.position="bottom",
                      axis.text.x = element_text(size=6),
                      strip.text.x = element_text(size = 7),
                      plot.title = element_text(size = 12))
    ggplot2::ggsave(imagename, width=image.width, height=image.height)
  }
}

## distributions of the treated under current climate, using cmip5 inputs
if (stringr::str_detect(kw, "joint_highlevel")) {
  for (target.scenario in c("rcp45", "rcp85")) {
    df.plot.treated <- df.plot %>%
      dplyr::filter(is.real.retrofit==1) %>%
      dplyr::filter(scenario == target.scenario, period == "now") %>%
      {.}
    for (stacked in c(TRUE, FALSE)) {
      if (stacked) {
        imagename = sprintf("%s/retrofit_effect_cf_treated_slides_%s_%s_stack.png", imagedir, target.scenario, kw)
        linesize = 0.4
        p <- df.plot.treated %>%
          ggplot2::ggplot(ggplot2::aes(x=predictions,
                                      group=interaction(action, fuel)))
      } else {
        imagename = sprintf("%s/retrofit_effect_cf_treated_slides_%s_%s.png", imagedir, target.scenario, kw)
        linesize = 0.2
        p <- df.plot.treated %>%
          ggplot2::ggplot(ggplot2::aes(x=predictions,
                                      group=interaction(action, fuel, model)))
      }
      p +
        ggplot2::geom_density(alpha=0.3, fill="grey", size=linesize) +
        ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
        ## ggplot2::geom_rug() +
        ggplot2::facet_grid(fuel ~ action) +
        ggplot2::scale_fill_brewer(name = "Whether retrofitted", palette = "Purples") +
        ggplot2::geom_text(size=3, data =joint.to.plot,
                          mapping=ggplot2::aes(x = Inf, y = -Inf, label=action.label),
                          hjust = -0.5, vjust = 1.2) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle("Distribution of treatment effect for the retrofitted") +
        ggplot2::xlab("Estimated retrofit effect (kBtu/sqft/year)") +
        ggplot2::ylab("Probability density") +
        ggplot2::coord_flip() +
        ggplot2::theme(legend.position="bottom",
                        axis.text.x = element_text(size=6),
                        strip.text.x = element_text(size = 7),
                        plot.title = element_text(size = 12))
      ggplot2::ggsave(imagename, width=6, height=4)
    }
  }
}

## distributions of the control under current, mid and late-century climate
## using cf learned with noaa weather input
if (stringr::str_detect(kw, "joint_highlevel")) {
  for (target.scenario in c("rcp45", "rcp85")) {
    df.insample = df.plot %>%
      dplyr::filter(period == "now", scenario == "measured",
                    is.real.retrofit == 0) %>%
      dplyr::mutate(scenario = target.scenario) %>%
      {.}
    df.plot.control <- df.plot %>%
      dplyr::filter(scenario == target.scenario) %>%
      dplyr::filter(is.real.retrofit == 0) %>%
      dplyr::bind_rows(df.insample) %>%
      dplyr::mutate(period = factor(period,
                                    levels=c("now", "mid-century",
                                             "late-century"))) %>%
      {.}
    if (stringr::str_detect(kw, "_lean")) {
      image.height = 8
    } else {
      image.height = 4
    }
    for (stacked in c(TRUE, FALSE)) {
      if (stacked) {
        if (plotkind == "vio") {
          imagename = sprintf("%s/retrofit_effect_cf_control_slides_vio_%s_%s_stack.png", imagedir, target.scenario, kw)
          set.seed(0)
          violin.distribution.untreated(df.plot.control, imagename, image.width=6, image.height,
                                        ratio=0.1)
        } else {
          df.plot.control %>%
            ggplot2::ggplot(ggplot2::aes(x=predictions, fill=period, color=period,
                                        group=interaction(action, fuel, period))) +
            ggplot2::geom_density(alpha=0.2, size=0.4) +
            ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
            ggplot2::facet_grid(fuel ~ action) +
            ggplot2::scale_fill_brewer(palette = pal) +
            ggplot2::scale_color_brewer(palette = pal) +
            ggplot2::ylab("Probability Density") +
            ggplot2::coord_flip() +
            ggplot2::theme_bw() +
            ggplot2::xlab(axislabel) +
            ggplot2::ggtitle(titletext) +
            ggplot2::theme(legend.position="bottom", axis.text.x=element_text(size=5),
                          strip.text.x = element_text(size = 7),
                          plot.title=element_text(size=11))
          ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_control_slides_%s_%s_stack.png",
                                  imagedir, target.scenario, kw),
                          width=image.width, height=image.height)
        }
      } else {
        actions = unique(df.plot$action)
        image.width = 6
        lapply(actions, function(target.action) {
          imagename = sprintf("%s/retrofit_effect_cf_control_slides_%s_%s_%s.png",
                              imagedir, target.action, target.scenario, kw)
          imagename <- gsub(" & ", "-", imagename)
          if (stringr::str_detect(kw, "_leed")) {
            titletext = sprintf("%s effect distribution on LEED for the un-retrofitted \nunder %s over different period", target.action, toupper(target.scenario))
          } else {
            titletext = sprintf("%s effect distribution on energy for the un-retrofitted \nunder %s over different period", target.action, toupper(target.scenario))
          }
          df.plot.control %>%
            dplyr::filter(action == target.action) %>%
            ggplot2::ggplot(ggplot2::aes(x=predictions, fill=period,
                                        color=period,
                                        group=interaction(action, fuel, model, period))) +
            ggplot2::geom_density(alpha=0.2, size=0.2) +
            ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
            ggplot2::facet_grid(fuel ~ period) +
            ggplot2::scale_fill_brewer(palette = pal) +
            ggplot2::scale_color_brewer(palette = pal) +
            ggplot2::ylab("Probability Density") +
            ggplot2::coord_flip() +
            ## ggplot2::coord_flip(ylim=c(0, 3)) +
            ggplot2::theme_bw() +
            ggplot2::ggtitle(titletext) +
            ggplot2::xlab(axislabel) +
            ggplot2::theme(legend.position="bottom",
                          axis.text.x=element_text(size=5),
                          strip.text.x = element_text(size = 7),
                          plot.title=element_text(size=9))
          ggplot2::ggsave(imagename,
                          width=image.width, height=image.height)
        })
      }
    }
  }
}

## distributions of the control under current, mid and late-century climate
## using cf learned with cmip5 weather input
if (stringr::str_detect(kw, "joint_highlevel")) {
  for (target.scenario in c("rcp45", "rcp85")) {
    df.plot.control <- df.plot %>%
      dplyr::filter(scenario == target.scenario) %>%
      dplyr::filter(is.real.retrofit == 0) %>%
      dplyr::mutate(period = factor(period, levels=c("now", "mid-century", "late-century"))) %>%
      {.}
    for (stacked in c(FALSE)) {
      ## for (stacked in c(TRUE, FALSE)) {
      if (stacked) {
        df.plot.control %>%
          ggplot2::ggplot(ggplot2::aes(x=predictions, fill=period, color=period,
                                      group=interaction(action, fuel, period))) +
          ggplot2::geom_density(alpha=0.2, size=0.4) +
          ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
          ggplot2::facet_grid(fuel ~ action) +
          ggplot2::scale_fill_brewer(palette = pal) +
          ggplot2::scale_color_brewer(palette = pal) +
          ggplot2::ylab("Probability Density") +
          ggplot2::coord_flip() +
          ggplot2::theme_bw() +
          ggplot2::xlab("Estimated effect (kBtu/sqft/year)") +
          ggplot2::ggtitle(sprintf("Effect distribution for the un-retrofitted under %s over different period",
                                  toupper(target.scenario))) +
          ggplot2::theme(legend.position="bottom", axis.text.x=element_text(size=5),
                        strip.text.x = element_text(size = 7),
                        plot.title=element_text(size=11))
        ggplot2::ggsave(sprintf("%s/retrofit_effect_cf_control_slides_%s_%s_stack.png",
                                imagedir, target.scenario, kw),
                        width=image.width, height=4)
      } else {
        actions = unique(df.plot$action)
        image.width = 6
        lapply(actions, function(target.action) {
          imagename = sprintf("%s/retrofit_effect_cf_control_slides_%s_%s_%s.png",
                              imagedir, target.action, target.scenario, kw)
          imagename <- gsub(" & ", "-", imagename)
          df.plot.control %>%
            dplyr::filter(action == target.action) %>%
            ggplot2::ggplot(ggplot2::aes(x=predictions, fill=period,
                                        color=period,
                                        group=interaction(action, fuel, model, period))) +
            ggplot2::geom_density(alpha=0.2, size=0.2) +
            ggplot2::geom_vline(xintercept=0, linetype = "dashed") +
            ggplot2::facet_grid(fuel ~ period) +
            ggplot2::scale_fill_brewer(palette = pal) +
            ggplot2::scale_color_brewer(palette = pal) +
            ggplot2::ylab("Probability Density") +
            ggplot2::coord_flip() +
            ## ggplot2::coord_flip(ylim=c(0, 3)) +
            ggplot2::theme_bw() +
            ggplot2::ggtitle(sprintf("%s effect distribution for the un-retrofitted under %s over different period",
                                    target.action, toupper(target.scenario))) +
            ggplot2::xlab("Estimated effect (kBtu/sqft/year)") +
            ggplot2::theme(legend.position="bottom", axis.text.x=element_text(size=5),
                          strip.text.x = element_text(size = 7),
                          plot.title=element_text(size=9))
          ## many models
          ggplot2::ggsave(imagename,
                          width=image.width, height=4)
        })
      }
    }
  }
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## generate image tex files
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
cf.result = readr::read_csv(sprintf("%s/grf_result_fewcol_detaillevel_bp.csv", tabledir))
df.label = cf.result %>%
  dplyr::filter(period == "now") %>%
  dplyr::filter(is.real.retrofit==1) %>%
  dplyr::distinct(action, fuel, BLDGNUM, Substantial_Completion_Date) %>%
  dplyr::group_by(action, fuel) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                   "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                   "GAS"="Gas", "KWHR"="Electricity",
                   "baseload_GAS"="baseload gas",
                   "baseload_KWHR"="baseload electricity",
                   "htcl_GAS"="heating or cooling gas",
                   "htcl_KWHR"="heating or cooling electricity") %>%
  dplyr::mutate(action.label = paste0("n=", n),
                model = NA) %>%
  {.}

action.detail = df.label %>%
  ## dplyr::filter(n>20) %>%
  tidyr::separate(action, into=c("l1", "l2", "l3"), sep="_") %>%
  dplyr::filter(l2 != "NA") %>%
  dplyr::group_by(l1, l3, fuel) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(l1, l3, l2) %>%
  {.}

suf.actions.detail = action.detail %>%
  dplyr::mutate(imagesuf = paste0(l1, "_", l3)) %>%
  dplyr::mutate(imagesuf = gsub(" / ", "-or-", imagesuf)) %>%
  dplyr::mutate(imagesuf = gsub(" ", "-", imagesuf)) %>%
  dplyr::distinct(imagesuf) %>%
  .$imagesuf %>%
  {.}

suf.actions.detail.subaction = action.detail %>%
  dplyr::mutate(imagesuf = paste0(l2, "-", l3)) %>%
  dplyr::mutate(imagesuf = gsub(" / ", "-or-", imagesuf)) %>%
  dplyr::mutate(imagesuf = gsub(" ", "-", imagesuf)) %>%
  dplyr::mutate(imagesuf = gsub("-NA", "", imagesuf)) %>%
  dplyr::distinct(imagesuf) %>%
  .$imagesuf %>%
  {.}

suf.actions.toplevel = readr::read_csv(sprintf("%s/grf_result_fewcol_toplevel_bp.csv", tabledir)) %>%
  dplyr::distinct(action) %>%
  dplyr::mutate(action = gsub(" ", "-", action)) %>%
  .$action %>%
  {.}

suf.actions.highlevel = readr::read_csv(sprintf("%s/grf_result_fewcol_highlevel_bp.csv", tabledir)) %>%
  dplyr::distinct(action) %>%
  dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                  "Building Tuneup or Utility Improvements"="Commissioning") %>%
  dplyr::mutate(action = gsub(" ", "-", action)) %>%
  .$action %>%
  {.}

suf.actions.jointhighlevel = c("A-B-C-H-L", "A-C-H-L", "C-H-L")

## generate file for the results of models learned from cmip5 imputs
for (target.scenario in c("rcp45", "rcp85")) {
  for (treat.status in c("treated", "control")) {
    for (stack.suf in c("_stack", "")) {
      if (stack.suf == "_stack" || treat.status == "treated") {
        images = sprintf("retrofit_effect_cf_%s_slides_%s_%s%s.png", treat.status, target.scenario, c("toplevel_bp", "highlevel_bp", "joint_highlevel_bp"), stack.suf)
        ## detailed action
        images <- c(images, sprintf("retrofit_effect_cf_%s_slides_%s_%s_detaillevel_bp%s.png", treat.status, suf.actions.detail, target.scenario, stack.suf))
      } else {
        images = c(sprintf("retrofit_effect_cf_%s_slides_%s_%s_toplevel_bp%s.png",
                           treat.status, suf.actions.toplevel, target.scenario,
                           stack.suf),
                   sprintf("retrofit_effect_cf_%s_slides_%s_%s_highlevel_bp%s.png",
                           treat.status, suf.actions.highlevel, target.scenario,
                           stack.suf),
                   sprintf("retrofit_effect_cf_%s_slides_%s_%s_joint_highlevel_bp%s.png",
                           treat.status, suf.actions.jointhighlevel,
                           target.scenario, stack.suf),
                   sprintf("retrofit_effect_cf_%s_slides_%s_%s_detaillevel_bp%s.png",
                           treat.status, suf.actions.detail.subaction,
                           target.scenario, stack.suf))
      }
      ## generate image tex file
      contents = sapply(images, function(imagei) {
        newlines <- c("\\begin{figure}[H]",
                      "\\centering",
                      sprintf("\\includegraphics[width=0.9\\linewidth]{../images/%s}",
                              imagei),
                      "\\end{figure}")
        return(newlines)
      })
      con <- file(sprintf("../images/cf_%s_cmip5_%s%s.tex", treat.status, target.scenario, stack.suf), open = "w+")
      writeLines(contents, con, sep = "\n", useBytes = FALSE)
      close(con)
    }
  }
}

## generate file for the results of models learned from noaa imputs
## treated
treat.status = "treated"
for (yvar.suf in c("", "_leed", "_lean")) {
  images = sprintf("retrofit_effect_cf_%s_slides_vio_%s%s.png", treat.status, c("toplevel_bp_measured_input", "highlevel_bp_measured_input", "joint_highlevel_bp_measured_input"), yvar.suf)
  ## detailed action
  ## generate image tex file
  contents = sapply(images, function(imagei) {
    newlines <- c("\\begin{figure}[H]",
                  "\\centering",
                  sprintf("\\includegraphics[width=0.9\\linewidth]{../images/%s}",
                          imagei),
                  "\\end{figure}")
    return(newlines)
  })
  detail.images <- c(sprintf("retrofit_effect_cf_%s_slides_vio_%s_detaillevel_bp_measured_input%s.png", treat.status, suf.actions.detail, yvar.suf))
  detail.contents = sapply(detail.images, function(imagei) {
    newlines <- c("\\begin{figure}[H]",
                  "\\centering",
                  ## thinner width for detail level analysis
                  sprintf("\\includegraphics[width=0.6\\linewidth]{../images/%s}",
                          imagei),
                  "\\end{figure}")
    return(newlines)
  })
  con <- file(sprintf("../images/cf_%s_noaa%s.tex", treat.status, yvar.suf), open = "w+")
  writeLines(c(contents, detail.contents), con, sep = "\n", useBytes = FALSE)
  close(con)
}

## control
treat.status = "control"
for (yvar.suf in c("", "_leed", "_lean")) {
  for (target.scenario in c("rcp45", "rcp85")) {
    for (stack.suf in c("_stack", "")) {
      if (stack.suf == "_stack") {
        images = sprintf("retrofit_effect_cf_%s_slides_vio_%s_%s%s%s.png", treat.status, target.scenario, c("toplevel_bp_measured_input", "highlevel_bp_measured_input", "joint_highlevel_bp_measured_input"), yvar.suf, stack.suf)
        ## detailed action
        detail.images <- sprintf("retrofit_effect_cf_%s_slides_vio_%s_%s_detaillevel_bp_measured_input%s%s.png", treat.status, suf.actions.detail, target.scenario, yvar.suf, stack.suf)
        ## generate image tex file
        contents = sapply(images, function(imagei) {
          newlines <- c("\\begin{figure}[H]",
                        "\\centering",
                        sprintf("\\includegraphics[width=0.9\\linewidth]{../images/%s}",
                                imagei),
                        "\\end{figure}")
          return(newlines)
        })
        detail.contents = sapply(detail.images, function(imagei) {
          newlines <- c("\\begin{figure}[H]",
                        "\\centering",
                        sprintf("\\includegraphics[width=0.6\\linewidth]{../images/%s}",
                                imagei),
                        "\\end{figure}")
          return(newlines)
        })
        contents <- c(contents, detail.contents)
      } else {
        images = c(sprintf("retrofit_effect_cf_%s_slides_%s_%s_toplevel_bp_measured_input%s%s.png",
                          treat.status, suf.actions.toplevel, target.scenario, yvar.suf,
                          stack.suf),
                  sprintf("retrofit_effect_cf_%s_slides_%s_%s_highlevel_bp_measured_input%s%s.png",
                                              treat.status, suf.actions.highlevel, target.scenario,
                                              yvar.suf, stack.suf),
                  sprintf("retrofit_effect_cf_%s_slides_%s_%s_joint_highlevel_bp_measured_input%s%s.png",
                          treat.status, suf.actions.jointhighlevel,
                          target.scenario, yvar.suf, stack.suf),
                  sprintf("retrofit_effect_cf_%s_slides_%s_%s_detaillevel_bp_measured_input%s%s.png",
                          treat.status, suf.actions.detail.subaction,
                          target.scenario, yvar.suf, stack.suf))
        ## generate image tex file
        contents = sapply(images, function(imagei) {
          newlines <- c("\\begin{figure}[H]",
                        "\\centering",
                        sprintf("\\includegraphics[width=0.9\\linewidth]{../images/%s}",
                                imagei),
                        "\\end{figure}")
          return(newlines)
        })
      }
      con <- file(sprintf("../images/cf_%s_noaa_%s%s%s.tex", treat.status, target.scenario, yvar.suf, stack.suf), open = "w+")
      writeLines(contents, con, sep = "\n", useBytes = FALSE)
      close(con)
    }
  }
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## Compare savings distribution in a summary table
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

actionkw = "detaillevel"
## actionkw = "joint_highlevel"
files <- list.files(path="../tables/",
                    pattern=sprintf("grf_result_fewcol_%s_bp_lean*", actionkw))
lean.result.agg <- lapply(files, function(f) {
  readr::read_csv(paste0("../tables/", f)) %>%
    {.}
}) %>%
  dplyr::bind_rows() %>%
{.}

lean.result.agg %>%
  readr::write_csv(sprintf("../tables/grf_result_fewcol_%s_bp_lean.csv", actionkw))

## ## Summary of energy
## yvar.suf = ""
## Summary of leed
## yvar.suf = "_leed"
yvar.suf = "_lean"
for (kw in
     c("toplevel_bp", "highlevel_bp", "detaillevel_bp", "joint_highlevel_bp")) {
  cf.result.cmip5 = readr::read_csv(sprintf("%s/grf_result_fewcol_%s%s.csv", tabledir, kw, yvar.suf)) %>%
    dplyr::mutate(input.source = "cmip5") %>%
    {.}
  cf.result.noaa = readr::read_csv(sprintf("%s/grf_result_fewcol_%s_measured_input%s.csv", tabledir, kw, yvar.suf)) %>%
    dplyr::mutate(input.source = "noaa") %>%
    {.}
  in.sample = cf.result.noaa %>%
    dplyr::filter(scenario == "measured")
  in.sample.45 <- in.sample %>%
    dplyr::mutate(scenario = "rcp45") %>%
    {.}
  in.sample.85 <- in.sample %>%
    dplyr::mutate(scenario = "rcp85") %>%
    {.}
  to.summarise = cf.result.noaa %>%
    dplyr::filter(scenario != "measured") %>%
    dplyr::bind_rows(in.sample.45) %>%
    dplyr::bind_rows(in.sample.85) %>%
    dplyr::bind_rows(cf.result.cmip5) %>%
    dplyr::mutate_at(vars(period), recode,
                    "2050Jan through 2059Jan"="mid-century",
                    "2090Jan through 2099Jan"="late-century") %>%
    dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                    "Building Tuneup or Utility Improvements"="Commissioning") %>%
    dplyr::mutate_at(dplyr::vars(fuel), dplyr::recode,
                    "GAS"="Gas", "KWHR"="Electricity") %>%
    {.}
  if (stringr::str_detect(kw, "joint_highlevel")) {
    to.summarise <- to.summarise %>%
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
  if (yvar.suf != "_leed") {
    to.summarise <- to.summarise %>%
      dplyr::mutate(predictions = (-1)*predictions * 12) %>%
      {.}
  }
  to.summarise %>%
    dplyr::group_by(fuel, action, period, scenario, is.real.retrofit,
                    input.source) %>%
    dplyr::summarise_at(vars(predictions),
                        tibble::lst(min, mean, median, max, sd)) %>%
    dplyr::ungroup() %>%
    ## tidyr::pivot_wider(names_from=input.source, values_from=min:sd) %>%
    readr::write_csv(sprintf("savings_cmip5_vs_noaa_%s%s.csv", kw, yvar.suf))
}

## output summary table to document
for (yvar.suf in c("", "_leed", "_lean")) {
  for (kw in
      c("toplevel_bp", "highlevel_bp", "joint_highlevel_bp", "detaillevel_bp")) {
    kw = paste0(kw, yvar.suf)
    summaryfile = readr::read_csv(sprintf("savings_cmip5_vs_noaa_%s.csv", kw))
    if (stringr::str_detect(kw, "detaillevel_bp")) {
      summaryfile <- summaryfile %>%
        tidyr::separate(action, into=c("action", "l2", "l3"), sep="_") %>%
        tidyr::unite(col="sub action", l2, l3, sep=" ") %>%
        dplyr::mutate(`sub action`=gsub("NA", "", `sub action`)) %>%
        {.}
    }
    summary.treated <- summaryfile %>%
      dplyr::filter(period == "now", is.real.retrofit == 1,
                    input.source == "noaa") %>%
      dplyr::select(-scenario, -period, -input.source, -is.real.retrofit) %>%
      dplyr::distinct() %>%
      dplyr::mutate_at(vars(min:sd), function(x) {round(x, 3)}) %>%
      {.}
    summary.control <- summaryfile %>%
      dplyr::filter(period != "now", is.real.retrofit == 0,
                    input.source == "noaa") %>%
      dplyr::select(-input.source, -is.real.retrofit) %>%
      dplyr::distinct() %>%
      dplyr::mutate_at(vars(min:sd), function(x) {round(x, 3)}) %>%
      {.}
    if (stringr::str_detect(kw, "detaillevel_bp")) {
      summary.treated <- summary.treated %>%
        dplyr::arrange(fuel, action, `sub action`) %>%
        {.}
      summary.control <- summary.control %>%
        dplyr::arrange(fuel, action, `sub action`, period, scenario) %>%
        {.}
    } else {
      summary.treated <- summary.treated %>%
        dplyr::arrange(fuel, action) %>%
        {.}
      summary.control <- summary.control %>%
        dplyr::arrange(fuel, action, period, scenario) %>%
        {.}
    }
    summary.treated %>%
      dplyr::rename(outcome = fuel) %>%
      readr::write_csv(sprintf("savings_treated_%s.csv", kw))
    summary.control %>%
      dplyr::rename(outcome = fuel) %>%
      readr::write_csv(sprintf("savings_control_%s.csv", kw))
  }
}

table.fontsize = 10
for (treat.status in c("treated", "control")) {
  for (yvar.suf in c("", "_leed", "_lean")) {
    for (kw in
        c("toplevel_bp", "highlevel_bp", "joint_highlevel_bp", "detaillevel_bp")) {
      summary.input <- readr::read_csv(sprintf("savings_%s_%s%s.csv", treat.status, kw, yvar.suf)) %>%
        dplyr::mutate_at(dplyr::vars(action), dplyr::recode,
                        "Building Tuneup or Utility Improvements"="Commissioning") %>%
        {.}
      if (treat.status == "treated") {
        dfs = summary.input %>%
          dplyr::group_by(outcome) %>%
          dplyr::group_split() %>%
          {.}
      } else {
        dfs = summary.input %>%
          dplyr::group_by(outcome, period, scenario) %>%
          dplyr::group_split() %>%
          {.}
      }
      if (stringr::str_detect(kw, "detaillevel")) {
        collapse.cols.end = 2
        hline.option = "major"
      } else {
        collapse.cols.end = 1
        hline.option = "none"
      }
      lapply(dfs, function(df.summary) {
        out.var = df.summary$outcome[[1]]
        if (yvar.suf == "_leed") {
          out.string = "LEED"
        } else if (yvar.suf == "_lean"){
          out.string <- gsub("_GAS", " gas", out.var)
          out.string <- gsub("_KWHR", " electricity", out.string)
          out.string <- gsub("htcl", "air conditioning", out.string)
        } else {
          out.string = tolower(out.var)
        }
        if (treat.status == "treated") {
          treat.string = "retrofitted"
          period.string = "current"
          out.filename = sprintf("../tables/summary_%s_%s_%s.tex", treat.status, out.var, kw)
          period.scenario.string = period.string
        } else {
          treat.string = "un-retrofitted"
          scenario.string = toupper(df.summary$scenario[[1]])
          period.string = df.summary$period[[1]]
          out.filename = sprintf("../tables/summary_%s_%s_%s_%s_%s.tex",
                                treat.status, out.var, period.string, scenario.string, kw)
          period.scenario.string = sprintf("%s (%s)", period.string, toupper(scenario.string))
          df.summary <- df.summary %>%
            dplyr::select(-scenario, -period)
        }
        captiontext = sprintf("Distribution of retrofit effect on %s on the %s, under the %s climate",
                              out.string, treat.string, period.scenario.string)
        sink(out.filename)
        df.summary %>%
          dplyr::select(-outcome) %>%
          knitr::kable("latex", booktabs = T,
                      format.args=list(big.mark=",", digits=3, scientific=F),
                      caption=captiontext) %>%
          kableExtra::kable_styling("striped", full_width = FALSE,
                                    font_size = table.fontsize,
                                    latex_options = "HOLD_position") %>%
          kableExtra::collapse_rows(columns=1:collapse.cols.end, latex_hline=hline.option) %>%
          print()
        sink()
      })
    }
  }
}
