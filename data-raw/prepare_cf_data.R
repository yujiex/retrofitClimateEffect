library("dplyr")

load("../data/retrofit.alldata.rda")

disagg = readr::read_csv("lean_result_GAS.csv") %>%
  dplyr::bind_rows(readr::read_csv("lean_result_KWHR.csv")) %>%
  {.}
disagg <- disagg %>%
  dplyr::mutate(htcl = ifelse(baseload < 0, htcl - baseload, htcl)) %>%
  dplyr::mutate(baseload = pmax(baseload, 0)) %>%
  dplyr::group_by(BLDGNUM, `Substantial_Completion_Date`, variable) %>%
  ## no negative heating cooling load, removed 30 records
  dplyr::filter(sum(htcl < 0) == 0) %>%
  dplyr::ungroup() %>%
  {.}

pre.lean <- disagg %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::select(-cvrmse, -cp, -retro.status) %>%
  tidyr::pivot_wider(names_from = variable, values_from = baseload:htcl,
                     values_fill = list("baseload"=0, "htcl"=0)) %>%
  {.}

lean.diff = disagg %>%
  dplyr::select(-cvrmse, -cp) %>%
  tidyr::gather(leantype, value, baseload:htcl) %>%
  tidyr::unite(col="variable", leantype, variable) %>%
  tidyr::spread(retro.status, value) %>%
  dplyr::mutate(eui.diff = post - pre) %>%
  dplyr::select(-post, -pre) %>%
  {.}

pre.nonenergy = retrofit.alldata %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::mutate(is.office = (`Building_Type`=="Office")) %>%
  dplyr::select(-ownership, -variable, -retro.status, -mean.kbtu, -name, -count,
                -start, -end, -`Building_Type`, -HDD, -CDD) %>%
  dplyr::distinct() %>%
  dplyr::mutate(REGNNUM = paste0("Region ", REGNNUM)) %>%
  dplyr::mutate(value=T) %>%
  tidyr::spread(REGNNUM, value, fill=F) %>%
  dplyr::mutate(BLDGCAT= paste0("Category ", BLDGCAT)) %>%
  dplyr::mutate(value=T) %>%
  tidyr::spread(BLDGCAT, value, fill=F) %>%
  {.}

pre.energy = retrofit.alldata %>%
  dplyr::filter(!(variable %in% c("KWDMD", "OIL"))) %>%
  dplyr::filter(retro.status == "pre") %>%
  dplyr::distinct(BLDGNUM, is.real.retrofit, `Substantial_Completion_Date`,
                  variable, mean.kbtu, GROSSSQFT) %>%
  dplyr::mutate(kbtu.per.sqft = mean.kbtu/GROSSSQFT) %>%
  dplyr::select(-mean.kbtu, -GROSSSQFT) %>%
  tidyr::spread(variable, kbtu.per.sqft, fill=0) %>%
  {.}

eui.diff.total = retrofit.alldata %>%
  dplyr::filter(!(variable %in% c("KWDMD", "OIL"))) %>%
  dplyr::distinct(BLDGNUM, Substantial_Completion_Date, retro.status,
                  is.real.retrofit, variable, mean.kbtu, GROSSSQFT) %>%
  dplyr::mutate(kbtu.per.sqft = mean.kbtu / GROSSSQFT) %>%
  dplyr::select(-mean.kbtu, -GROSSSQFT) %>%
  tidyr::spread(retro.status, kbtu.per.sqft) %>%
  dplyr::mutate(eui.diff = post - pre) %>%
  dplyr::select(-post, -pre) %>%
  {.}

non.action.data = pre.nonenergy %>%
  dplyr::left_join(pre.energy, by=c("BLDGNUM", "is.real.retrofit", "Substantial_Completion_Date")) %>%
  dplyr::left_join(eui.diff.total, by=c("BLDGNUM", "is.real.retrofit", "Substantial_Completion_Date")) %>%
  dplyr::filter(variable %in% c("KWHR", "GAS")) %>%
  dplyr::select(-with.leed.post) %>%
  {.}

usethis::use_data(non.action.data, overwrite = T)

non.action.data.binary = pre.nonenergy %>%
  dplyr::filter(!with.leed.pre) %>%
  dplyr::left_join(pre.energy, by=c("BLDGNUM", "is.real.retrofit", "Substantial_Completion_Date")) %>%
  dplyr::select(-with.leed.pre) %>%
  dplyr::mutate(variable = "leed") %>%
  {.}

usethis::use_data(non.action.data.binary, overwrite=T)

non.action.data.lean = pre.nonenergy %>%
  dplyr::left_join(pre.energy, by=c("BLDGNUM", "is.real.retrofit", "Substantial_Completion_Date")) %>%
  dplyr::inner_join(pre.lean, by=c("BLDGNUM", "Substantial_Completion_Date")) %>%
  dplyr::left_join(lean.diff, by=c("BLDGNUM", "Substantial_Completion_Date")) %>%
  dplyr::select(-with.leed.post) %>%
  {.}

usethis::use_data(non.action.data.lean, overwrite=T)
