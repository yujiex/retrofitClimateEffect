library("dplyr")

load("../data/retrofit.alldata.rda")

retrofit.alldata %>%
  names()

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
  {.}

usethis::use_data(non.action.data, overwrite = T)
