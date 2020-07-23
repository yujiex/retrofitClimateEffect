library("dplyr")

load("../data/retrofit.energy.avgtemp.rda")
load("../data/retrofit.alldata.rda")

CVRMSE <- function(y, y_hat, n_par) {
  n = length(y)
  if (!missing(n_par)) {
    return(sqrt(sum((y_hat - y) ^ 2) / (n - n_par))/mean(y))
  } else {
    return(sqrt(sum((y - y_hat)^2)) / n / mean(y))
  }
}

## with known change point
## left.flat: whether the baseload is on the left
fit.piecewise <- function (cp, x, y, left.flat, npar) {
  if (left.flat) {
    xmod = pmax(cp, x)
  } else {
    xmod = pmin(cp, x)
  }
  data.x = xmod
  lin.mod = lm(y ~ xmod)
  yhat = predict(lin.mod, newdata = data.frame(xmod = data.x))
  cvrmse = CVRMSE(y, yhat, n_par=npar)
  baseload = predict(lin.mod, newdata = data.frame(xmod = cp))[[1]]
  ## if baseload is negative, make it zero
  htcl = mean(yhat) - baseload
  return(list("cvrmse"=cvrmse, "model"=lin.mod, "baseload"=baseload,
              "htcl"=htcl, "cp"=cp))
}

## cps: change points
## x: x points
## left.flat: whether the baseload is on the left
grid.search.cp <- function (cps, x, y, left.flat, npar=3) {
  result = lapply(cps, function(cp) {
    cvrmse = fit.piecewise(cp, x, y, left.flat, npar)$cvrmse
  })
  argmin.cvrmse = which.min(unlist(result))
  return (argmin.cvrmse)
}

## k: how many grid points per iteration
twostep.grid.search.cp <- function (x, y, left.flat, k=10) {
  cps = seq(min(x), max(x), length.out = k)
  argmin.cvrmse = grid.search.cp(cps, x, y, left.flat, 3)
  cps = seq(cps[[max(1, argmin.cvrmse - 1)]], cps[[min(argmin.cvrmse + 1, k)]], length.out = 2 * k)
  argmin.cvrmse = grid.search.cp(cps, x, y, left.flat, 3)
  cp = cps[[argmin.cvrmse]]
  result = fit.piecewise(cp, x, y, left.flat, 3)
  return(result)
}

plot.fitting <- function (x, y, cp, model, left.flat, titletext, pathname) {
  predictor_fit <- c(min(x), cp, max(x))
  if (left.flat) {
    data.x = pmax(cp, predictor_fit)
  } else {
    data.x = pmin(cp, predictor_fit)
  }
  png(pathname)
  plot(x, y, col="black",pch=16, main=titletext)
  lines(predictor_fit,
        predict(model, newdata=data.frame(xmod=data.x)),
        col="red")
  dev.off()
}

with.enough.data = retrofit.alldata %>%
  distinct(BLDGNUM, `Substantial_Completion_Date`, retro.status, variable) %>%
  {.}

## plot.var = "GAS"
## left.flat.var = FALSE
plot.var = "KWHR"
left.flat.var = TRUE

dfs = retrofit.energy.avgtemp %>%
  dplyr::filter(variable == plot.var) %>%
  dplyr::inner_join(with.enough.data,
                    by=c("BLDGNUM", "Substantial_Completion_Date", "retro.status", "variable")) %>%
  dplyr::group_by(BLDGNUM, Substantial_Completion_Date, is.real.retrofit, retro.status, variable) %>%
  dplyr::group_split() %>%
  {.}

## retrofit.energy.avgtemp %>%
##   dplyr::inner_join(with.enough.data,
##                     by=c("BLDGNUM", "Substantial_Completion_Date", "retro.status", "variable")) %>%
##   dplyr::filter(variable %in% c("GAS", "KWHR")) %>%
##   dplyr::group_by(BLDGNUM, Substantial_Completion_Date, is.real.retrofit, retro.status, variable) %>%
##   dplyr::summarise(count=n()) %>%
##   dplyr::ungroup() %>%
##   readr::write_csv("retrofit_energy_avgtemp_data_count.csv")

k = 10
result.changepoint = lapply(seq_along(dfs), function(j) {
  df = dfs[[j]]
  imagedir = "../images/energy_vs_monthly_temp/"
  imagename = sprintf("%s.png", paste(df$BLDGNUM[[1]], df$Substantial_Completion_Date[[1]], df$retro.status[[1]], plot.var, sep = "_"))
  y = df$kbtu.per.sqft
  x = df$mean.temp
  if (all(y == 0)) {
    print(sprintf("%d %s ---- all 0 y", j, imagename))
    return(NULL)
  }
  print(sprintf("%d %s", j, imagename))
  result = twostep.grid.search.cp(x, y, left.flat.var, k=10)
  ## titletext = sprintf("%s %s, %s %s\n CV-RMSE=%.2f, baseload=%.5f, htcl=%.4f", df$BLDGNUM[[1]], plot.var, df$retro.status[[1]], df$Substantial_Completion_Date[[1]], result$cvrmse, result$baseload, result$htcl)
  ## print(titletext)
  changed.side = FALSE
  if (result$htcl < 0) {
    ## print("asdf")
    result <- twostep.grid.search.cp(x, y, !left.flat.var, k=10)
    changed.side = TRUE
  }
  if (result$baseload < 0) {
    baseload.adj = 0
    htcl.adj = result$htcl - result$baseload
  } else {
    baseload.adj = result$baseload
    htcl.adj = result$htcl
  }
  titletext = sprintf("%s %s, %s %s\n CV-RMSE=%.2f, baseload=%.5f, htcl=%.4f", df$BLDGNUM[[1]], plot.var, df$retro.status[[1]], df$Substantial_Completion_Date[[1]], result$cvrmse, baseload.adj, htcl.adj)
  ## print(titletext)
  if (changed.side) {
    new.left.flat.var = !left.flat.var
  } else {
    new.left.flat.var = left.flat.var
  }
  plot.fitting(x, y, result$cp, result$model, new.left.flat.var, titletext,
               pathname=paste0(imagedir, imagename))
  tibble::as.tibble(result[names(result) != "model"]) %>%
    dplyr::bind_cols(df[1,c("BLDGNUM", "Substantial_Completion_Date", "retro.status", "variable")]) %>%
    {.}
})
result.changepoint %>%
  dplyr::bind_rows() %>%
  readr::write_csv(sprintf("lean_result_%s.csv", plot.var))

## generate tex for lean
load("../data/non.action.data.lean.rda")

study.set = non.action.data.lean %>%
  distinct(BLDGNUM, `Substantial_Completion_Date`, variable) %>%
  tidyr::separate(variable, into=c("type", "variable")) %>%
  dplyr::distinct(BLDGNUM, `Substantial_Completion_Date`, variable) %>%
  dplyr::mutate(`Substantial_Completion_Date`=substr(as.character(`Substantial_Completion_Date`), 1, 10)) %>%
  {.}

dfs = retrofit.energy.avgtemp %>%
  dplyr::distinct(BLDGNUM, `Substantial_Completion_Date`, variable, retro.status) %>%
  dplyr::mutate(`Substantial_Completion_Date`=substr(as.character(`Substantial_Completion_Date`), 1, 10)) %>%
  dplyr::inner_join(study.set,
                    by=c("BLDGNUM", "Substantial_Completion_Date", "variable")) %>%
  dplyr::arrange(variable, BLDGNUM, retro.status) %>%
  dplyr::mutate(imagename = paste0(BLDGNUM, "_", `Substantial_Completion_Date`, "_", retro.status, "_", variable)) %>%
  dplyr::group_by(variable) %>%
  dplyr::group_split() %>%
  {.}

j = 1
df = dfs[[1]]

perpage = 24
lapply(dfs, function(df) {
  dfs.pages = df %>%
    dplyr::select(imagename) %>%
    tibble::rowid_to_column() %>%
    dplyr::mutate(pagenum = (rowid - 1) %/% perpage) %>%
    dplyr::group_by(pagenum) %>%
    dplyr::group_split() %>%
    {.}
  contents <- sapply(dfs.pages, function(df.page) {
    page.content = c("\\begin{figure}",
                    "\\centering",
                    sprintf("\\includegraphics[width = 0.24\\textwidth, keepaspectratio]{../images/energy_vs_monthly_temp/%s.png}", df.page$imagename),
                    "\\end{figure}",
                    "\\clearpage")
  })
  varname = df$variable[[1]]
  con <- file(sprintf("../document/lean_output_%s.tex", varname), open = "w+")
  writeLines(unlist(contents), con, sep = "\n", useBytes = FALSE)
  close(con)
})
