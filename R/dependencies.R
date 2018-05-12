dependencies <- c(
  "htmltools",
  "colorspace",
  "tidyverse",
  "tidyquant",
  "tidyr",
  "mnormt",
  "plyr",
  "lubridate",
  "yaml",
  "quantmod",
  "xts",
  "zoo",
  "PerformanceAnalytics",
  "alphavantager",
  "TTR",
  "dygraphs",
  "purrr",
  "purrrlyr",
  "dplyr",
  "ggplot2",
  "rlang",
  "pracma",
  "IBrokers",
  "timetk",
  "rpart",
  "rpart.plot",
  "mosaic",
  "corrr"
  )

new.packages <- dependencies[!(dependencies %in% installed.packages()[, "Package"])] # get list of installed packages
if(length(new.packages)>0) {
  install.packages(new.packages)
}
lapply(dependencies, library, character.only = TRUE)

sourceDir <- function(path, trace = TRUE, ...){
  for (fl in list.files(path, pattern = "[.][RrSsQq]$")) {
    if (trace)
      cat(fl,":")
    source(file.path(path, fl), ...)
    if (trace) cat("\n")
  }
}

rm(dependencies, new.packages)