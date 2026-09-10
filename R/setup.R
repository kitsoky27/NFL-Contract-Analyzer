# run once after cloning. installs what the project needs.

pkgs <- c("nflreadr", "dplyr", "tidyr", "readr", "ggplot2", "scales", "fixest")

missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

# renv scans for library() calls, so these put the packages into renv.lock.
library(nflreadr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(fixest)

cat("setup ok\n")
