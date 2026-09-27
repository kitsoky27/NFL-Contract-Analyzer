# runs the feature builder on the market sample.

library(dplyr)
source("R/features.R")

market <- readRDS("data/processed/market.rds")
ps     <- readRDS("data/processed/player_season.rds")

market$contract_id <- seq_len(nrow(market))
feat <- build_features(market, ps)

saveRDS(market, "data/processed/market_keyed.rds")
saveRDS(feat,   "data/processed/features.rds")

# the guard is the whole point, so report it every run rather than trusting it
# worked. -1 means the newest season a feature can see is the year before signing.
pr <- prior_seasons(market, ps)
cat("newest season relative to signing year:", max(pr$season - pr$year_signed), "\n\n")

cat("contracts:                      ", nrow(market), "\n")
cat("with a gsis id:                 ", sum(!is.na(market$gsis_id)), "\n")
cat("with any prior season:          ", sum(!is.na(feat$car_seasons)), "\n")
cat("with the season before signing: ", sum(!is.na(feat$last_games)), "\n")
cat("median prior seasons:           ", median(feat$car_seasons, na.rm = TRUE), "\n")

# 2011 signings need 2010 production and the season table starts in 2011, so
# they cannot have features.
cat("\n2011 signings with no prior season:",
    sum(is.na(feat$car_seasons[market$year_signed == 2011])),
    "of", sum(market$year_signed == 2011), "\n")
