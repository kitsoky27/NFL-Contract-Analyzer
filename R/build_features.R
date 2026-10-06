# runs the feature builder on the market sample.

library(dplyr)
source("R/features.R")
source("R/attributes.R")
source("R/draft.R")

market  <- readRDS("data/processed/market.rds")
ps      <- readRDS("data/processed/player_season.rds")
players <- readRDS("data/raw/players.rds")

market$contract_id <- seq_len(nrow(market))
market <- add_player_attributes(market, players)
market <- add_draft_features(market)
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
cat("\nage at signing present:        ", sprintf("%.1f%%", 100 * mean(!is.na(market$age_signing))), "\n")
cat("experience present:            ", sprintf("%.1f%%", 100 * mean(!is.na(market$exp_years))), "\n")
cat("dropped as implausible, age:   ", sum(market$age_implausible, na.rm = TRUE),
    " experience:", sum(market$exp_implausible, na.rm = TRUE), "\n")
cat("median age / experience:       ", round(median(market$age_signing, na.rm = TRUE), 1),
    "/", median(market$exp_years, na.rm = TRUE), "\n")

cat("\ndrafted:                       ", sum(market$is_drafted),
    sprintf("(%.1f%%)", 100 * mean(market$is_drafted)), "\n")
cat("median cap share, round 1:     ",
    sprintf("%.2f%%", 100 * median(market$apy_share[market$draft_round == 1], na.rm = TRUE)), "\n")
cat("median cap share, undrafted:   ",
    sprintf("%.2f%%", 100 * median(market$apy_share[!market$is_drafted])), "\n")

cat("\n2011 signings with no prior season:",
    sum(is.na(feat$car_seasons[market$year_signed == 2011])),
    "of", sum(market$year_signed == 2011), "\n")
