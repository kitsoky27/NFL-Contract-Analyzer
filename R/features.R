# production features as of the signing date.
#
# this is the part of the project most likely to be quietly wrong. a contract
# signed in march 2023 was priced on what the player had done through the 2022
# season. if a feature includes 2023 or later, the model knows how the deal
# turned out and every result after that is meaningless.
#
# so nothing here reads the player season table directly. prior_seasons() is the
# only door, and it drops anything at or after the signing year before returning.
# every feature below is built on its output, which means a new feature cannot
# see the future even if i forget to think about it.

library(dplyr)

# the door. contracts in, prior production out.
prior_seasons <- function(contracts, player_seasons) {
  stopifnot(all(c("contract_id", "gsis_id", "year_signed") %in% names(contracts)))

  out <- contracts %>%
    select(contract_id, gsis_id, year_signed) %>%
    filter(!is.na(gsis_id)) %>%
    inner_join(player_seasons, by = "gsis_id", relationship = "many-to-many") %>%
    filter(season < year_signed)

  # an error, not a warning. if this ever trips the whole project is void.
  stopifnot(nrow(out) == 0 || all(out$season < out$year_signed))
  out
}

# everything the player had done before signing, however long ago.
career_features <- function(prior) {
  prior %>%
    group_by(contract_id) %>%
    summarise(
      car_seasons = n_distinct(season),
      car_games   = sum(games, na.rm = TRUE),
      car_snaps   = sum(off_snaps, na.rm = TRUE) + sum(def_snaps, na.rm = TRUE),
      .groups = "drop"
    )
}

# the season immediately before signing. a player who missed that year gets no
# row here, which is itself informative and should stay missing rather than zero.
last_season_features <- function(prior) {
  prior %>%
    filter(season == year_signed - 1) %>%
    group_by(contract_id) %>%
    summarise(
      last_games    = sum(games, na.rm = TRUE),
      last_snaps    = sum(off_snaps, na.rm = TRUE) + sum(def_snaps, na.rm = TRUE),
      last_off_pct  = mean(off_snap_pct, na.rm = TRUE),
      last_def_pct  = mean(def_snap_pct, na.rm = TRUE),
      .groups = "drop"
    )
}

# a three year window smooths out one good or one injured season.
window_features <- function(prior, window = 3) {
  prior %>%
    filter(season >= year_signed - window) %>%
    group_by(contract_id) %>%
    summarise(
      w_seasons  = n_distinct(season),
      w_games    = sum(games, na.rm = TRUE),
      w_off_snaps = sum(off_snaps, na.rm = TRUE),
      w_def_snaps = sum(def_snaps, na.rm = TRUE),
      w_pass_yds = sum(pass_yds, na.rm = TRUE), w_pass_td = sum(pass_td, na.rm = TRUE),
      w_pass_int = sum(pass_int, na.rm = TRUE), w_pass_epa = sum(pass_epa, na.rm = TRUE),
      w_rush_yds = sum(rush_yds, na.rm = TRUE), w_rush_td = sum(rush_td, na.rm = TRUE),
      w_rush_epa = sum(rush_epa, na.rm = TRUE),
      w_rec      = sum(rec, na.rm = TRUE),      w_rec_yds = sum(rec_yds, na.rm = TRUE),
      w_rec_td   = sum(rec_td, na.rm = TRUE),   w_rec_epa = sum(rec_epa, na.rm = TRUE),
      w_tackles  = sum(def_tackles, na.rm = TRUE), w_sacks = sum(def_sacks, na.rm = TRUE),
      w_tfl      = sum(def_tfl, na.rm = TRUE),  w_qb_hits = sum(def_qb_hits, na.rm = TRUE),
      w_int      = sum(def_int, na.rm = TRUE),  w_pbu     = sum(def_pbu, na.rm = TRUE),
      .groups = "drop"
    )
}

build_features <- function(contracts, player_seasons, window = 3) {
  prior <- prior_seasons(contracts, player_seasons)
  contracts %>%
    select(contract_id) %>%
    left_join(career_features(prior),      by = "contract_id") %>%
    left_join(last_season_features(prior), by = "contract_id") %>%
    left_join(window_features(prior, window), by = "contract_id")
}

# ---- run it on the market sample ------------------------------------------

if (sys.nframe() == 0) {
  market <- readRDS("data/processed/market.rds")
  ps     <- readRDS("data/processed/player_season.rds")

  market$contract_id <- seq_len(nrow(market))
  feat <- build_features(market, ps)

  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  saveRDS(market, "data/processed/market_keyed.rds")
  saveRDS(feat,   "data/processed/features.rds")

  # the guard is the whole point, so the script reports it every run rather than
  # trusting that it worked. -1 means the newest season any feature can see is
  # the season before signing.
  pr <- prior_seasons(market, ps)
  cat("newest season relative to signing year:", max(pr$season - pr$year_signed), "\n\n")

  cat("contracts:                        ", nrow(market), "\n")
  cat("with a gsis id:                   ", sum(!is.na(market$gsis_id)), "\n")
  cat("with any prior season:            ", sum(!is.na(feat$car_seasons)), "\n")
  cat("with the season before signing:   ", sum(!is.na(feat$last_games)), "\n")
  cat("median prior seasons:             ", median(feat$car_seasons, na.rm = TRUE), "\n")

  # 2011 signings need 2010 production and the season table starts in 2011, so
  # they cannot have features. worth seeing rather than discovering later.
  cat("\n2011 signings with no prior season:",
      sum(is.na(feat$car_seasons[market$year_signed == 2011])),
      "of", sum(market$year_signed == 2011), "\n")
}
