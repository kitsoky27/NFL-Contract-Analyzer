# assembles the table the model is fit on. one row per contract, the outcome
# and every feature side by side, nothing left to compute at fit time.
#
# two things happen here that matter.
#
# first, window production is converted to a per game rate. the window sums
# cover up to three seasons, so a player with three healthy years has roughly
# three times the yards of one with a single season, for reasons that have
# nothing to do with how good he is. rates separate how well he played from how
# long he was around, and games played stays as its own feature so availability
# is still visible.
#
# the denominator for those rates is games with a recorded snap, not games with
# a box score stat. the stats table only counts a game when the player recorded
# something, so it reads about 4 games a season for a left tackle against 13
# real ones, which would inflate every lineman rate roughly threefold.
#
# second, every exclusion is counted.

library(dplyr)
source("R/positions.R")

market <- add_position_tier(readRDS("data/processed/market_keyed.rds"))
feat   <- readRDS("data/processed/features.rds")

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)
con <- file("output/tables/model_table_report.txt", "w")
say <- function(...) { cat(..., "\n", sep = "", file = con); cat(..., "\n", sep = "") }

d <- left_join(market, feat, by = "contract_id")
stopifnot(nrow(d) == nrow(market))

say("model table, ", format(Sys.Date()))

n <- c(start = nrow(d))

# kickers, punters and long snappers are never on an offensive or defensive
# snap, so neither production nor usage is observed for them at all.
d <- filter(d, position_tier != "excluded")
n["on field positions"] <- nrow(d)

# snap counts begin in 2013, and a contract signed in year t can only use
# seasons through t-1. so snap features exist from the 2014 signing class on.
d <- filter(d, year_signed >= 2014)
n["signed 2014 or later"] <- nrow(d)

d <- filter(d, !is.na(car_seasons), !is.na(age_signing))
n["has prior play and age"] <- nrow(d)

# a window with no games cannot produce a rate, and a player with no snaps in
# three years is not someone the market priced on recent play.
d <- filter(d, !is.na(w_snap_games), w_snap_games > 0, (w_off_snaps + w_def_snaps) > 0)
n["played in the window"] <- nrow(d)

pg <- function(x, games) x / games

model_tbl <- d %>%
  transmute(
    contract_id, player, otc_id, year_signed, cba,

    # outcomes. the log is here because cap share is right skewed and bounded
    # below at zero; which one the model uses is settled in the fitting script.
    apy_share, log_apy_share = log(apy_share), gtd_share, gtd_is_zero,

    # what the player is and how the deal was reached. an extension is
    # negotiated with one team holding his rights, free agency is an open
    # market, so the two are not the same pricing mechanism.
    position = factor(position), position_tier = factor(position_tier),
    contract_type = factor(contract_type),

    age_signing, exp_years, is_drafted, draft_pick_log,

    # availability and usage, the two measures that exist for every position.
    w_games = w_snap_games, games_per_season = w_snap_games / w_seasons,
    snaps_per_game = pg(w_off_snaps + w_def_snaps, w_snap_games),
    last_off_pct, last_def_pct,

    # production per game.
    pass_yds_pg = pg(w_pass_yds, w_snap_games), pass_td_pg = pg(w_pass_td, w_snap_games),
    pass_int_pg = pg(w_pass_int, w_snap_games), pass_epa_pg = pg(w_pass_epa, w_snap_games),
    rush_yds_pg = pg(w_rush_yds, w_snap_games), rush_td_pg = pg(w_rush_td, w_snap_games),
    rush_epa_pg = pg(w_rush_epa, w_snap_games),
    rec_pg      = pg(w_rec, w_snap_games),      rec_yds_pg = pg(w_rec_yds, w_snap_games),
    rec_td_pg   = pg(w_rec_td, w_snap_games),   rec_epa_pg = pg(w_rec_epa, w_snap_games),
    tackles_pg  = pg(w_tackles, w_snap_games),  sacks_pg   = pg(w_sacks, w_snap_games),
    tfl_pg      = pg(w_tfl, w_snap_games),      qb_hits_pg = pg(w_qb_hits, w_snap_games),
    int_pg      = pg(w_int, w_snap_games),      pbu_pg     = pg(w_pbu, w_snap_games)
  )

stopifnot(nrow(model_tbl) == nrow(d))
stopifnot(all(is.finite(model_tbl$apy_share)))
saveRDS(model_tbl, "data/processed/model_table.rds")

say("\n[how the model sample was built]")
wf <- data.frame(step = names(n), rows = as.integer(n),
                 dropped = c(NA, -diff(as.integer(n))))
say(paste(capture.output(print(wf, row.names = FALSE)), collapse = "\n"))

say("\n[what is in it]")
say("contracts: ", nrow(model_tbl), "   players: ", n_distinct(model_tbl$otc_id))
say("years: ", min(model_tbl$year_signed), " to ", max(model_tbl$year_signed))
say("features: ", ncol(model_tbl) - 5, " plus the outcomes")

say("\n[contracts by tier]")
say(paste(capture.output(print(table(model_tbl$position_tier))), collapse = "\n"))

say("\n[outcome]")
say(sprintf("apy share: median %.2f%%, mean %.2f%%, max %.2f%%",
            100 * median(model_tbl$apy_share), 100 * mean(model_tbl$apy_share),
            100 * max(model_tbl$apy_share)))

# a feature that is missing on most rows cannot carry a coefficient, so the
# count is here rather than discovered when lm silently drops the rows.
say("\n[features missing on more than 5% of rows]")
miss <- sapply(model_tbl, function(x) mean(is.na(x)))
miss <- sort(miss[miss > 0.05], decreasing = TRUE)
if (length(miss) == 0) say("none") else
  say(paste(sprintf("  %-18s %.1f%%", names(miss), 100 * miss), collapse = "\n"))

close(con)
cat("\nwrote data/processed/model_table.rds\n")
