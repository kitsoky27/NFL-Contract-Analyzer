# tests for the as of signing feature builder. run this after touching
# features.R. the point is that a leak should break the build, not sit quietly
# in a model for three weeks.
#
# no test framework on purpose. these are small enough to read, and adding a
# package for seven checks is not worth the dependency.

library(dplyr)
source("R/features.R")

failed <- 0
ok <- function(label, passed) {
  cat(if (isTRUE(passed)) "pass  " else "FAIL  ", label, "\n", sep = "")
  if (!isTRUE(passed)) failed <<- failed + 1
}

# a fake player season table with every column the feature builders touch, so
# the tests do not depend on the real data files.
fake_seasons <- function(id, seasons) {
  cols <- c("games", "off_snaps", "def_snaps", "pass_yds", "pass_td", "pass_int",
            "pass_epa", "rush_yds", "rush_td", "rush_epa", "rec", "rec_yds",
            "rec_td", "rec_epa", "def_tackles", "def_sacks", "def_tfl",
            "def_qb_hits", "def_int", "def_pbu")
  d <- data.frame(gsis_id = id, season = seasons)
  for (n in cols) d[[n]] <- 1
  d$off_snap_pct <- 0.5
  d$def_snap_pct <- 0.5
  d
}

deal <- function(id, year) {
  data.frame(contract_id = 1L, gsis_id = id, year_signed = year)
}

# 1. the basic case. seasons after signing must not come back.
ps <- fake_seasons("A", 2018:2022)
pr <- prior_seasons(deal("A", 2021), ps)
ok("only seasons before the signing year are returned",
   identical(sort(pr$season), 2018:2020))

# 2. the off by one. a season equal to the signing year is still the future,
# because the deal is signed before that season is played.
ok("the signing year itself is excluded", !(2021 %in% pr$season))

# 3. a player with nothing before signing gets no rows, not zeros. "did not
# play" and "played badly" are different and the model should see the difference.
pr_none <- prior_seasons(deal("B", 2015), fake_seasons("B", 2015:2019))
ok("a player with no prior seasons returns no rows", nrow(pr_none) == 0)

f_none <- build_features(deal("B", 2015), fake_seasons("B", 2015:2019))
ok("and his career features are missing, not zero", is.na(f_none$car_seasons))

# 4. the window must respect the cutoff too, not just the career totals.
w <- window_features(prior_seasons(deal("A", 2021), ps), window = 3)
ok("the three year window stops at the signing year", w$w_seasons == 3)

# 5. the join must not multiply contracts. a duplicated key here would inflate
# every downstream count without raising anything.
two <- data.frame(contract_id = 1:2, gsis_id = c("A", "A"),
                  year_signed = c(2020L, 2022L))
ok("one row out per contract in", nrow(build_features(two, ps)) == 2)

# 6. a contracts table missing year_signed should stop, not guess.
bad <- data.frame(contract_id = 1L, gsis_id = "A")
ok("a contracts table with no signing year errors",
   inherits(try(prior_seasons(bad, ps), silent = TRUE), "try-error"))

# 7. the real data. this is the check that matters in practice.
if (file.exists("data/processed/market_keyed.rds")) {
  m  <- readRDS("data/processed/market_keyed.rds")
  psr <- readRDS("data/processed/player_season.rds")
  real <- prior_seasons(m, psr)
  ok("no contract in the real sample sees its own signing season",
     max(real$season - real$year_signed) == -1)
} else {
  cat("skip  real data check, run build_features.R first\n")
}

cat("\n")
if (failed > 0) stop(failed, " test(s) failed") else cat("all tests passed\n")
