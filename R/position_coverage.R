# measures how well production is actually observed at each position, which is
# what the tiers in positions.R are based on. run this before trusting them.

library(dplyr)
source("R/positions.R")

m <- add_position_tier(readRDS("data/processed/market_keyed.rds"))
f <- readRDS("data/processed/features.rds")
d <- left_join(m, f, by = "contract_id")

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
con <- file("output/tables/position_coverage.txt", "w")
say <- function(...) { cat(..., "\n", sep = "", file = con); cat(..., "\n", sep = "") }

say("position coverage, ", format(Sys.Date()))
say("share of contracts with a nonzero signal in the three year window before signing")

cov <- d %>%
  group_by(position_tier, position) %>%
  summarise(
    n        = n(),
    snaps    = round(100 * mean(!is.na(w_off_snaps) & (w_off_snaps + w_def_snaps) > 0), 0),
    passing  = round(100 * mean(w_pass_yds > 0, na.rm = TRUE), 0),
    rush_rec = round(100 * mean((w_rush_yds + w_rec_yds) > 0, na.rm = TRUE), 0),
    defense  = round(100 * mean((w_tackles + w_sacks + w_int + w_pbu) > 0, na.rm = TRUE), 0),
    .groups  = "drop"
  ) %>%
  arrange(factor(position_tier, levels = c("measured", "partial", "usage_only", "excluded")),
          desc(n))

say("\n", paste(capture.output(print(as.data.frame(cov), row.names = FALSE)), collapse = "\n"))

say("\n[contracts by tier]")
say(paste(capture.output(print(table(m$position_tier))), collapse = "\n"))

# the defense column reads high for linemen because a guard who recovers one
# fumble in three years counts as nonzero. it is incidental, not a signal, and
# the usage_only tier exists to stop it being read as one.
say("\nnote: the defense column is a floor, not a measure. any single tackle in")
say("three years counts, which is why offensive linemen score above 60 on it.")

close(con)
cat("\nwrote output/tables/position_coverage.txt\n")
