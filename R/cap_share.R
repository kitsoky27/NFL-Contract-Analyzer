# puts every contract on a common scale: share of that year's salary cap.
#
# a 20 million dollar deal in 2015 and a 20 million dollar deal in 2025 are not
# the same thing, the cap roughly doubled in between. raw dollars would mostly
# measure the cap going up.
#
# i divide by data/salary_cap.csv rather than the apy_cap_pct column that ships
# with the data, which is rounded to three decimals. check_cap_table.R is where
# i verify my table is right.

library(dplyr)
library(readr)

contracts <- readRDS("data/raw/contracts.rds")
cap       <- read_csv("data/salary_cap.csv", show_col_types = FALSE)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

con <- file("output/tables/cap_share_summary.txt", "w")
say <- function(...) { cat(..., "\n", sep = "", file = con); cat(..., "\n", sep = "") }

# otc files a deal under every team that held it, so a traded player shows the
# same contract twice. dropping the repeats before anything is computed.
deals <- contracts %>%
  select(-season_history, -contract_history) %>%
  distinct(otc_id, year_signed, years, value, apy, guaranteed, .keep_all = TRUE)

say("contract data on cap share, ", format(Sys.Date()))
say("rows: ", nrow(contracts), " -> ", nrow(deals), " after dropping traded-contract repeats")

# inner_join drops year_signed 0, pre-1994, and the 2010 uncapped season.
# none of those have a cap to divide by.
deals <- deals %>%
  inner_join(cap, by = c("year_signed" = "season")) %>%
  mutate(
    apy_share = (apy * 1e6) / cap_dollars,           # values arrive in millions
    gtd_share = (guaranteed * 1e6) / cap_dollars,
    # zero on two thirds of rows. checked against otc: these are real zeros on
    # minimum and practice squad deals, not missing data. keeping them flagged
    # because the pile at zero will matter when guaranteed money is an outcome.
    gtd_is_zero = guaranteed == 0
  )

say("matched to a cap year: ", nrow(deals))
saveRDS(deals, "data/processed/deals.rds")

say("\n[apy as share of cap, all deals]")
q <- quantile(deals$apy_share, c(.5, .75, .9, .99, 1))
say(paste(sprintf("  %-5s %6.2f%%", names(q), 100 * q), collapse = "\n"))

# the median deal is close to the league minimum, which is what a market model
# has to be kept away from. noting it here so the number is on record.
say("\nshare of deals under 1% of the cap: ",
    sprintf("%.1f%%", 100 * mean(deals$apy_share < 0.01)))
say("share with nothing guaranteed: ",
    sprintf("%.1f%%", 100 * mean(deals$gtd_is_zero)))

say("\n[biggest deals on record, by cap share]")
deals %>%
  arrange(desc(apy_share)) %>%
  transmute(player, position, year_signed, years,
            apy_m = round(apy, 1), pct_of_cap = round(100 * apy_share, 1)) %>%
  head(10) %>%
  { say(paste(capture.output(print(as.data.frame(.), row.names = FALSE)), collapse = "\n")) }

close(con)
cat("\nwrote data/processed/deals.rds and output/tables/cap_share_summary.txt\n")
