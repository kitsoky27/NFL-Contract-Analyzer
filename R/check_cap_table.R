# checks my salary cap table against the data before i build anything on it.
#
# the contracts file ships an apy_cap_pct column, but it is rounded to three
# decimals so a median contract carries one significant figure (see
# audit_contracts.R). i divide by my own cap table instead. this script is how
# i know that table is right: recompute cap share myself and see whether it
# lands where over the cap says it should.

library(dplyr)
library(readr)

contracts <- readRDS("data/raw/contracts.rds")
cap       <- read_csv("data/salary_cap.csv", show_col_types = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

con <- file("output/tables/cap_table_check.txt", "w")
say <- function(...) { cat(..., "\n", sep = "", file = con); cat(..., "\n", sep = "") }

say("salary cap table check, ", format(Sys.Date()))
say("cap table covers ", min(cap$season), " to ", max(cap$season),
    " (", nrow(cap), " seasons, 2010 omitted as uncapped)")

# inner_join drops year_signed == 0 and the 2010 uncapped season, neither of
# which has a cap to divide by.
deduped <- contracts %>%
  distinct(otc_id, year_signed, years, value, apy, guaranteed, .keep_all = TRUE)

d <- deduped %>%
  inner_join(cap, by = c("year_signed" = "season")) %>%
  mutate(apy_share = (apy * 1e6) / cap_dollars)   # values arrive in millions

say("\nrows: ", nrow(contracts), " -> ", nrow(deduped), " after dropping traded-contract repeats")
say("matched to a cap year: ", nrow(d), " (rest are year_signed 0, pre-1994, or the 2010 uncapped season)")

# the comparison. rounding alone can move apy_cap_pct by up to 0.0005, so
# anything inside that is agreement, not error.
v <- d %>% filter(apy_cap_pct > 0, apy > 0) %>% mutate(diff = abs(apy_share - apy_cap_pct))

say("\n[agreement with over the cap's own apy_cap_pct]")
say("rows compared: ", nrow(v))
say(sprintf("median absolute difference: %.4f pp", 100 * median(v$diff)))
say(sprintf("within rounding tolerance (0.05 pp): %.2f%%", 100 * mean(v$diff < 0.0005)))
say(sprintf("off by more than 0.5 pp: %.2f%%",          100 * mean(v$diff > 0.005)))
say(sprintf("largest single difference: %.4f pp", 100 * max(v$diff)))

# a real cap error would show up as one bad year, not as scatter, so check by year.
say("\n[worst year by median difference]")
v %>% group_by(year_signed) %>%
  summarise(n = n(), med_pp = round(100 * median(diff), 4), .groups = "drop") %>%
  arrange(desc(med_pp)) %>% head(5) %>%
  { say(paste(capture.output(print(as.data.frame(.), row.names = FALSE)), collapse = "\n")) }

# second check, independent of the first. big contracts have the least rounding
# error, so apy / apy_cap_pct on those backs out the cap the data assumes.
say("\n[cap implied by large contracts vs my table, in millions]")
big <- d %>% filter(apy_cap_pct >= 0.05, apy > 0, year_signed >= 2011)
big %>% group_by(year_signed) %>%
  summarise(n = n(),
            implied = round(median(apy * 1e6 / apy_cap_pct) / 1e6, 1),
            mine    = round(first(cap_dollars) / 1e6, 1), .groups = "drop") %>%
  mutate(gap = round(implied - mine, 1)) %>%
  { say(paste(capture.output(print(as.data.frame(.), row.names = FALSE)), collapse = "\n")) }

close(con)
cat("\nwrote output/tables/cap_table_check.txt\n")
