# what is actually in the contract data, before building anything on it.
# writes a report to output/tables so i can point at it later.

library(dplyr)
library(tidyr)

contracts <- readRDS("data/raw/contracts.rds")
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

con <- file("output/tables/audit_report.txt", "w")
say <- function(...) { cat(..., "\n", sep = "", file = con); cat(..., "\n", sep = "") }

say("contract data audit, ", format(Sys.Date()))
say("rows: ", nrow(contracts), "   columns: ", ncol(contracts))

say("\n[year_signed coverage]")
yr <- as.data.frame(table(contracts$year_signed))
names(yr) <- c("year", "n")
say(paste(capture.output(print(yr, row.names = FALSE)), collapse = "\n"))
say("rows with year_signed == 0: ", sum(contracts$year_signed == 0))

say("\n[missing by column]")
for (n in names(contracts)) {
  x <- contracts[[n]]
  # is.na does not work on the two list columns, so count empty entries instead
  n_miss <- if (is.list(x)) sum(lengths(x) == 0) else sum(is.na(x))
  say(sprintf("  %-22s %6d  (%.1f%%)", n, n_miss, 100 * n_miss / nrow(contracts)))
}

# money columns show no NAs, which is misleading. missing money is stored as zero.
say("\n[zeros in money columns]")
for (n in c("value", "apy", "guaranteed", "apy_cap_pct")) {
  z <- sum(contracts[[n]] == 0, na.rm = TRUE)
  say(sprintf("  %-14s %6d zeros (%.1f%%)", n, z, 100 * z / nrow(contracts)))
}

# values are in millions, not dollars. printing it so nobody scales twice.
say("\n[units] max value = ", max(contracts$value), ", max apy = ", max(contracts$apy))

# rounded to three decimals, so a median contract carries one significant figure.
# this is why i will not use apy_cap_pct as the outcome.
nz <- contracts$apy_cap_pct[contracts$apy_cap_pct > 0]
say("\n[apy_cap_pct precision]")
say("distinct nonzero values: ", length(unique(nz)), " across ", nrow(contracts), " rows")
say("identical to round(x, 3): ", all(abs(nz - round(nz, 3)) < 1e-9))

say("\n[position labels]")
pos <- sort(table(contracts$position), decreasing = TRUE)
say(paste(capture.output(print(pos)), collapse = "\n"))
say("distinct positions: ", length(pos))

# otc files a deal under every team that held it, so a traded player shows the
# same contract twice. those are not two contracts.
key <- paste(contracts$otc_id, contracts$year_signed, contracts$years,
             contracts$value, contracts$apy, contracts$guaranteed)
say("\n[duplicates] repeated rows on player + terms: ", sum(duplicated(key)))

# contract_type is nested in contract_history, not a column. i need it to keep
# rookie deals and franchise tags out of a market model, so check it joins back.
hist <- contracts %>%
  select(otc_id, contract_history) %>%
  filter(lengths(contract_history) > 0) %>%
  distinct(otc_id, .keep_all = TRUE) %>%
  unnest(contract_history) %>%
  select(otc_id, year_signed, apy, contract_type) %>%
  distinct(otc_id, year_signed, apy, .keep_all = TRUE)

deduped <- contracts %>%
  select(-season_history, -contract_history) %>%
  distinct(otc_id, year_signed, years, value, apy, guaranteed, .keep_all = TRUE)

joined <- left_join(deduped, hist, by = c("otc_id", "year_signed", "apy"))

say("\n[contract_type recovery]")
say("deduped contracts: ", nrow(deduped))
say(sprintf("matched a type: %d (%.1f%%)",
            sum(!is.na(joined$contract_type)), 100 * mean(!is.na(joined$contract_type))))
say(paste(capture.output(print(sort(table(joined$contract_type), decreasing = TRUE))),
          collapse = "\n"))

close(con)
cat("\nwrote output/tables/audit_report.txt\n")
