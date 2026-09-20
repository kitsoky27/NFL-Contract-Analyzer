# builds the sample the pricing model actually runs on.
#
# a market price needs a player who could have signed somewhere else. rookie
# deals come off the wage scale, ERFA and practice squad deals sit at the
# minimum, RFA is a tender, and the franchise tag is a formula. keeping UFA
# signings and veteran extensions, dropping everything else.
#
# every exclusion is counted below so the sample can be argued with.

library(dplyr)

deals <- readRDS("data/processed/deals.rds")
types <- readRDS("data/processed/contract_types.rds")

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
con <- file("output/tables/market_sample_report.txt", "w")
say <- function(...) { cat(..., "\n", sep = "", file = con); cat(..., "\n", sep = "") }

d <- left_join(deals, types, by = c("otc_id", "year_signed", "apy"))
stopifnot(nrow(d) == nrow(deals))   # a duplicated key here would corrupt everything

# the waterfall. each row is one decision and what it cost.
steps <- list()
keep  <- function(df, label) {
  steps[[length(steps) + 1]] <<- data.frame(step = label, rows = nrow(df))
  df
}

say("market sample, ", format(Sys.Date()))

m <- d %>% keep("start: contracts on cap share")

m <- m %>% filter(!is.na(contract_type)) %>%
  keep("drop: type could not be recovered")

# the repeats disagreed on type for these, so i do not know what they are.
m <- m %>% filter(!type_ambiguous) %>%
  keep("drop: type ambiguous across repeats")

m <- m %>% filter(contract_type %in% c("UFA", "Extension")) %>%
  keep("keep: UFA and Extension only")

# the 2011 cba introduced the rookie wage scale and reset how young players are
# priced. before that is a different regime, and the data is thin there anyway.
m <- m %>% filter(year_signed >= 2011) %>%
  keep("drop: signed before the 2011 cba")

# a zero here is a recording gap, not a free player.
m <- m %>% filter(apy > 0, value > 0) %>%
  keep("drop: zero apy or zero total value")

say("\n[how the sample was built]")
wf <- bind_rows(steps) %>% mutate(lost = c(NA, -diff(rows)))
say(paste(capture.output(print(as.data.frame(wf), row.names = FALSE)), collapse = "\n"))

saveRDS(m, "data/processed/market.rds")

say("\n[what is in it]")
say("contracts: ", nrow(m), "   players: ", n_distinct(m$otc_id))
say("years: ", min(m$year_signed), " to ", max(m$year_signed))
say(sprintf("UFA %d, Extension %d",
            sum(m$contract_type == "UFA"), sum(m$contract_type == "Extension")))
say(sprintf("median apy: %.2f%% of cap, 90th pct: %.2f%%",
            100 * median(m$apy_share), 100 * quantile(m$apy_share, .9)))
say(sprintf("nothing guaranteed: %.1f%%", 100 * mean(m$gtd_is_zero)))

say("\n[contracts per year]")
say(paste(capture.output(print(table(m$year_signed))), collapse = "\n"))

# thin cells are where a per-position estimate stops meaning anything, so the
# count matters before any of this gets cut by position.
say("\n[contracts per position]")
say(paste(capture.output(print(sort(table(m$position), decreasing = TRUE))), collapse = "\n"))

cells <- m %>% count(position, year_signed)
say("\nposition-year cells: ", nrow(cells),
    "   median size: ", median(cells$n),
    "   cells under 5: ", sum(cells$n < 5))

close(con)
cat("\nwrote data/processed/market.rds and output/tables/market_sample_report.txt\n")
