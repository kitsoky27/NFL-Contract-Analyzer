# recovers contract type, which is not a column on the contracts table.
#
# it lives inside the nested contract_history list column. i need it because
# rookie deals come off the wage scale, practice squad and ERFA deals sit at the
# minimum, and the franchise tag is a formula. none of those are negotiated
# against other bidders, so none of them belong in a model of market prices.
#
# output is a lookup keyed on player + year + apy, joined in market_sample.R.

library(dplyr)
library(tidyr)

contracts <- readRDS("data/raw/contracts.rds")
deals     <- readRDS("data/processed/deals.rds")

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

con <- file("output/tables/contract_type_report.txt", "w")
say <- function(...) { cat(..., "\n", sep = "", file = con); cat(..., "\n", sep = "") }

say("contract type recovery, ", format(Sys.Date()))

# the same history is repeated on every row for a player, so take it once each.
types <- contracts %>%
  select(otc_id, contract_history) %>%
  filter(lengths(contract_history) > 0) %>%
  distinct(otc_id, .keep_all = TRUE) %>%
  unnest(contract_history) %>%
  select(otc_id, year_signed, apy, contract_type)

say("contracts listed in the nested history: ", nrow(types))

# the history repeats a deal under every team that held it, so a key can appear
# more than once. collapsing to one row, but flagging the keys where the repeats
# disagree on type rather than quietly picking one.
key_info <- types %>%
  group_by(otc_id, year_signed, apy) %>%
  summarise(n_rows = n(), n_types = n_distinct(contract_type), .groups = "drop")

types <- types %>%
  distinct(otc_id, year_signed, apy, .keep_all = TRUE) %>%
  left_join(key_info, by = c("otc_id", "year_signed", "apy")) %>%
  mutate(type_ambiguous = n_types > 1) %>%
  select(-n_rows, -n_types)

say("keys appearing more than once: ", sum(key_info$n_rows > 1))
say(sprintf("of those, repeats agree on type: %d (%.1f%%)",
            sum(key_info$n_rows > 1 & key_info$n_types == 1),
            100 * mean(key_info$n_types[key_info$n_rows > 1] == 1)))
say("keys flagged ambiguous: ", sum(types$type_ambiguous))

saveRDS(types, "data/processed/contract_types.rds")

say("\n[types available]")
say(paste(capture.output(print(sort(table(types$contract_type), decreasing = TRUE))),
          collapse = "\n"))

# a left join that silently duplicates rows would corrupt everything downstream,
# so check the row count survives before trusting it.
joined <- left_join(deals, types, by = c("otc_id", "year_signed", "apy"))
stopifnot(nrow(joined) == nrow(deals))

say("\n[match rate against the cap-share table]")
say("deals: ", nrow(deals))
say(sprintf("matched a type: %d (%.1f%%)",
            sum(!is.na(joined$contract_type)), 100 * mean(!is.na(joined$contract_type))))

# coverage is thin early and near perfect recently, which decides where the
# usable sample starts.
say("\n[match rate by era]")
joined %>%
  mutate(era = cut(year_signed, c(-Inf, 2010, 2014, 2018, 2022, Inf),
                   labels = c("<=2010", "2011-14", "2015-18", "2019-22", "2023+"))) %>%
  group_by(era) %>%
  summarise(n = n(), matched_pct = round(100 * mean(!is.na(contract_type)), 1),
            .groups = "drop") %>%
  { say(paste(capture.output(print(as.data.frame(.), row.names = FALSE)), collapse = "\n")) }

say("\n[what stays unmatched, by year]")
joined %>% filter(is.na(contract_type)) %>% count(year_signed) %>%
  arrange(desc(n)) %>% head(8) %>%
  { say(paste(capture.output(print(as.data.frame(.), row.names = FALSE)), collapse = "\n")) }

close(con)
cat("\nwrote data/processed/contract_types.rds and output/tables/contract_type_report.txt\n")
