# pulls nflverse data once and caches it. downstream scripts read the snapshot,
# not the live feed.

library(nflreadr)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

contracts   <- nflreadr::load_contracts()
draft_picks <- nflreadr::load_draft_picks()
players     <- nflreadr::load_players()

saveRDS(contracts,   "data/raw/contracts.rds")
saveRDS(draft_picks, "data/raw/draft_picks.rds")
saveRDS(players,     "data/raw/players.rds")

# in data/ not data/raw/ so git tracks it. says which snapshot a result came from.
writeLines(
  c(paste("pulled:", format(Sys.time(), "%Y-%m-%d %H:%M")),
    paste("nflreadr:", as.character(utils::packageVersion("nflreadr"))),
    paste("contracts rows:", nrow(contracts)),
    paste("draft_picks rows:", nrow(draft_picks)),
    paste("players rows:", nrow(players))),
  "data/PULL_INFO.txt"
)

cat("contracts:  ", nrow(contracts), "rows,", ncol(contracts), "cols\n")
cat("draft_picks:", nrow(draft_picks), "rows,", ncol(draft_picks), "cols\n")
cat("players:    ", nrow(players), "rows,", ncol(players), "cols\n")
