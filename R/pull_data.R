# pulls nflverse data once and caches it. downstream scripts read the snapshot,
# not the live feed.

library(nflreadr)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

contracts   <- nflreadr::load_contracts()
draft_picks <- nflreadr::load_draft_picks()
players     <- nflreadr::load_players()

# season totals rather than weekly. seasons = TRUE grabs everything available so
# career-to-date features are never cut short by a choice made at pull time.
player_stats <- nflreadr::load_player_stats(seasons = TRUE, summary_level = "reg")

# snaps are the only usage measure that exists for linemen. they start in 2013.
snap_counts <- nflreadr::load_snap_counts(seasons = TRUE)

saveRDS(contracts,    "data/raw/contracts.rds")
saveRDS(draft_picks,  "data/raw/draft_picks.rds")
saveRDS(players,      "data/raw/players.rds")
saveRDS(player_stats, "data/raw/player_stats_reg.rds")
saveRDS(snap_counts,  "data/raw/snap_counts.rds")

# in data/ not data/raw/ so git tracks it. says which snapshot a result came from.
writeLines(
  c(paste("pulled:", format(Sys.time(), "%Y-%m-%d %H:%M")),
    paste("nflreadr:", as.character(utils::packageVersion("nflreadr"))),
    paste("contracts rows:", nrow(contracts)),
    paste("draft_picks rows:", nrow(draft_picks)),
    paste("players rows:", nrow(players)),
    paste("player_stats rows:", nrow(player_stats)),
    paste("snap_counts rows:", nrow(snap_counts))),
  "data/PULL_INFO.txt"
)

cat("contracts:   ", nrow(contracts), "rows\n")
cat("draft_picks: ", nrow(draft_picks), "rows\n")
cat("players:     ", nrow(players), "rows\n")
cat("player_stats:", nrow(player_stats), "rows\n")
cat("snap_counts: ", nrow(snap_counts), "rows\n")
