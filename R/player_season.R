# builds one row per player per season of on-field production.
#
# this is the input side of the pricing model. nothing here knows about
# contracts yet, and nothing here is filtered to a signing date. that join
# happens later, in the as-of-signing step, so this table stays a plain record
# of what a player did in a season.
#
# snap share is here on purpose. box score stats cover quarterbacks and skill
# positions and say almost nothing about linemen. snaps at least measure whether
# a team put the player on the field, which is the one usage signal that exists
# for every position.

library(dplyr)

stats   <- readRDS("data/raw/player_stats_reg.rds")
snaps   <- readRDS("data/raw/snap_counts.rds")
players <- readRDS("data/raw/players.rds")

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

con <- file("output/tables/player_season_report.txt", "w")
say <- function(...) { cat(..., "\n", sep = "", file = con); cat(..., "\n", sep = "") }

say("player season production table, ", format(Sys.Date()))
say("stats rows: ", nrow(stats), " seasons ", min(stats$season), "-", max(stats$season))
say("snap rows:  ", nrow(snaps), " seasons ", min(snaps$season), "-", max(snaps$season))

# snap counts are keyed on the pro football reference id, the stats are keyed on
# the gsis id. the players table is the only thing carrying both.
xwalk <- players %>%
  filter(!is.na(pfr_id), !is.na(gsis_id)) %>%
  distinct(pfr_id, .keep_all = TRUE) %>%
  select(pfr_id, gsis_id)

snap_season <- snaps %>%
  filter(game_type == "REG") %>%
  left_join(xwalk, by = c("pfr_player_id" = "pfr_id")) %>%
  group_by(gsis_id, season) %>%
  summarise(
    snap_games   = n(),
    off_snaps    = sum(offense_snaps, na.rm = TRUE),
    def_snaps    = sum(defense_snaps, na.rm = TRUE),
    st_snaps     = sum(st_snaps,      na.rm = TRUE),
    off_snap_pct = mean(offense_pct,  na.rm = TRUE),
    def_snap_pct = mean(defense_pct,  na.rm = TRUE),
    .groups = "drop"
  )

# the id match is not perfect and i would rather count the misses than let them
# disappear into a join.
unmatched <- snaps %>% filter(game_type == "REG") %>%
  anti_join(xwalk, by = c("pfr_player_id" = "pfr_id"))
say("\n[snap counts to gsis id]")
say(sprintf("game rows with no gsis match: %d of %d (%.1f%%)",
            nrow(unmatched), sum(snaps$game_type == "REG"),
            100 * nrow(unmatched) / sum(snaps$game_type == "REG")))
say("distinct players affected: ", n_distinct(unmatched$pfr_player_id))

snap_season <- filter(snap_season, !is.na(gsis_id))

prod <- stats %>%
  transmute(
    gsis_id = player_id, season, position, position_group, games,
    pass_att = attempts, pass_yds = passing_yards, pass_td = passing_tds,
    pass_int = passing_interceptions, pass_epa = passing_epa,
    carries, rush_yds = rushing_yards, rush_td = rushing_tds, rush_epa = rushing_epa,
    targets, rec = receptions, rec_yds = receiving_yards, rec_td = receiving_tds,
    rec_epa = receiving_epa,
    def_tackles = def_tackles_solo + def_tackle_assists,
    def_sacks, def_tfl = def_tackles_for_loss, def_qb_hits,
    def_int = def_interceptions, def_pbu = def_pass_defended
  ) %>%
  full_join(snap_season, by = c("gsis_id", "season"))

stopifnot(!any(duplicated(prod[c("gsis_id", "season")])))

saveRDS(prod, "data/processed/player_season.rds")

say("\n[result]")
say("player seasons: ", nrow(prod), "   players: ", n_distinct(prod$gsis_id))
say("seasons: ", min(prod$season), " to ", max(prod$season))
say(sprintf("have box score stats: %.1f%%", 100 * mean(!is.na(prod$games))))
say(sprintf("have snap counts:     %.1f%%", 100 * mean(!is.na(prod$snap_games))))

# snap coverage by position group is the thing that decides which positions the
# model can say anything about.
say("\n[snap coverage by position group]")
prod %>%
  filter(!is.na(position_group)) %>%
  group_by(position_group) %>%
  summarise(n = n(), with_snaps = round(100 * mean(!is.na(snap_games)), 1),
            .groups = "drop") %>%
  arrange(desc(n)) %>%
  { say(paste(capture.output(print(as.data.frame(.), row.names = FALSE)), collapse = "\n")) }

close(con)
cat("\nwrote data/processed/player_season.rds\n")
