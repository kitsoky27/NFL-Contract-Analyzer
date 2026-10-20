# baseline hedonic regression of contract price on what was known at signing.
#
# the outcome is log cap share. cap share is right skewed, mean 2.7% against a
# median of 1.4%, so in levels a handful of quarterback deals would dominate the
# fit. in logs a coefficient reads as a percent change in price.
#
# production enters as a signed log rather than a raw rate. the diagnostics in
# diagnose_baseline.R showed the raw version predicting one contract at 212% of
# the salary cap, which is not a price anyone can pay. a linear term has no
# diminishing returns, and the top of this market plainly does.
#
# experience is out. it correlates 0.94 with age, the coefficient was
# insignificant, and with both in neither is identified.
#
# the specification choice that matters is that position dummies are left out of
# the main model on purpose. put them in and the average residual at every
# position is zero by construction, which deletes the exact quantity this
# project exists to measure. a second model with them in is fit alongside, not
# to replace the first but to show how much of the variation positions account
# for once production is already controlled for.

library(dplyr)

tbl <- readRDS("data/processed/model_table.rds")
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

con <- file("output/tables/baseline_model.txt", "w")
say <- function(...) { cat(..., "\n", sep = "", file = con); cat(..., "\n", sep = "") }

# 97 players did not play the season before signing, usually injury or a year
# out of the league. lm would drop them without saying so, and they are exactly
# the group a pricing model should have an opinion about. so the share becomes
# zero and an indicator carries "did not play", the same pattern used for
# undrafted players.
tbl <- tbl %>%
  mutate(
    played_last  = !is.na(last_off_pct),
    last_off_pct = coalesce(last_off_pct, 0),
    last_def_pct = coalesce(last_def_pct, 0)
  )

# four contracts are marked drafted but carry no pick number, so there is no
# slot to put them on. too few to model, counted rather than hidden.
n_nopick <- sum(is.na(tbl$draft_pick_log))
tbl <- filter(tbl, !is.na(draft_pick_log))

# age enters as a quadratic because pay rises with experience and then falls,
# and a straight line cannot represent a peak.
tbl <- tbl %>% mutate(age_sq = age_signing^2, year_f = factor(year_signed),
                      row_id = row_number())

raw_production <- c("pass_yds_pg", "pass_td_pg", "pass_int_pg", "pass_epa_pg",
                    "rush_yds_pg", "rush_td_pg", "rush_epa_pg",
                    "rec_pg", "rec_yds_pg", "rec_td_pg", "rec_epa_pg",
                    "tackles_pg", "sacks_pg", "tfl_pg", "qb_hits_pg",
                    "int_pg", "pbu_pg")

# signed log, because epa can be negative and a plain log would drop it.
for (v in raw_production) tbl[[paste0("l_", v)]] <- sign(tbl[[v]]) * log1p(abs(tbl[[v]]))
production <- paste0("l_", raw_production)
usage      <- c("snaps_per_game", "w_games", "games_per_season",
                "last_off_pct", "last_def_pct", "played_last")
player     <- c("age_signing", "age_sq", "is_drafted", "draft_pick_log")
deal       <- c("contract_type", "year_f")

rhs  <- paste(c(production, usage, player, deal), collapse = " + ")
f_np <- as.formula(paste("log_apy_share ~", rhs))
f_p  <- as.formula(paste("log_apy_share ~", rhs, "+ position"))

m_nopos <- lm(f_np, data = tbl)
m_pos   <- lm(f_p,  data = tbl)

say("baseline model, ", format(Sys.Date()))
say("outcome: log of average per year as a share of the salary cap")

# lm drops any row with a missing value silently, so the count is reported
# rather than assumed.
say("\ncontracts in the table:     ", nrow(tbl) + n_nopick)
say("dropped, drafted but no pick: ", n_nopick)
say("contracts used:             ", nobs(m_nopos))
stopifnot(nobs(m_nopos) == nrow(tbl))   # nothing else may be dropped silently

fitstat <- function(m, label) {
  s <- summary(m)
  say(sprintf("  %-28s r2 %.3f   adj r2 %.3f   residual sd %.3f   terms %d",
              label, s$r.squared, s$adj.r.squared, s$sigma, length(coef(m)) - 1))
}

say("\n[fit]")
fitstat(m_nopos, "without position effects")
fitstat(m_pos,   "with position effects")
say(sprintf("\nposition dummies add %.3f to r2 once production is controlled for",
            summary(m_pos)$r.squared - summary(m_nopos)$r.squared))

# the headline model is the one without position effects. these are the terms
# that are not position specific, so they are the ones worth reading directly.
say("\n[selected coefficients, model without position effects]")
keep <- c("age_signing", "age_sq", "is_draftedTRUE", "draft_pick_log",
          "contract_typeUFA", "snaps_per_game", "w_games", "last_off_pct",
          "played_lastTRUE")
co <- summary(m_nopos)$coefficients
co <- co[rownames(co) %in% keep, , drop = FALSE]
say(paste(capture.output(print(round(co, 4))), collapse = "\n"))

say("\na coefficient here is a change in log cap share, so roughly a percent")
say("change in price per unit of the variable.")

# residuals are the point of the whole exercise, so they are saved keyed back to
# the contract rather than left inside the model object.
# keyed by an explicit row id rather than by position in the frame, so a future
# change that drops rows cannot silently misalign residuals with contracts.
fit_rows <- as.integer(names(residuals(m_nopos)))
resid_tbl <- tbl %>%
  filter(row_id %in% fit_rows) %>%
  arrange(match(row_id, fit_rows)) %>%
  transmute(contract_id, player, position, position_tier, year_signed,
            apy_share, fitted = fitted(m_nopos), resid = residuals(m_nopos))
stopifnot(nrow(resid_tbl) == length(fit_rows))

saveRDS(list(nopos = m_nopos, pos = m_pos), "data/processed/baseline_models.rds")
saveRDS(resid_tbl, "data/processed/baseline_residuals.rds")

# a log linear model has no diminishing returns, so an extreme production
# season extrapolates without limit. the largest fitted value here is well
# above the whole salary cap, which is not a price anyone could pay. recorded
# now because it is the first thing the diagnostics have to deal with.
say("\n[extrapolation check]")
say(sprintf("largest fitted cap share: %.1f%% (actual max in the data: %.1f%%)",
            100 * exp(max(resid_tbl$fitted)), 100 * max(resid_tbl$apy_share)))
say(sprintf("fitted values above the cap: %d of %d",
            sum(exp(resid_tbl$fitted) > 1), nrow(resid_tbl)))
say("the model is linear in production and so cannot flatten out at the top.")

say("\n[residuals saved]")
say("rows: ", nrow(resid_tbl))
say(sprintf("mean %.4f, sd %.3f", mean(resid_tbl$resid), sd(resid_tbl$resid)))

close(con)
cat("\nwrote data/processed/baseline_models.rds and baseline_residuals.rds\n")
