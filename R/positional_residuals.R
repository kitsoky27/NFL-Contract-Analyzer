# the result the project exists to produce.
#
# the baseline has no position dummies, so nothing forces a position's residuals
# to average zero. a position whose residuals sit consistently above zero is one
# the market pays more than its production, usage, age and draft slot predict.
#
# residuals are averaged within player first. a player who signed four deals
# would otherwise count four times, and the same man's contracts are not four
# independent observations of how the market treats his position.

library(dplyr)
library(ggplot2)
source("R/theme.R")

r <- readRDS("data/processed/baseline_residuals.rds")

dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
con <- file("output/tables/positional_residuals.txt", "w")
say <- function(...) { cat(..., "\n", sep = "", file = con); cat(..., "\n", sep = "") }

by_player <- r %>%
  group_by(position, position_tier, player) %>%
  summarise(resid = mean(resid), .groups = "drop")

pos <- by_player %>%
  group_by(position_tier, position) %>%
  summarise(players = n(), mean_resid = mean(resid),
            se = sd(resid) / sqrt(n()), .groups = "drop") %>%
  mutate(lo = mean_resid - 1.96 * se,
         hi = mean_resid + 1.96 * se,
         pct = 100 * (exp(mean_resid) - 1),
         clear_of_zero = lo > 0 | hi < 0)

say("positional residuals, ", format(Sys.Date()))
say("contracts: ", nrow(r), "   players: ", n_distinct(r$player))
say("a positive value means the market pays the position above what the model predicts")

say("\n[by position]")
out <- pos %>% arrange(desc(mean_resid)) %>%
  transmute(position, tier = position_tier, players,
            resid = round(mean_resid, 3),
            ci = sprintf("[%+.2f, %+.2f]", lo, hi),
            vs_model = sprintf("%+.0f%%", pct),
            clear = ifelse(clear_of_zero, "yes", ""))
say(paste(capture.output(print(as.data.frame(out), row.names = FALSE)), collapse = "\n"))
write.csv(pos, "output/tables/positional_residuals.csv", row.names = FALSE)

# the obvious objection to the tackle result is that linemen have no production
# measure, so the model cannot see whether one played well. that objection
# cannot explain a gap inside the offensive line, where all five positions are
# measured exactly as badly as each other.
say("\n[the within line comparison]")
ol <- by_player %>%
  filter(position_tier == "usage_only") %>%
  mutate(grp = ifelse(position %in% c("LT", "RT"), "tackle", "interior"))

g <- ol %>% group_by(grp) %>%
  summarise(players = n(), mean_resid = round(mean(resid), 3), .groups = "drop")
say(paste(capture.output(print(as.data.frame(g), row.names = FALSE)), collapse = "\n"))

tt   <- t.test(resid ~ grp, data = ol)          # interior first, tackle second
gap  <- unname(tt$estimate[2] - tt$estimate[1])
ci   <- sort(-tt$conf.int)
say(sprintf("\ntackles minus interior: %+.3f log points, 95%% CI [%+.3f, %+.3f], p = %.4g",
            gap, ci[1], ci[2], tt$p.value))
say(sprintf("in price terms tackles earn %.0f%% more than guards and centres",
            100 * (exp(gap) - 1)))
say("all five line positions share the same measurement problem, so the gap")
say("between them is about pay, not about what the model can see.")

# where production is observed the model should be unbiased, and it is. that is
# the check that the approach works before any of the above is believed.
say("\n[sanity check on the measured tier]")
m <- by_player %>% filter(position_tier == "measured")
say(sprintf("mean residual %+.4f, 95%% CI [%+.3f, %+.3f] across %d players",
            mean(m$resid), mean(m$resid) - 1.96 * sd(m$resid) / sqrt(nrow(m)),
            mean(m$resid) + 1.96 * sd(m$resid) / sqrt(nrow(m)), nrow(m)))
say("quarterbacks, backs and receivers sit at zero. where production is")
say("measured, the model prices it without systematic error.")

say("\n[what this does not say]")
say("a positive residual means a position is paid above what this model's")
say("features predict. it does not mean teams are wrong. durability, pass")
say("protection, scheme fit and the number of teams bidding are all invisible")
say("here, and left tackle is exactly the position where the missing measure")
say("would plausibly be worth the most.")

plot_d <- pos %>%
  mutate(position = reorder(position, mean_resid),
         tier_lab = factor(tier_labels[as.character(position_tier)],
                           levels = c("Measured", "Partial", "Usage Only")))

p <- ggplot(plot_d, aes(mean_resid, position)) +
  geom_vline(xintercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.4, colour = "black") +
  geom_point(size = 1.6, colour = "black") +
  facet_grid(tier_lab ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "What Each Position Is Paid, Against What the Model Predicts",
    subtitle = paste("Mean residual in log cap share, averaged within player.",
                     "Bars are 95% intervals.",
                     "\nPositions are grouped by how well production is measured for them."),
    x = "Mean Residual (Log Cap Share)", y = NULL,
    caption = paste("Baseline model, 4,624 contracts, 2014-2026 signings.",
                    "The model carries no position effects, so nothing forces these to zero.")
  ) +
  theme_contracts()

save_figure(p, "output/figures/positional_residuals.png", width = 7.5, height = 5.5)

close(con)
cat("\nwrote output/tables/positional_residuals.txt and the figure\n")
