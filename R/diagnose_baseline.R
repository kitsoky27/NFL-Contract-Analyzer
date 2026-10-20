# checks whether the baseline is fit for the use it is about to be put to.
#
# the model exists to produce residuals, so the question is not only how much
# variance it explains but whether what is left over is structureless. a
# residual that still trends with something the model already contains means the
# functional form is wrong, and any positional result read off it is wrong too.

library(dplyr)
library(ggplot2)
source("R/theme.R")

mods <- readRDS("data/processed/baseline_models.rds")
m    <- mods$nopos
r    <- readRDS("data/processed/baseline_residuals.rds")
tbl  <- readRDS("data/processed/model_table.rds")

dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
con <- file("output/tables/baseline_diagnostics.txt", "w")
say <- function(...) { cat(..., "\n", sep = "", file = con); cat(..., "\n", sep = "") }

say("baseline diagnostics, ", format(Sys.Date()))
say("model: log cap share, no position effects, ", nrow(r), " contracts")

say("\n[residual mean by signing year]")
say("year effects are in the model, so these should be zero. a nonzero value")
say("would mean the year terms are not doing their job.")
yr <- r %>% group_by(year_signed) %>%
  summarise(n = n(), mean_resid = round(mean(resid), 4), .groups = "drop")
say(paste(capture.output(print(as.data.frame(yr), row.names = FALSE)), collapse = "\n"))

say("\n[residual mean by position tier]")
say("these are not constrained to zero and they are the first look at the")
say("result the project is after.")
ti <- r %>% group_by(position_tier) %>%
  summarise(n = n(), mean_resid = round(mean(resid), 3),
            sd = round(sd(resid), 3), .groups = "drop")
say(paste(capture.output(print(as.data.frame(ti), row.names = FALSE)), collapse = "\n"))

say("\n[residual spread across the fitted range]")
say("constant spread would mean the model is equally confident everywhere.")
q <- r %>% mutate(bin = ntile(fitted, 5)) %>% group_by(bin) %>%
  summarise(n = n(), fitted_mid = round(median(fitted), 2),
            mean_resid = round(mean(resid), 3), sd = round(sd(resid), 3), .groups = "drop")
say(paste(capture.output(print(as.data.frame(q), row.names = FALSE)), collapse = "\n"))

# the contracts the fit leans on hardest. a handful of points driving the whole
# thing would make every coefficient fragile.
say("\n[influence]")
cd <- cooks.distance(m)
say(sprintf("contracts above 4/n cook's distance: %d of %d (%.1f%%)",
            sum(cd > 4 / length(cd)), length(cd), 100 * mean(cd > 4 / length(cd))))
say(sprintf("largest cook's distance: %.3f", max(cd)))

# the floor. this is the main thing the baseline still gets wrong.
say("\n[the floor]")
floor_share <- tbl %>% group_by(year_signed) %>%
  summarise(min_share = min(apy_share), .groups = "drop")
near_min <- tbl %>% left_join(floor_share, by = "year_signed") %>%
  mutate(at_floor = apy_share <= 1.5 * min_share)
say(sprintf("contracts within 1.5x the year's lowest deal: %d (%.1f%%)",
            sum(near_min$at_floor), 100 * mean(near_min$at_floor)))
say(paste(capture.output(print(
  near_min %>% group_by(contract_type) %>%
    summarise(n = n(), at_floor = sprintf("%.1f%%", 100 * mean(at_floor)), .groups = "drop") %>%
    as.data.frame(), row.names = FALSE)), collapse = "\n"))
say("\nthe league minimum is a hard floor, so the outcome is censored from below.")
say("a player on a minimum deal was not priced by the market, he took what was")
say("there. the model cannot predict below the floor but keeps trying, which is")
say("why residuals run positive at the bottom of the fitted range.")

# figure
plot_d <- r %>% mutate(fit_pct = 100 * exp(fitted))
p <- ggplot(plot_d, aes(fitted, resid)) +
  geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.4) +
  geom_point(alpha = 0.18, size = 0.6, colour = "black") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              colour = "black", linewidth = 0.6) +
  facet_wrap(~position_tier, labeller = labeller(position_tier = tier_labels)) +
  labs(title = "Residuals Against Fitted Price, by Measurement Tier",
       subtitle = paste("Each point is one contract. The solid line is a local average;",
                        "a flat line at zero is what a correctly specified model gives."),
       x = "Fitted Log Cap Share", y = "Residual",
       caption = paste("Positive residuals at the left edge are the league minimum",
                       "acting as a floor the model cannot see.")) +
  theme_contracts()

save_figure(p, "output/figures/baseline_residuals.png", width = 9, height = 3.6)

close(con)
cat("\nwrote output/tables/baseline_diagnostics.txt and output/figures/baseline_residuals.png\n")
