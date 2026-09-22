# first chart. what the top of each position's market costs, in cap terms.
#
# the median market deal is still close to the minimum, so a median line would
# mostly track roster churn. positional value arguments are about the top of the
# market, so this takes the five biggest deals per position per year.

library(dplyr)
library(readr)
library(ggplot2)
library(scales)

market <- readRDS("data/processed/market.rds")
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

# specialists dropped, there are too few of them per year for a top five to mean
# anything. fb, k, p and ls all have under 120 contracts across 15 seasons.
core <- c("QB", "WR", "ED", "CB", "IDL", "LT", "S", "LB",
          "RT", "TE", "LG", "RB", "C", "RG")

# 2026 is in the sample but the league year is not finished, extensions get
# signed through the summer. leaving it in would show a drop that is really an
# incomplete count.
top5 <- market %>%
  filter(position %in% core, year_signed <= 2025) %>%
  group_by(position, year_signed) %>%
  arrange(desc(apy_share), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  summarise(n_used = n(), top5_share = mean(apy_share), .groups = "drop")

write_csv(top5, "output/tables/top5_cap_share_by_position.csv")

# same line in every panel, so each position reads against the overall level
# instead of only against itself.
league <- top5 %>%
  group_by(year_signed) %>%
  summarise(league_share = mean(top5_share), .groups = "drop")

# panels ordered by where the position sits now, so the chart reads as a ranking.
ord <- top5 %>%
  filter(year_signed >= 2023) %>%
  group_by(position) %>%
  summarise(m = mean(top5_share), .groups = "drop") %>%
  arrange(desc(m)) %>%
  pull(position)

top5$position <- factor(top5$position, levels = ord)

p <- ggplot(top5, aes(year_signed, top5_share)) +
  geom_line(data = league, aes(year_signed, league_share), inherit.aes = FALSE,
            colour = "grey55", linetype = "dashed", linewidth = 0.4) +
  geom_line(colour = "black", linewidth = 0.5) +
  facet_wrap(~position, ncol = 7) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(2012, 2018, 2024)) +
  labs(
    title = "Top of the market by position, as a share of the salary cap",
    subtitle = paste("Mean of the five largest free agent and extension contracts",
                     "signed at each position each year.",
                     "Dashed line is the fourteen-position average."),
    x = NULL, y = "share of salary cap",
    caption = paste("Source: Over The Cap via nflreadr. 2011-2025 signings;",
                    "rookie deals, franchise tags and minimum contracts excluded.",
                    "Five contracts per point.")
  ) +
  theme_bw(base_size = 10, base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.25),
    panel.border     = element_rect(colour = "grey40", linewidth = 0.4),
    strip.background = element_rect(fill = "grey93", colour = "grey40",
                                    linewidth = 0.4),
    strip.text       = element_text(size = 9),
    plot.title       = element_text(size = 12, hjust = 0),
    plot.subtitle    = element_text(size = 8.5, colour = "grey30", lineheight = 1.1),
    plot.caption     = element_text(size = 7, colour = "grey40", hjust = 0),
    axis.title.y     = element_text(size = 8.5),
    axis.text        = element_text(size = 7.5, colour = "grey25")
  )

ggsave("output/figures/cap_share_by_position.png", p,
       width = 11, height = 5, dpi = 200, bg = "white")

cat("cells with fewer than five contracts:", sum(top5$n_used < 5), "of", nrow(top5), "\n")
cat("wrote output/figures/cap_share_by_position.png\n")
