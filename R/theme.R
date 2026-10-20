# one theme for every figure in the project, so charts made weeks apart still
# look like they belong together. plain black and white with a serif face,
# which is what figures in an economics paper look like.

library(ggplot2)

theme_contracts <- function(base_size = 10) {
  theme_bw(base_size = base_size, base_family = "serif") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey92", linewidth = 0.25),
      panel.border     = element_rect(colour = "grey40", linewidth = 0.4),
      strip.background = element_rect(fill = "grey93", colour = "grey40",
                                      linewidth = 0.4),
      strip.text       = element_text(size = 9),
      plot.title       = element_text(size = 12, hjust = 0),
      plot.subtitle    = element_text(size = 8.5, colour = "grey30",
                                      lineheight = 1.1),
      plot.caption     = element_text(size = 7, colour = "grey40", hjust = 0),
      axis.title       = element_text(size = 8.5),
      axis.text        = element_text(size = 7.5, colour = "grey25")
    )
}

# tier codes are column values, not labels. they should not appear on a figure
# with an underscore in them.
tier_labels <- c(measured   = "Measured",
                 partial    = "Partial",
                 usage_only = "Usage Only",
                 excluded   = "Excluded")

save_figure <- function(plot, file, width, height) {
  ggsave(file, plot, width = width, height = height, dpi = 200, bg = "white")
}
