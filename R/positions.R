# what the model is allowed to say about each position.
#
# box score stats do not measure every position equally. a receiver's yards are
# the thing he is paid for. a left guard has no counting stat at all, and a
# kicker barely appears in the snap data because he is never on an offensive or
# defensive snap. fitting one model and reporting one set of residuals would
# quietly treat all three as equally well measured.
#
# so every contract gets a tier, and the tier limits the claim rather than the
# sample. snaps, age, experience and draft slot exist for everyone, so a guard
# can still be priced. what he cannot get is a production coefficient that
# means anything.

library(dplyr)

add_position_tier <- function(contracts) {
  measured   <- c("QB", "RB", "WR", "TE", "FB")   # box score captures the job
  partial    <- c("ED", "IDL", "LB", "CB", "S")   # counting stats exist, weak proxies
  usage_only <- c("LT", "LG", "C", "RG", "RT")    # snaps and nothing else
  excluded   <- c("K", "P", "LS")                 # neither, they are special teams

  out <- contracts %>%
    mutate(position_tier = case_when(
      position %in% measured   ~ "measured",
      position %in% partial    ~ "partial",
      position %in% usage_only ~ "usage_only",
      position %in% excluded   ~ "excluded",
      TRUE                     ~ NA_character_
    ))

  # a new position label appearing in a later pull should stop the script rather
  # than silently fall through as missing.
  stopifnot(!any(is.na(out$position_tier)))
  out
}
