# draft position at signing.
#
# the draft is the monopsony part of this market. a team holds a player's rights
# for his whole rookie deal, so where he went says something about what teams
# believed about him before anyone had to bid. whether that still predicts pay
# once production is controlled for is one of the questions the model answers.

library(dplyr)

add_draft_features <- function(contracts) {
  out <- contracts %>%
    mutate(
      # a missing round means undrafted, not unknown. draft_year is present on
      # every row, so the player exists in the draft data either way.
      is_drafted = !is.na(draft_round),

      # undrafted is not round eight. treating it as a worse late pick would say
      # these players sit on the same curve, and they do not, they were never on
      # it. so the pick number stays missing for them and the indicator carries
      # the difference.
      draft_pick = ifelse(is_drafted, draft_overall, NA_integer_),

      # pick value falls off steeply early and flattens out, so the log is closer
      # to how teams actually value slots than the raw number.
      # zero for undrafted keeps them in the regression, with is_drafted picking
      # up the level shift. the slope is then identified off drafted players only.
      draft_pick_log = ifelse(is_drafted, log(draft_pick), 0),

      # years between entering the league and signing this deal. a first round
      # pick signing at four years out is on a very different track than a sixth
      # rounder signing at four years out.
      years_since_draft = year_signed - draft_year
    )

  stopifnot(nrow(out) == nrow(contracts))
  stopifnot(all(is.na(out$draft_pick) | out$draft_pick > 0))
  out
}
