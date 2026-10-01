# age and experience at signing.
#
# both are known on signing day, so unlike production there is no look-ahead
# question here. they still live in one function so the model table gets built
# from named parts instead of a pile of mutates.

library(dplyr)

add_player_attributes <- function(contracts, players) {
  pl <- players %>%
    filter(!is.na(gsis_id)) %>%
    distinct(gsis_id, .keep_all = TRUE) %>%
    select(gsis_id, birth_date, rookie_season)

  out <- contracts %>%
    left_join(pl, by = "gsis_id") %>%
    mutate(
      # the players table is iso dates and covers 99.3%, so it leads. the
      # contracts column spells the month out, which only parses under an
      # english locale, so it is the fallback rather than the source.
      dob_players   = as.Date(birth_date),
      dob_contracts = as.Date(date_of_birth, format = "%B %d, %Y"),
      dob           = coalesce(dob_players, dob_contracts),

      # the league year opens in mid march, so that is the moment a signing
      # happens for age purposes. january 1 would be off by a quarter of a year.
      age_signing = as.numeric(as.Date(paste0(year_signed, "-03-15")) - dob) / 365.25,

      # rookie_season covers undrafted players, who have no draft year to count
      # from. draft_year is the fallback.
      exp_years = year_signed - coalesce(rookie_season, draft_year)
    )

  # one birth date in the data is off by about a thousand years, and a handful
  # disagree between the two sources by more than a year. anything implausible
  # becomes missing rather than a number the model will happily fit.
  out <- out %>%
    mutate(
      age_implausible = !is.na(age_signing) & (age_signing < 18 | age_signing > 45),
      age_signing     = ifelse(age_implausible, NA_real_, age_signing),
      exp_implausible = !is.na(exp_years) & (exp_years < 0 | exp_years > 25),
      exp_years       = ifelse(exp_implausible, NA_real_, exp_years)
    ) %>%
    select(-birth_date, -rookie_season, -dob_players, -dob_contracts)

  stopifnot(nrow(out) == nrow(contracts))
  out
}
