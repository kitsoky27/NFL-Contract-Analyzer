# NFL contract analyzer

A pricing model for NFL player contracts. This is a labor economics
project about how a market prices productivity.

## The question

I am fitting what NFL teams actually pay as a function of what you can observe
about a player before he signs. Position, age, experience, draft slot, and
production up to that point. The gap between the fitted value and what he really
signed for is the residual. If one position's residuals are consistently
positive, the market pays that position more than the rest of the pricing
function predicts (it is the same method used to value houses).

Everything gets normalized to share of that year's salary cap. A 20 million
dollar deal in 2015 and a 20 million dollar deal in 2025 are not the same thing,
the cap roughly doubled in between.

## How to run it

Open `nfl-contract-analyzer.Rproj` in RStudio, then:

```r
source("R/setup.R")   # installs packages, once
```

## Things I already know will be a problem

Production is measurable for quarterbacks and skill positions and much worse for
linemen and most defensive roles, so I will probably have to restrict the model
to positions where output is observable.

Players who do not work out get cut, so the pool that survives to a second
contract is selected. Marginal revenue product is not observable either, on
field production is a loose proxy for it.

Reported contract value is usually inflated relative to the cash a player sees.
Guaranteed money is closer to real and I want to model it as a second outcome.

The biggest technical risk is look-ahead bias, a deal signed in March 2023 was
priced on what the player had done through 2022. If I train on stats from later
seasons the model knows the future and every number is meaningless. I want the
pipeline built so that cannot happen, not something I check by hand each time.

A residual also means a player was paid differently from players the
model thinks are comparable, it does not mean he is overpaid. The gap can easily
be something the model cannot see (ex: like durability, scheme fit, or three teams
bidding against each other).
