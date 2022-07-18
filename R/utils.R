

# Lego Site URLs ------------------------------------------------------

lego_urls <- c(
  "brickset"  = "https://brickset.com",
  "bricklink" = "https://bricklink.com"
)


# For Exchange Rates --------------------------------------------------

forex <- function(currency = "CAD") {

  priceR::exchange_rate_latest(currency = currency) |>
    tibble::as_tibble()

}


# Growth Rates --------------------------------------------------------

# See [collection valuation](https://brickset.com/mycollection/sets/valuation).

calc_growth_rate <- function(past = 12, present = 15, avg = FALSE, n_per = 1) {

  # (present - past) / past
  ((present / past)^(1/n_per)) - 1

}


calc_avg_growth <- function(past = 12, growth_rate = 15, n_per = 1) {

  past * (1 + growth_rate)^n_per

}
