

# Lego Site URLs ------------------------------------------------------

lego_urls <- c(
  "brickset"  = "https://brickset.com",
  "bricklink" = "https://bricklink.com"
)


# For Exchange Rates --------------------------------------------------

# see <https://exchangerate.host/#/#docs> for documentation.

#' Foreign Exchange Rate Conversion
#'
#' See [here](https://exchangerate.host/#/#docs) for documentation.
#'
#' @param from A character string. Three-letter currency abbreviation.
#' @param amount A numerical scalae. The amount to be converted.
#' @param to A character string or vector. Three-letter currency abbreviation(s).
#' @param as_df A logical scalar. Return as dataframe or vector (if `FALSE`).
#'
#' @return A dataframe or vector of currency equivalents.
#' @keywords internal
#'
forex_exchange_rate <- function(from = "CAD", amount = 1, to = c("USD", "EUR", "GBP"), as_df = TRUE) {


  # construct request url ----

  exchange_rate_url <- "https://api.exchangerate.host"

  path_list <- list(
    "latest"
  )

  currency_symbols <- paste0(to, collapse = ",")

  query_list <- list(
    base = "CAD",
    symbols = currency_symbols,
    amount = amount
  )


  # perform api request and handle response ----

  api_req <- httr2::request(exchange_rate_url) |>
    httr2::req_url_path_append(paste0(path_list, collapse = "/")) |>
    httr2::req_url_query(!!!query_list) |>
    httr2::req_perform()

  api_resp <- api_req |>
    httr2::resp_body_json() |>
    purrr::pluck("rates")

  if (as_df) {

    api_resp |>
      dplyr::bind_rows() |>
      dplyr::mutate("{from}" := amount, .before = 1)

  } else {

    vctrs::vec_set_names(
      x = purrr::map_dbl(api_resp, ~.x[1]),
      names = names(api_resp)
    )

  }

}


# forex <- function(currency = "CAD") {
#
#   priceR::exchange_rate_latest(currency = currency) |>
#     tibble::as_tibble() |>
#     dplyr::rename(one_cad_equiv = .data[["one_cad_is_equivalent_to"]])
#
# }


# Growth Rates --------------------------------------------------------

# See [collection valuation](https://brickset.com/mycollection/sets/valuation).

calc_growth_rate <- function(past = 12, present = 15, n_per = 1) {

  # (present - past) / past
  ((present / past)^(1/n_per)) - 1

}


calc_avg_growth <- function(past = 12, growth_rate = 15, n_per = 1) {

  past * (1 + growth_rate)^n_per

}

