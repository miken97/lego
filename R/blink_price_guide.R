
# Scrape Bricklink Price Guide ----------------------------------------

#' Current Items for Sale
#'
#' Scrape currently for sale new or used items from the bricklink price guide.
#'
#' @param set_id A numeric scalar or character. unique Lego set identifier.
#' @param url_only A logical scalar. If `TRUE`, URL is returned. Defaults to `FALSE`.
#'
#' @return A data frame with three columns containing price and quantity of used items for sale.
#' @export
#'
#' @examples
#' \dontrun{
#' bricklink_price_guide(set_id = 10255)
#' }
blink_price_guide <- function(set_id, url_only = FALSE) {

  blink_set_url <- glue::glue('{lego_urls[["bricklink"]]}/catalogPG.asp?S={set_id}-1&ColorID=0')

  if (url_only) {
    blink_set_url

  } else {

    raw_html <- rvest::read_html(blink_set_url)

    # set-up output list
    output_list <- list(
      sold_new      = NULL,
      sold_used     = NULL,
      for_sale_new  = NULL,
      for_sale_used = NULL
    )

    listings_test <- raw_html |>
      rvest::html_elements(".fv table td") |>
      rvest::html_text2() |>
      tibble::as_tibble_col("price_guide_txt") |>
      dplyr::filter(stringr::str_detect(.data[["price_guide_txt"]], "^Times Sold: \t|^Total Lots: \t|\\(Unavailable")) |>
      dplyr::transmute(
        listing_type = c("sold_new", "sold_used", "for_sale_new", "for_sale_used"),
        is_available = stringr::str_detect(.data[["price_guide_txt"]], "\\(Unavailable", negate = T)
      ) |>
      tibble::deframe()


    # currently for sale listings ----

    if (listings_test[["for_sale_new"]]) {
      output_list[["for_sale_new"]] <- extract_for_sale(raw_html, type = "new")

    }

    if (listings_test[["for_sale_used"]]) {
      output_list[["for_sale_used"]] <- extract_for_sale(raw_html, type = "used")

    }


    # previously sold listings ----

    if (sum(listings_test[c("sold_new", "sold_used")]) > 0L) {

      all_months <- paste0(
        as.character(
          lubridate::month(seq.int(1L, 12L), label = T, abbr = F)
        ),
        collapse = "|"
      )

      sold_raw <- raw_html |>
        rvest::html_elements("tr~ tr+ tr td") |>
        rvest::html_text2() |>
        tibble::as_tibble_col("sold_txt") |>
        dplyr::filter(
          stringr::str_detect(.data[["sold_txt"]], all_months) &
            stringr::str_detect(.data[["sold_txt"]], "\\s\tQty\tEach\n\t")
        )

      if (listings_test[["sold_new"]]) {
        output_list[["sold_new"]] <- extract_sold(sold_raw, all_months, type = "new")
      }

      if (listings_test[["sold_used"]]) {
        output_list[["sold_used"]] <- extract_sold(sold_raw, all_months, type = "used")

      }

    }

    dplyr::bind_rows(purrr::compact(output_list), .id = "type")

  }

}


## Internal Functions -------------------------------------------------

extract_for_sale_internal <- function(raw_html, type) {

  rlang::arg_match(type, c("new", "used"))

  html_node <- switch(
    type,
    "new"  = "tr > td:nth-child(3)",
    "used" = "tr > td:nth-child(4)"
  )

  for_sale_raw <- raw_html |>
    rvest::html_elements(html_node) |>
    rvest::html_text2() |>
    tibble::as_tibble_col("for_sale_txt")

  for_sale <- for_sale_raw |>
    dplyr::filter(stringr::str_detect(.data[["for_sale_txt"]], "^Currently Available")) |>
    tidyr::separate_rows(.data[["for_sale_txt"]], sep = "\n \t") |>
    dplyr::filter(stringr::str_detect(.data[["for_sale_txt"]], "^Currently Available", negate = T)) |>
    dplyr::filter(stringr::str_detect(.data[["for_sale_txt"]], "^\\d")) |>
    tidyr::separate(.data[["for_sale_txt"]], c("qty", "each"), sep = "\t\\s~?", convert = T) |>
    dplyr::transmute(
      month_year = format(Sys.Date(), "%B %Y"),
      .data[["qty"]],
      price_usd = dplyr::if_else(
        stringr::str_detect(.data[["each"]], "^US"),
        readr::parse_number(.data[["each"]]),
        NA_real_
      )
    )

  for_sale

}


extract_sold_internal <- function(sold_raw, all_months, type) {

  rlang::arg_match(type, c("new", "used"))

  sold_raw_by_type <- switch(
    type,
    "new"  = dplyr::slice_head(sold_raw, n = 1L),
    "used" = dplyr::slice_tail(sold_raw, n = 1L)
  )

  sold <- sold_raw_by_type |>
    # slice_head(n = 1L) |>
    tidyr::separate_rows(.data[["sold_txt"]], sep = "\n\t") |>
    dplyr::mutate(
      month_col = stringr::str_extract_all(
        string = dplyr::lag(.data[["sold_txt"]], 1),
        pattern = paste0("(", all_months, ") \\d{4}"),
        simplify = T
      )[,1]
    ) |>
    dplyr::filter(!is.na(.data[["month_col"]])) |>
    dplyr::mutate(month_col = dplyr::if_else(.data[["month_col"]] == "", NA_character_, .data[["month_col"]])) |>
    tidyr::fill(.data[["month_col"]], .direction = "down") |>
    tidyr::separate(.data[["sold_txt"]], c("sold_txt_raw", "extra"), sep = "\n\n\n", fill = "right") |>
    tidyr::separate(.data[["sold_txt_raw"]], c("qty", "each"), sep = "\t\\s~?", convert = T) |>
    dplyr::transmute(
      month_year = .data[["month_col"]],
      .data[["qty"]],
      price_usd = dplyr::if_else(
        stringr::str_detect(.data[["each"]], "^US"),
        readr::parse_number(.data[["each"]]),
        NA_real_
      )
    )

  sold

}


# Helper Functions ----------------------------------------------------

#' Current Items for Sale
#'
#' New or used items currently for sale scraped from the bricklink price guide.
#'
#' @param raw_html An list object of class `xml_document` imported by `rvest::read_html()`.
#' @param type A character string specifying the type of listing. Either `"new"` or `"used"`.
#'
#' @return A data frame with three columns containing price and quantity of used items for sale.
#' @keywords internal
#'
extract_for_sale <- purrr::possibly(
  .f = extract_for_sale_internal,
  otherwise = tibble::tibble(
    month_year = c(NA_character_),
    qty = c(NA_character_),
    price_usd = c(NA_character_)
  )
)


#' Previously Sold Items
#'
#' Previously sold new or used items scraped from the bricklink price guide.
#'
#' @param sold_raw A dataframe with raw text extracted from the `xml_document` imported by `rvest::read_html()`.
#' @param all_months A character string with month labels for regex.
#' @param type A character string specifying the type of listing. Either `"new"` or `"used"`.
#'
#' @return A dataframe with three columns containing price and quantity of new or used items sold in the past six months.
#' @keywords internal
#'
extract_sold <- purrr::possibly(
  .f = extract_sold_internal,
  otherwise = tibble::tibble(
    month_year = c(NA_character_),
    qty = c(NA_character_),
    price_usd = c(NA_character_)
  )
)
