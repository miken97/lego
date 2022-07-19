
#' Exclusive Lego Sets
#'
#' Scrape currently available exclusive sets from [Lego](https://www.lego.com/en-ca/categories/exclusives).
#'
#' @param country_code A character string consisting of the country and language code. Defaults to `"en-ca"`.
#'
#' @return A dataframe of exclusive sets from [Lego](https://www.lego.com/en-ca/categories/exclusives).
#' @export
#'
#' @examples
#' \dontrun{
#' lego_exclusive_sets(country_code = "en-ca")
#' }
lego_exclusive_sets <- function(country_code = "en-ca") {

  lego_base_url <- "https://www.lego.com"

  path_list <- list(
    country_code,
    "categories",
    "exclusives"
  )

  lego_url <- paste0(
    c(
      lego_base_url,
      paste0(path_list, collapse = "/")
    ),
    collapse = "/"
  )

  scrape_lego_internal(lego_url)

}


#' Lego Sets for Specified Theme
#'
#' Scrape currently available sets for a specified theme from [Lego](https://www.lego.com/en-ca/themes).
#'
#' @param theme_url A character string. URL path code format. See `theme_url` column returned by `current_lego_themes()` for valid options.
#' @param country_code A character string consisting of the country and language code. Defaults to `"en-ca"`.
#'
#' @return A dataframe of exclusive sets from [Lego](https://www.lego.com/en-ca/themes).
#' @export
#'
#' @examples
#' \dontrun{
#' lego_theme_sets(theme_url = "brickheadz")
#' }
lego_theme_sets <- function(theme_url, country_code = "en-ca") {

  lego_base_url <- "https://www.lego.com"

  path_list <- list(
    country_code,
    "themes",
    theme_url
  )

  lego_url <- paste0(
    c(
      lego_base_url,
      paste0(path_list, collapse = "/")
    ),
    collapse = "/"
  )

  scrape_lego_internal(lego_url)

}


#' Current Lego Themes
#'
#' Scrape currently available themes from [Lego](https://www.lego.com/en-ca/themes).
#'
#' @param country_code A character string consisting of the country and language code. Defaults to `"en-ca"`.
#'
#' @return A dataframe of currently available Lego themes from [Lego](https://www.lego.com/en-ca/themes).
#' @export
#'
#' @examples
#' \dontrun{
#' current_lego_themes()
#' }
current_lego_themes <- function(country_code = "en-ca") {

  lego_base_url <- "https://www.lego.com"

  path_list <- list(
    country_code,
    "themes"
  )

  lego_url <- paste0(
    c(
      lego_base_url,
      paste0(path_list, collapse = "/")
    ),
    collapse = "/"
  )

  raw_html <- rvest::read_html(lego_url)

  theme <- raw_html |>
    rvest::html_elements(".kNjbTr") |>
    rvest::html_text2()

  theme_url <- raw_html |>
    rvest::html_elements("div.CategoryLeafstyles__Footer-is33yg-15.hxJIDj") |>
    rvest::html_elements("a:nth-child(1)") |>
    rvest::html_attr("href") |>
    stringr::str_remove_all(paste0("/", paste0(path_list, collapse = "/"), "/"))

  tibble::tibble(
    theme = theme,
    theme_url = theme_url
  ) |>
    tidyr::separate(.data[["theme"]], c("theme", "extra"), sep = "\n", fill = "right") |>
    dplyr::select(-.data[["extra"]])

}


# Internal Functions --------------------------------------------------

#' Internal Utility Function for Scraping Pages from [Lego](https://www.lego.com/en-ca/).
#'
#' @param lego_url A character string. URL to specific lego web page.
#'
#' @return A dataframe of data scraped from [Lego](https://www.lego.com/en-ca/).
#' @keywords internal
#'
scrape_lego_internal <- function(lego_url) {

  raw_html <- rvest::read_html(lego_url)

  pagination <- raw_html |>
    rvest::html_elements("div.Summary__TextWrapper-sc-1ki9luf-1.iFsBlp > span") |>
    rvest::html_text2() |>
    stringr::str_split("\\D") |>
    purrr::pluck(1)

  page_size <- as.integer(pagination[pagination != ""][2])
  n_results <- as.integer(pagination[pagination != ""][3])
  n_pages <- ceiling(n_results / page_size)

  output_list <- vector(mode = "list", length = n_pages)

  for (i in seq_along(output_list)) {

    raw_html_paged <- paste0(lego_url, "?page=", i) |>
      rvest::read_html()

    # extract action button which informs inventory
    action_button <- raw_html_paged |>
      rvest::html_elements("div > div.ProductLeafListingstyles__ActionRow-sc-19n1otk-2.coyJam") |>
      rvest::html_text2()

    # extract product details
    raw_html_prod_info <- raw_html_paged |>
      rvest::html_elements(".heRrTF")

    product_url <- raw_html_prod_info |>
      rvest::html_elements("a") |>
      rvest::html_attr("href") |>
      tibble::as_tibble_col("set_url") |>
      dplyr::transmute(
        products_idx = dplyr::row_number(),
        set_id = stringr::str_extract_all(.data[["set_url"]], "\\d{4,6}$", simplify = T)[,1],
        set_name = rvest::html_text2(rvest::html_elements(raw_html_prod_info, ".iqocOo")),
        inventory = snakecase::to_snake_case(action_button),
        set_url = paste0("https://www.lego.com", .data[["set_url"]])
      )

    product_info <- raw_html_prod_info |>
      rvest::html_text2() |>
      tibble::as_tibble_col("txt") |>
      dplyr::mutate(product_idx = dplyr::row_number(), .before = 1) |>
      tidyr::separate_rows(.data[["txt"]], sep = "\n") |>
      dplyr::mutate(
        col_label = dplyr::case_when(
          .data[["txt"]] %in% product_url[["set_name"]] ~ "set_name",
          stringr::str_detect(.data[["txt"]], "Price") ~ "price_raw",
          stringr::str_detect(.data[["txt"]], "rating") ~ "rating",
          stringr::str_detect(.data[["txt"]], "^Exclusive|^Hard|^New|OFF$") ~ "badge",
          TRUE ~ NA_character_
        )
      ) |>
      tidyr::pivot_wider(
        id_cols = .data[["product_idx"]],
        names_from = .data[["col_label"]],
        values_from = .data[["txt"]]
      )

    output_list[[i]] <- product_info |>
      dplyr::left_join(product_url, by = "set_name")

  }

  output_list |>
    dplyr::bind_rows() |>
    tidyr::separate(
      .data[["price_raw"]],
      into = c("price_raw", "sale_price"),
      sep = "Sale Price",
      fill = "right"
    ) |>
    dplyr::transmute(
      .data[["set_id"]],
      .data[["set_name"]],
      avg_rating = as.numeric(stringr::str_remove_all(.data[["rating"]], "Average rating | out of 5 stars")),
      badge = stringr::str_replace_all(snakecase::to_snake_case(.data[["badge"]]), "_off", "_pct_off"),
      .data[["inventory"]],
      price = as.numeric(stringr::str_extract_all(.data[["price_raw"]], "(?<=^Price)\\d{1,4}.\\d{2}", simplify = T)[,1]),
      sale_price = as.numeric(stringr::str_extract_all(.data[["sale_price"]], "\\d{1,4}.\\d{2}", simplify = T)[,1]),
      .data[["set_url"]]
    )

}
