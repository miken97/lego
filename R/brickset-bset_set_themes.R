
#' Bricket Lego Themes
#'
#' Scrape list of Lego themes from [brickset](https://brickset.com/browse/sets).
#'
#' @return A dataframe with themes and link to corresponding brickset webpage.
#' @export
#'
#' @examples
#' \dontrun{
#' bset_set_themes()
#' }
bset_set_themes <- function() {

  bset_base_url <- lego_urls[["brickset"]]

  html_raw <- paste0(bset_base_url, "/browse/sets") |>
    rvest::read_html() |>
    rvest::html_elements("#body > div.outerwrap > div > div > section:nth-child(2)") |>
    rvest::html_elements("a")

  all_themes <- tibble::tibble(
    theme = rvest::html_text2(html_raw),
    bset_theme_url = rvest::html_attr(html_raw, "href")
  )

  all_themes |>
    dplyr::mutate(bset_theme_url = paste0(bset_base_url, .data[["bset_theme_url"]])) |>
    dplyr::arrange(.data[["theme"]])

}
