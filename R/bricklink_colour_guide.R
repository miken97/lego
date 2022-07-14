
#' Bricklink Colour Guide/Catalogue
#'
#' Scrape the colour guide from [Bricklink](https://www.bricklink.com/catalogColors.asp) which includes information pertaining to number of parts, number of sets, wanted, quantities for sale, year start and year end.
#'
#' @return A dataframe with colour information.
#' @export
#'
#' @importFrom naniar replace_with_na
#'
#' @examples
#' \dontrun{
#' bricklink_colour_guide()
#' }
bricklink_colour_guide <- function() {

  bricklink_colour_guide_url <- "https://www.bricklink.com/catalogColors.asp?utm_content=subnav"

  lego_colours <- c(
    "Solid",
    "Transparent",
    "Chrome",
    "Pearl",
    "Satin",
    "Metallic",
    "Milky",
    "Glitter",
    "Speckle",
    "Modulex"
  )

  tbl_headers <- c(
    "id",
    "colour_name",
    "parts",
    "in_sets",
    "wanted",
    "for_sale",
    "colour_timeline"
  )

  html_raw <- rvest::read_html(bricklink_colour_guide_url) |>
    rvest::html_table()

  tbl_list_raw <- html_raw |>
    purrr::keep(purrr::map_lgl(html_raw, ~dim(.x)[[2]] == 9L)) |>
    purrr::set_names(nm = lego_colours) |>
    dplyr::bind_rows(.id = "colour_type") |>
    janitor::remove_empty("cols") |>
    purrr::set_names(c("colour_type", tbl_headers)) |>
    dplyr::filter(.data[["id"]] != "ID")

  colour_guide_tbl <- tbl_list_raw |>
    dplyr::mutate(colour_timeline = stringr::str_replace_all(.data[["colour_timeline"]], "\\s.\\s", ",")) |>
    tidyr::separate(
      .data[["colour_timeline"]],
      into = c("year_start", "year_end"),
      sep = ",",
      convert = T,
      fill = "right"
    ) |>
    dplyr::mutate(
      dplyr::across(tidyselect::all_of(c("id", "parts", "in_sets", "wanted", "for_sale")), ~as.integer(.x)),
      dplyr::across(tidyselect::vars_select_helpers$where(is.character), ~snakecase::to_snake_case(.x))
    ) |>
    naniar::replace_with_na(replace = list(year_start = "", year_end = "?")) |>
    dplyr::mutate(year_start = as.integer(.data[["year_start"]]))
    # dplyr::mutate(
    #   year_start = dplyr::if_else(.data[["year_start"]] == "", NA, .data[["year_start"]]),
    #   year_end = dplyr::if_else(.data[["year_end"]] == "?", NA, .data[["year_end"]])
    # )

  colour_guide_tbl

}
