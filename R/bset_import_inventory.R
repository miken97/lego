
#' Brickset Inventory
#'
#' Import sets or parts inventory from csv exported from [brickset](https://brickset.com/export).
#'
#' @param type A character string specifying the inventory to import. One of `"sets"` or `"parts"`.
#' @param f_path A character string specifying the file path to the inventory csv.
#'
#' @return A dataframe with Lego set information.
#' @export
#'
#' @examples
#' \dontrun{
#' bset_import_inventory(type = "sets", f_path = "local")
#' }
bset_import_inventory <- function(type = "sets", f_path = "local") {

  bset_inventory <- switch(
    type,
    # "parts" = bset_import_parts_inventory(f_path),
    "sets" = bset_import_sets_inventory(f_path)
  )

  bset_inventory

}


#' Brickset Sets Inventory
#'
#' Import sets inventory from csv exported from [brickset](https://brickset.com/export).
#'
#' @param f_path A character string specifying the file path to the inventory csv.
#'
#' @return A dataframe with Lego set information.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' bset_import_sets_inventory()
#' }
bset_import_sets_inventory <- function(f_path = "local") {

  f_path <- switch(
    f_path,
    "local" = "C:/mike_nanos/r_files/lego_repo/data",
    "dl" = "C:/Users/Mike.Nanos/Downloads",
  )

  inventory_path <- paste0(f_path, "/Brickset-MySets-all.csv")

  cad_to_x <- forex_exchange_rate(
    from = "CAD",
    amount = 1,
    to = c("USD", "EUR", "GBP"),
    as_df = FALSE
  )

  inventory_raw <- readr::read_csv(
    file = inventory_path,
    col_names = TRUE,
    col_types = "ccciciiddddccddddciiiciiccccccccddcc"
  ) |>
    janitor::clean_names() |>
    dplyr::transmute(
      set_id = .data[["number"]],
      .data[["set_name"]],
      .data[["theme"]],
      sub_theme = .data[["subtheme"]],
      .data[["notes"]],
      .data[["year"]],
      .data[["pieces"]],
      .data[["minifigs"]],
      .data[["launch_date"]],
      .data[["exit_date"]],
      .data[["rrp_cad"]],
      .data[["rrp_usd"]],
      .data[["rrp_eur"]],
      .data[["rrp_gbp"]],
      qty_own = .data[["qty_owned"]],
      qty_new = .data[["qty_owned_new"]],
      qty_used = .data[["qty_owned_used"]],
      .data[["wanted"]],
      qty_want = .data[["qty_wanted"]],
      .data[["priority"]],
      width_cm = .data[["width"]],
      height_cm = .data[["height"]],
      depth_cm = .data[["depth"]],
      weight_kg = .data[["weight"]],
      value_new_cad = round(.data[["value_new_usd"]] * (1 / cad_to_x[["USD"]]), 2),
      value_used_cad = round(.data[["value_used_usd"]] * (1 / cad_to_x[["USD"]]), 2),
      rrp_usd_cad_equiv = dplyr::if_else(
        !is.na(.data[["rrp_usd"]]),
        round(.data[["rrp_usd"]] * (1 / cad_to_x[["USD"]]), 2),
        .data[["rrp_usd"]]
      ),
      rrp_eur_cad_equiv = dplyr::if_else(
        !is.na(.data[["rrp_eur"]]),
        round(.data[["rrp_eur"]] * (1 / cad_to_x[["EUR"]]), 2),
        .data[["rrp_eur"]]
      ),
      rrp_gbp_cad_equiv = dplyr::if_else(
        !is.na(.data[["rrp_gbp"]]),
        round(.data[["rrp_gbp"]] * (1 / cad_to_x[["GBP"]]), 2),
        .data[["rrp_gbp"]]
      ),
      .data[["value_new_usd"]],
      .data[["value_used_usd"]],
      growth_rate_new = round((.data[["value_new_usd"]] - .data[["rrp_usd"]]) / .data[["rrp_usd"]], 4),
      growth_rate_used = round((.data[["value_used_usd"]] - .data[["rrp_usd"]]) / .data[["rrp_usd"]], 4),
      .data[["ean"]],
      .data[["upc"]]
    )

  inventory_raw |>
    dplyr::mutate(dplyr::across(dplyr::contains("date"), ~as.Date(.x, format = "%d/%m/%Y"))) |>
    tidyr::nest(rrp_cad_equiv = tidyselect::all_of(c("rrp_usd_cad_equiv", "rrp_eur_cad_equiv", "rrp_gbp_cad_equiv"))) |>
    dplyr::relocate(.data[["rrp_cad_equiv"]], .after = .data[["rrp_gbp"]]) |>
    tidyr::nest(dimensions = tidyselect::all_of(c("width_cm", "height_cm", "depth_cm", "weight_kg"))) |>
    dplyr::relocate(.data[["dimensions"]], .after = .data[["priority"]]) |>
    tidyr::nest(product_ids = c(.data[["ean"]], .data[["upc"]]))

}
