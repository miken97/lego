
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

  inventory_raw <- readr::read_csv(
    file = inventory_path,
    col_names = TRUE,
    col_types = "ccciciiddddccddddciiiciiccccccccddcc"
  ) |>
    janitor::clean_names() |>
    dplyr::transmute(
      set_id = .data[["number"]],
      .data[["theme"]],
      sub_theme = .data[["subtheme"]],
      .data[["set_name"]],
      .data[["notes"]],
      .data[["year"]],
      .data[["pieces"]],
      .data[["minifigs"]],
      .data[["launch_date"]],
      .data[["exit_date"]],
      .data[["rrp_cad"]],
      .data[["rrp_usd"]],
      .data[["rrp_gbp"]],
      .data[["rrp_eur"]],
      width_cm = .data[["width"]],
      height_cm = .data[["height"]],
      depth_cm = .data[["depth"]],
      weight_kg = .data[["weight"]],
      qty_own = .data[["qty_owned"]],
      qty_new = .data[["qty_owned_new"]],
      qty_used = .data[["qty_owned_used"]],
      .data[["wanted"]],
      qty_want = .data[["qty_wanted"]],
      .data[["priority"]],
      .data[["value_new_usd"]],
      .data[["value_used_usd"]],
      .data[["ean"]],
      .data[["upc"]]
    )

  inventory_raw |>
    dplyr::mutate(dplyr::across(dplyr::contains("date"), ~as.Date(.x, format = "%d/%m/%Y"))) |>
    tidyr::nest(dimensions = tidyselect::all_of(c("width_cm", "height_cm", "depth_cm", "weight_kg"))) |>
    dplyr::relocate(.data[["dimensions"]], .after = .data[["priority"]]) |>
    tidyr::nest(product_ids = c(.data[["ean"]], .data[["upc"]]))

  inventory_raw

}
