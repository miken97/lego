
# Step 0: Resources -------------------------------------------------------

# pkg development resources

# <https://www.pipinghotdata.com/posts/2020-10-25-your-first-r-package-in-1-hour>
# <https://rtask.thinkr.fr/when-development-starts-with-documentation>
# <https://github.com/ThinkR-open/attachment/blob/master/devstuff_history.R>
# <https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html>


# Step 1: Create Pkg ------------------------------------------------------

# create pkg
usethis::create_package("C:/mike_nanos/r_files/lego")


# Step 2: Development History ---------------------------------------------

# create dev_history.R and hide from pkg build
# > save dev_history.R in project root
usethis::use_build_ignore("dev_history.R")


# Step 3: Git Connection --------------------------------------------------

# connect local pkg to git repo
usethis::use_git()
usethis::use_github()


# Step 4: Description & License -------------------------------------------

# add mit license and update description file
usethis::use_mit_license("Mike Nanos")
# > update description file manually

# add pipe
# usethis::use_pipe()

# use tidy evaluation
usethis::use_tidy_eval()

# document and run first check
devtools::document()
devtools::check()


# Step 5: Functions & Dependencies ----------------------------------------

# create global variables in utils.R
usethis::use_r("utils")

# create data description file data.R
# usethis::use_r("data")

# create datasets
# usethis::use_data_raw("")


## utility functions ----
usethis::use_r("utils")

## bricklink functions ----
usethis::use_r("blink_price_guide")
usethis::use_r("blink_colour_guide")

## brickset functions ----
usethis::use_r("bset_import_inventory")
usethis::use_r("bset_set_themes")

## ebay functions ----
# usethis::use_r("ebay_sold_listings")


# document functions
# > insert roxygen skeleton
devtools::document()

# specify package dependencies
usethis::use_package("dplyr")
usethis::use_package("glue")
usethis::use_package("janitor")
usethis::use_package("lubridate")
usethis::use_package("naniar")
usethis::use_package("priceR")
usethis::use_package("purrr")
usethis::use_package("readr")
usethis::use_package("rlang")
usethis::use_package("rvest")
usethis::use_package("snakecase")
usethis::use_package("stringr")
usethis::use_package("tibble")
usethis::use_package("tidyr")
usethis::use_package("tidyselect")


# check build
devtools::check()


# Step 6: Package Site ----------------------------------------------------

# build site
pkgdown::build_site()

# add pkgdown, yml and docs to .buildignore
usethis::use_build_ignore("_pkgdown.yml")
usethis::use_build_ignore("^pkgdown$")

# generate readme
usethis::use_readme_rmd()
devtools::build_readme()


# library(tidyverse)
#
# res <- r_resources()
#
# types <- sort(unique(res$type))
#
# topics <- res |>
#   select(topics) |>
#   separate_rows(topics, sep = ", ") |>
#   count(topics, sort = T) |>
#   arrange(topics)
#
# types
# topics |> view()
#
#
# topics_na <- res |>
#   transmute(
#     row_id = row_number(),
#     topics
#   ) |>
#   separate_rows(topics, sep = ", ") |>
#   filter(is.na(topics)) |>
#   pull(row_id)
#
# res |>
#   # filter(row_number() == 302)
#   filter(row_number() %in% topics_na)
#
#
# res |>
#   transmute(
#     row_id = row_number(),
#     topics
#   ) |>
#   separate_rows(topics, sep = ", ") |>
#   filter(topics == "NA")
