#' Path to the crosswalk folder shipped with the package
#'
#' @param ... Path components appended to the crosswalk root.
#' @return A file path.
#' @export
cw_path <- function(...) {
  root <- inectab_file("extdata", "crosswalks")
  if (root == "") cli::cli_abort("Crosswalk folder not found in the installed package.")
  file.path(root, ...)
}

#' Path to a file shipped with the package
#'
#' Like `system.file(..., package = "inectab")`. When the package is not
#' installed (the app bundle deployed to shinyapps.io sources `R/` directly),
#' set `options(inectab.inst = "<path to inst>")`.
#'
#' @param ... Path components below `inst/`.
#' @return A file path.
#' @export
inectab_file <- function(...) {
  inst <- getOption("inectab.inst")
  if (!is.null(inst)) return(file.path(inst, ...))
  system.file(..., package = "inectab")
}

inectab_version <- function() {
  v <- getOption("inectab.version")
  if (!is.null(v)) return(v)
  as.character(utils::packageVersion("inectab"))
}

inec_datasets <- c("defunciones", "egresos")
inec_years <- 2000:2024

check_ds_year <- function(dataset, year) {
  dataset <- rlang::arg_match0(dataset, inec_datasets)
  if (length(year) != 1 || !year %in% inec_years) {
    cli::cli_abort("{.arg year} must be one of {min(inec_years)}-{max(inec_years)}; got {year}.")
  }
  invisible(dataset)
}

read_cw_csv <- function(path) {
  readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                  na = character(), progress = FALSE)
}

#' Read the crosswalk for one dataset and year
#'
#' @param dataset `"defunciones"` or `"egresos"`.
#' @param year Integer, 2000-2024.
#' @return A list with `variables` (raw name to standard variable) and
#'   `values` (raw code to standard code), both tibbles of character columns.
#' @export
read_crosswalk <- function(dataset, year) {
  check_ds_year(dataset, year)
  dir <- cw_path(dataset, year)
  list(
    variables = read_cw_csv(file.path(dir, "variables.csv")),
    values = read_cw_csv(file.path(dir, "values.csv"))
  )
}

#' Standard schema
#' @return Tibble with one row per standard variable.
#' @export
inec_schema <- function() read_cw_csv(cw_path("schema.csv"))

#' Canton reassignments for boundary schemes
#' @return Tibble with columns `scheme`, `from_cant`, `to_cant`, `datasets`, `years`, `evidence`.
#' @export
geo_changes <- function() read_cw_csv(cw_path("geo_changes.csv"))

#' ICD-10 codes known to INEC (3 and 4 characters)
#' @return Tibble with `code` and `label`.
#' @export
icd10_reference <- function() read_cw_csv(cw_path("reference", "icd10_inec.csv"))
