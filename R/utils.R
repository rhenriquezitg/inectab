# Raw values as character, the way the crosswalk CSVs store them:
# numeric 1 -> "1", 2024 -> "2024", text "01" stays "01", blank -> NA.
raw_chr <- function(x) {
  if (is.null(x)) return(NULL)
  x <- unclass(x)
  attributes(x) <- NULL
  if (is.numeric(x)) {
    v <- as.numeric(x)
    out <- ifelse(is.na(v), NA_character_,
                  ifelse(v == trunc(v), sprintf("%.0f", v), as.character(v)))
    return(out)
  }
  out <- trimws(as.character(x))
  out[out == ""] <- NA_character_
  out
}

as_int_chk <- function(x) {
  v <- suppressWarnings(as.integer(x))
  bad <- !is.na(x) & is.na(v)
  list(value = v, bad = bad)
}

log_rows <- function(dataset, year, idx, std_var, raw, issue) {
  if (!length(idx)) return(NULL)
  tibble::tibble(dataset = dataset, file_year = as.integer(year), record_id = as.integer(idx),
                 std_var = std_var, raw_value = as.character(raw[idx]), issue = issue)
}

empty_log <- function() {
  tibble::tibble(dataset = character(), file_year = integer(), record_id = integer(),
                 std_var = character(), raw_value = character(), issue = character())
}

#' Locate the INEC SPSS file for a dataset and year
#'
#' @param data_dir Folder that contains `defunciones/` and `egresos/`
#'   subfolders, or the dataset folder itself.
#' @inheritParams read_crosswalk
#' @return The file path. Errors if the file is not found.
#' @export
inec_sav_path <- function(data_dir, dataset, year) {
  check_ds_year(dataset, year)
  stem <- if (dataset == "defunciones") "DEF" else "EGR"
  target <- paste0(stem, year, ".sav")
  for (d in c(file.path(data_dir, dataset), data_dir)) {
    if (!dir.exists(d)) next
    hit <- list.files(d, pattern = paste0("^", target, "$"), ignore.case = TRUE, full.names = TRUE)
    if (length(hit)) return(hit[[1]])
  }
  cli::cli_abort("File {.file {target}} not found under {.path {data_dir}}.")
}
