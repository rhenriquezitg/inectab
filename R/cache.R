#' Build the Parquet cache of harmonized files
#'
#' Harmonizes each requested file and writes it to
#' `cache_dir/data/dataset=<ds>/file_year=<year>/part-0.parquet`. The log of
#' unmapped or invalid records goes to `cache_dir/logs/<ds>_<year>.csv` and a
#' summary row to `cache_dir/manifest.csv`. Files already cached from the same
#' source file (same size and modification time) and the same package version
#' are skipped unless `overwrite = TRUE`.
#'
#' @param data_dir Folder with `defunciones/` and `egresos/` subfolders.
#' @param cache_dir Folder for the cache. Created if needed.
#' @param datasets Character vector, any of `"defunciones"`, `"egresos"`.
#' @param years Integer vector of years.
#' @param overwrite Rebuild even when the cache is current.
#' @return The manifest (tibble), invisibly.
#' @export
build_cache <- function(data_dir, cache_dir, datasets = c("defunciones", "egresos"),
                        years = 2000:2024, overwrite = FALSE) {
  datasets <- rlang::arg_match(datasets, inec_datasets, multiple = TRUE)
  dir.create(file.path(cache_dir, "logs"), recursive = TRUE, showWarnings = FALSE)
  man <- read_manifest(cache_dir)
  ver <- inectab_version()
  for (ds in datasets) for (y in years) {
    src <- inec_sav_path(data_dir, ds, y)
    info <- file.info(src)
    key <- man$dataset == ds & man$file_year == y
    current <- any(key) && !overwrite &&
      man$source_size[key][1] == info$size &&
      man$source_mtime[key][1] == format(info$mtime, "%Y-%m-%dT%H:%M:%S") &&
      man$package_version[key][1] == ver &&
      file.exists(cache_file(cache_dir, ds, y))
    if (current) {
      cli::cli_inform("{ds} {y}: cache is current, skipped.")
      next
    }
    t0 <- Sys.time()
    h <- harmonize_year(ds, y, path = src)
    f <- cache_file(cache_dir, ds, y)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(dplyr::select(h$data, -"dataset", -"file_year"), f)
    readr::write_csv(h$log, file.path(cache_dir, "logs", paste0(ds, "_", y, ".csv")), na = "")
    row <- tibble::tibble(
      dataset = ds, file_year = as.integer(y), n_records = nrow(h$data), n_log = nrow(h$log),
      n_icd_invalid = sum(h$log$issue == "ICD-10 code not in reference"),
      n_unmapped_codes = sum(h$log$issue == "unmapped code"),
      source_file = basename(src), source_size = info$size,
      source_mtime = format(info$mtime, "%Y-%m-%dT%H:%M:%S"),
      package_version = ver, built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    )
    man <- dplyr::bind_rows(man[!key, ], row)
    man <- man[order(man$dataset, man$file_year), ]
    readr::write_csv(man, file.path(cache_dir, "manifest.csv"))
    cli::cli_inform("{ds} {y}: {format(nrow(h$data), big.mark = ',')} records, {nrow(h$log)} log rows ({round(as.numeric(difftime(Sys.time(), t0, units = 'secs')))} s).")
  }
  invisible(man)
}

cache_file <- function(cache_dir, dataset, year) {
  file.path(cache_dir, "data", paste0("dataset=", dataset), paste0("file_year=", year), "part-0.parquet")
}

read_manifest <- function(cache_dir) {
  f <- file.path(cache_dir, "manifest.csv")
  if (!file.exists(f)) {
    return(tibble::tibble(dataset = character(), file_year = integer(), n_records = integer(),
                          n_log = integer(), n_icd_invalid = integer(), n_unmapped_codes = integer(),
                          source_file = character(), source_size = numeric(), source_mtime = character(),
                          package_version = character(), built_at = character()))
  }
  readr::read_csv(f, col_types = "ciiiiicdccc", progress = FALSE)
}

#' Open the harmonized cache
#'
#' @param cache_dir Folder given to [build_cache()].
#' @return An Arrow Dataset. Filter with dplyr verbs, then `dplyr::collect()`.
#' @export
#' @examples
#' \dontrun{
#' open_cache("C:/Datasets/ecuador/cache") |>
#'   dplyr::filter(dataset == "defunciones", substr(icd10, 1, 3) == "A15") |>
#'   dplyr::collect()
#' }
open_cache <- function(cache_dir) {
  arrow::open_dataset(
    file.path(cache_dir, "data"),
    partitioning = arrow::hive_partition(dataset = arrow::utf8(), file_year = arrow::int32())
  )
}

#' Read the logs of unmapped or invalid records from the cache
#' @inheritParams open_cache
#' @return Tibble with all log rows.
#' @export
read_cache_log <- function(cache_dir) {
  fs <- list.files(file.path(cache_dir, "logs"), pattern = "\\.csv$", full.names = TRUE)
  if (!length(fs)) return(empty_log())
  dplyr::bind_rows(lapply(fs, readr::read_csv, col_types = "ciiccc",
                          na = "", progress = FALSE))
}

#' Read the cache manifest
#' @inheritParams open_cache
#' @return Tibble, one row per cached file.
#' @export
cache_manifest <- function(cache_dir) read_manifest(cache_dir)
