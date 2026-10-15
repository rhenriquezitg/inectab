#' Standard output columns, in order
#' @export
std_columns <- c(
  "dataset", "file_year", "record_id",
  "event_year", "event_month", "reg_year", "adm_year", "adm_month",
  "sex", "age_value", "age_unit", "age_years",
  "prov_res", "cant_res", "prov_occ", "cant_occ", "area_res",
  "residente", "place_occ", "discharge_cond", "los_days", "sector",
  "icd10", "icd10_raw", "icd10_valid"
)

int_vars <- c("event_year", "event_month", "reg_year", "adm_year", "adm_month", "los_days")
coded_vars <- c("sex", "age_unit", "prov_res", "prov_occ", "area_res", "residente",
                "place_occ", "discharge_cond", "sector")
valid_range <- list(event_year = c(1850, 2030), reg_year = c(1850, 2030), adm_year = c(1850, 2030),
                    event_month = c(1, 12), adm_month = c(1, 12), los_days = c(0, 40000))
age_range <- list(h = c(0, 23), d = c(0, 30), m = c(0, 11), y = c(0, 125))
age_divisor <- c(h = 24 * 365.25, d = 365.25, m = 12, y = 1)

#' Harmonize one INEC file to the standard schema
#'
#' Reads the SPSS file, renames variables and recodes categories with the
#' crosswalk for that year, builds 4-digit canton codes, converts age to
#' years and builds a normalized ICD-10 code. Geography is returned as
#' delivered by INEC (after code fixes); use [apply_geo_scheme()] to
#' harmonize boundaries.
#'
#' @inheritParams read_crosswalk
#' @param data_dir Folder with `defunciones/` and `egresos/` subfolders.
#' @param path Optional explicit path to the `.sav` file; overrides `data_dir`.
#' @return A list with `data` (tibble with [std_columns]) and `log` (tibble
#'   with one row per unmapped or invalid value: `dataset`, `file_year`,
#'   `record_id`, `std_var`, `raw_value`, `issue`).
#' @export
harmonize_year <- function(dataset, year, data_dir = NULL, path = NULL) {
  dataset <- check_ds_year(dataset, year)
  if (is.null(path)) path <- inec_sav_path(data_dir, dataset, year)
  cw <- read_crosswalk(dataset, year)
  vmap <- cw$variables[cw$variables$std_var != "" & cw$variables$raw_name != "", ]

  df <- haven::read_sav(path, col_select = dplyr::all_of(vmap$raw_name))
  n <- nrow(df)
  out <- list(dataset = rep(dataset, n), file_year = rep(as.integer(year), n), record_id = seq_len(n))
  logs <- list()
  src <- function(std) vmap$raw_name[vmap$std_var == std]
  raw_of <- function(std) {
    s <- src(std)
    if (length(s) == 1) raw_chr(df[[s]]) else NULL
  }

  # --- coded variables via values.csv ---
  for (v in coded_vars) {
    raw <- raw_of(v)
    if (is.null(raw)) { out[[v]] <- rep(NA_character_, n); next }
    lk <- cw$values[cw$values$std_var == v, ]
    ok <- lk[lk$status %in% c("mapped", "NA"), ]
    code <- stats::setNames(ok$std_code, ok$raw_code)
    std <- unname(code[raw])
    std[!is.na(std) & std == ""] <- NA_character_
    unm <- which(!is.na(raw) & !raw %in% ok$raw_code)
    logs[[length(logs) + 1]] <- log_rows(dataset, year, unm, v, raw, "unmapped code")
    out[[v]] <- std
  }

  # --- integer variables ---
  for (v in int_vars) {
    raw <- raw_of(v)
    if (is.null(raw)) { out[[v]] <- rep(NA_integer_, n); next }
    ck <- as_int_chk(raw)
    val <- ck$value
    rg <- valid_range[[v]]
    oor <- !is.na(val) & (val < rg[1] | val > rg[2])
    logs[[length(logs) + 1]] <- log_rows(dataset, year, which(ck$bad | oor), v, raw, "invalid value")
    val[oor] <- NA_integer_
    out[[v]] <- val
  }

  # --- age ---
  age_raw <- raw_of("age_value")
  age <- as_int_chk(age_raw)$value
  unit <- out$age_unit
  age[is.na(unit)] <- NA_integer_
  lo <- unname(vapply(age_range, `[`, numeric(1), 1)[unit])
  hi <- unname(vapply(age_range, `[`, numeric(1), 2)[unit])
  bad_age <- which(!is.na(age) & (age < lo | age > hi))
  logs[[length(logs) + 1]] <- log_rows(dataset, year, bad_age, "age_value",
                                       paste0(age_raw, " ", unit), "age out of range for unit")
  missing_age <- which(!is.na(unit) & is.na(age))
  logs[[length(logs) + 1]] <- log_rows(dataset, year, missing_age, "age_value", age_raw,
                                       "age missing with known unit")
  out$age_value <- age
  out$age_years <- unname(age / age_divisor[unit])

  # --- cantons ---
  ref_cant <- read_cw_csv(cw_path("reference", "cantons.csv"))$cant
  for (w in c("res", "occ")) {
    cv <- paste0("cant_", w); pv <- paste0("prov_", w)
    raw_c <- raw_of(cv)
    if (is.null(raw_c)) { out[[cv]] <- rep(NA_character_, n); next }
    raw_p <- raw_of(pv)
    cant <- build_cant(raw_p, raw_c, out[[pv]], dataset, year)
    not_ref <- which(!is.na(cant) & !cant %in% ref_cant)
    mism <- which(!is.na(cant) & cant %in% ref_cant & !is.na(out[[pv]]) &
                    substr(cant, 1, 2) != out[[pv]] & !cant %in% c("8800", "9000"))
    logs[[length(logs) + 1]] <- log_rows(dataset, year, not_ref, cv, cant, "canton not in reference")
    logs[[length(logs) + 1]] <- log_rows(dataset, year, mism, cv, paste0(out[[pv]], "|", cant),
                                         "canton outside its province")
    out[[cv]] <- cant
  }

  # --- ICD-10 ---
  icd_src <- src("icd10")
  icd_src <- icd_src[order(match(tolower(icd_src), c("causa", "causa1")))]
  icd_raw <- build_icd_raw(df, dataset, year, icd_src)
  icd <- normalize_icd(icd_raw)
  valid <- icd %in% icd10_reference()$code
  valid[is.na(icd)] <- NA
  logs[[length(logs) + 1]] <- log_rows(dataset, year, which(!is.na(icd) & !valid), "icd10",
                                       icd_raw, "ICD-10 code not in reference")
  logs[[length(logs) + 1]] <- log_rows(dataset, year, which(is.na(icd)), "icd10", icd_raw,
                                       "ICD-10 code missing")
  out$icd10 <- icd
  out$icd10_raw <- icd_raw
  out$icd10_valid <- valid

  data <- tibble::as_tibble(out[std_columns])
  log <- dplyr::bind_rows(empty_log(), !!!logs)
  list(data = data, log = log)
}

build_cant <- function(raw_p, raw_c, std_p, dataset, year) {
  out <- rep(NA_character_, length(raw_c))
  has <- !is.na(raw_c)
  four <- has & nchar(raw_c) > 2
  out[four] <- stringr::str_pad(raw_c[four], 4, pad = "0")
  two <- has & !four
  ext <- two & !is.na(std_p) & std_p == "88"
  znd_unknown <- two & dataset == "egresos" & year %in% 2001:2003 & !is.na(raw_p) & raw_p == "50"
  rest <- two & !ext & !znd_unknown & !is.na(raw_p)
  out[ext] <- "8800"
  out[znd_unknown] <- "9000"
  out[rest] <- sprintf("%02d%02d", as.integer(raw_p[rest]), as.integer(raw_c[rest]))
  out
}

#' Harmonize province and canton to a boundary scheme
#'
#' @param data Harmonized data (data frame) from [harmonize_year()] or the cache.
#' @param scheme `"asis"` (as delivered), `"dpa_2024"` (current boundaries:
#'   Santo Domingo, Santa Elena and La Concordia assigned to provinces 23 and
#'   24 in every year where the canton allows it) or `"dpa_pre2007"` (fold 23
#'   and 24 back into 17 and 09, La Concordia to zonas no delimitadas).
#' @return `data` with `prov_res`, `cant_res`, `prov_occ`, `cant_occ` recoded.
#'   Where no canton exists (defunciones residence 2015+, egresos
#'   establishment 2000-2001) only the province-level rule applies.
#' @export
apply_geo_scheme <- function(data, scheme = c("asis", "dpa_2024", "dpa_pre2007")) {
  scheme <- rlang::arg_match(scheme)
  if (scheme == "asis") return(data)
  g <- geo_changes()
  g <- g[g$scheme == scheme, ]
  to <- stats::setNames(g$to_cant, g$from_cant)
  for (w in c("res", "occ")) {
    cv <- paste0("cant_", w); pv <- paste0("prov_", w)
    hit <- !is.na(data[[cv]]) & data[[cv]] %in% names(to)
    data[[cv]][hit] <- unname(to[data[[cv]][hit]])
    data[[pv]][hit] <- substr(data[[cv]][hit], 1, 2)
    if (scheme == "dpa_pre2007") {
      data[[pv]][!is.na(data[[pv]]) & data[[pv]] == "23"] <- "17"
      data[[pv]][!is.na(data[[pv]]) & data[[pv]] == "24"] <- "09"
    }
  }
  data
}
