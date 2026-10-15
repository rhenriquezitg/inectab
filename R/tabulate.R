#' Count records by year, age band, sex and province for an ICD-10 selection
#'
#' @param source Cache folder (see [build_cache()]), an Arrow Dataset from
#'   [open_cache()], or a harmonized data frame.
#' @param dataset `"defunciones"` or `"egresos"`.
#' @param icd ICD-10 selection, see [parse_icd()].
#' @param years Integer vector of years to tabulate.
#' @param year_basis `"event"`: year of death or discharge, pooling every
#'   cached file (late registrations of a death sit in later files).
#'   `"file"`: the year of the INEC file (registration year for defunciones
#'   2014+).
#' @param geography `"residence"` or `"occurrence"` (place of death, or
#'   location of the establishment for egresos).
#' @param geo_scheme Boundary scheme, see [apply_geo_scheme()].
#' @param age Age band preset name or numeric lower bounds, see [age_bands()].
#' @param by Dimensions to keep, any of `"year"`, `"age_band"`, `"sex"`,
#'   `"province"`.
#' @param sex Optional filter: any of `"1"` Hombre, `"2"` Mujer, `"3"`.
#' @param provinces Optional filter: 2-digit province codes (after `geo_scheme`).
#' @param discharge Egresos only: `"all"`, `"alive"` or `"dead"`.
#' @param complete Fill missing combinations with zero counts.
#' @param rates Add population denominators and rates (see [inec_population()]).
#' @param per Rate multiplier, e.g. 100000.
#' @param standard Standard population for age-standardized rates, see
#'   [standard_population()]: `"who"` or `"ecuador_<year>"`.
#' @return A list with `table` (tidy counts; with `rates = TRUE` also
#'   `n_rate` (the numerator: residents of the 24 provinces), `pop`, `rate`,
#'   `rate_lcl`, `rate_ucl`: crude rate per `per` with exact Poisson 95% limits), `standardized` (with `rates = TRUE`: crude and directly
#'   age-standardized rates with Fay-Feuer 95% limits and the standard error
#'   `asr_se`, by every dimension in
#'   `by` except age band), `log` (unmapped or invalid records and notes),
#'   `codes` (ICD-10 codes found in the selection, with counts) and `params`.
#' @export
tabulate_icd <- function(source, dataset = c("defunciones", "egresos"), icd, years,
                         year_basis = c("event", "file"),
                         geography = c("residence", "occurrence"),
                         geo_scheme = c("dpa_2024", "asis", "dpa_pre2007"),
                         age = "5y", by = c("year", "age_band", "sex", "province"),
                         sex = NULL, provinces = NULL,
                         discharge = c("all", "alive", "dead"), complete = TRUE,
                         rates = FALSE, per = 1e5, standard = "who") {
  dataset <- rlang::arg_match(dataset)
  year_basis <- rlang::arg_match(year_basis)
  geography <- rlang::arg_match(geography)
  geo_scheme <- rlang::arg_match(geo_scheme)
  discharge <- rlang::arg_match(discharge)
  by <- rlang::arg_match(by, c("year", "age_band", "sex", "province"), multiple = TRUE)
  years <- as.integer(years)
  spec <- parse_icd(icd)
  bands <- age_bands(age)
  cache_dir <- if (is.character(source)) source else NULL

  # ---- select records (pushed down to Arrow when reading the cache) ----
  ds_data <- if (is.character(source)) open_cache(source) else source
  ds_val <- dataset
  cols <- c("file_year", "record_id", "event_year", "sex", "age_years", "age_value", "age_unit",
            "prov_res", "cant_res", "prov_occ", "cant_occ", "discharge_cond", "icd10")
  q <- dplyr::filter(ds_data, .data$dataset == ds_val)
  q <- if (year_basis == "event") dplyr::filter(q, .data$event_year %in% years)
       else dplyr::filter(q, .data$file_year %in% years)
  q <- dplyr::filter(q, !!icd_filter_expr(spec))
  if (dataset == "egresos" && discharge != "all") {
    keep <- if (discharge == "alive") "1" else c("2", "3")
    q <- dplyr::filter(q, .data$discharge_cond %in% keep)
  }
  d <- dplyr::collect(dplyr::select(q, dplyr::any_of(cols)))
  d <- apply_geo_scheme(d, geo_scheme)

  d$year <- if (year_basis == "event") d$event_year else d$file_year
  d$age_band <- cut_age(d$age_years, bands)
  d$sex_lab <- factor(dplyr::coalesce(c("1" = "Hombre", "2" = "Mujer", "3" = "Indeterminado")[d$sex], "Unknown"),
                      levels = c("Hombre", "Mujer", "Indeterminado", "Unknown"))
  pcol <- if (geography == "residence") "prov_res" else "prov_occ"
  d$province <- dplyr::coalesce(d[[pcol]], "NA")

  # ---- optional filters ----
  if (!is.null(sex)) d <- d[d$sex %in% sex, ]
  if (!is.null(provinces)) d <- d[d$province %in% provinces, ]

  # ---- counts ----
  grp <- c(if ("year" %in% by) "year", if ("age_band" %in% by) "age_band",
           if ("sex" %in% by) "sex_lab", if ("province" %in% by) "province")
  tab <- dplyr::count(d, dplyr::across(dplyr::all_of(grp)), name = "n")
  if (complete && length(grp)) {
    full <- list()
    if ("year" %in% by) full$year <- years
    if ("age_band" %in% by) full$age_band <- factor(levels(d$age_band), levels(d$age_band))
    if ("sex" %in% by) {
      sx <- c("Hombre", "Mujer")
      if (any(d$sex_lab %in% c("Indeterminado", "Unknown"))) sx <- unique(c(sx, as.character(unique(d$sex_lab))))
      if (!is.null(sex)) sx <- c("1" = "Hombre", "2" = "Mujer", "3" = "Indeterminado")[sex]
      full$sex_lab <- factor(sx, levels(d$sex_lab))
    }
    if ("province" %in% by) {
      pv <- province_codes(geo_scheme)
      pv <- union(pv, unique(d$province))
      if (!is.null(provinces)) pv <- provinces
      full$province <- sort(pv)
    }
    grid <- do.call(tidyr_expand, full)
    tab <- dplyr::left_join(grid, tab, by = grp)
    tab$n[is.na(tab$n)] <- 0L
    # drop the Unknown age row when it has no records
    if ("age_band" %in% by && !any(d$age_band == "Unknown")) tab <- tab[tab$age_band != "Unknown", ]
  }
  tab <- dplyr::arrange(tab, dplyr::across(dplyr::all_of(grp)))
  if ("province" %in% by) {
    tab$province_name <- province_label(tab$province)
    tab <- dplyr::relocate(tab, "province_name", .after = "province")
  }
  if ("sex_lab" %in% names(tab)) {
    tab$sex_lab <- as.character(tab$sex_lab)
    names(tab)[names(tab) == "sex_lab"] <- "sex"
  }
  if ("age_band" %in% names(tab)) tab$age_band <- as.character(tab$age_band)
  tab <- tibble::add_column(tab, dataset = dataset, .before = 1)

  codes <- dplyr::count(d, icd10, name = "n")
  codes <- dplyr::left_join(codes, dplyr::rename(icd10_reference(), icd10 = "code"), by = "icd10")
  codes <- dplyr::arrange(codes, dplyr::desc(.data$n))

  # ---- rates ----
  standardized <- NULL
  if (rates) {
    pop <- inec_population()
    yrs_pop <- range(pop$year)
    pop_tab <- population_denominator(pop, years, bands, by, geo_scheme, sex, provinces)
    # Numerators for rates: residents of provinces that have a denominator only
    # (Exterior, zonas no delimitadas and unknown province are left out, also
    # when province is not a breakdown).
    d_rate <- d[d$province %in% unique(pop_tab_provinces(pop, geo_scheme)), ]
    tab <- add_rates(tab, rate_counts(d_rate, grp), pop_tab, by, per)
    keys <- intersect(c("year", "sex", "province"), by)
    d_ok <- d_rate[d_rate$age_band != "Unknown", ]
    d_ok$sex_c <- as.character(d_ok$sex_lab)
    cnt <- dplyr::count(d_ok, dplyr::across(dplyr::all_of(c(
      if ("year" %in% keys) "year", if ("sex" %in% keys) "sex_c", if ("province" %in% keys) "province", "age_band"))),
      name = "n")
    names(cnt)[names(cnt) == "sex_c"] <- "sex"
    cnt$age_band <- as.character(cnt$age_band)
    full <- list()
    if ("year" %in% keys) full$year <- years
    if ("sex" %in% keys) full$sex <- if (is.null(sex)) c("Hombre", "Mujer") else
      unname(c("1" = "Hombre", "2" = "Mujer", "3" = "Indeterminado")[sex])
    if ("province" %in% keys) full$province <- if (is.null(provinces)) setdiff(province_codes(geo_scheme), c("88", "90")) else provinces
    full$age_band <- bands$label
    grid <- do.call(tidyr_expand, full)
    cnt <- dplyr::left_join(grid, cnt, by = names(full))
    cnt$n[is.na(cnt$n)] <- 0L
    pop_std <- population_denominator(pop, years, bands, c(keys, "age_band"), geo_scheme, sex, provinces)
    standardized <- standardize(cnt, pop_std, band_weights(bands, standard), keys, per)
    standardized <- tibble::add_column(standardized, dataset = dataset, .before = 1)
    standardized$standard <- standard
    if ("province" %in% keys) {
      standardized$province_name <- province_label(standardized$province)
      standardized <- dplyr::relocate(standardized, "province_name", .after = "province")
    }
  }

  params <- list(dataset = dataset, icd = paste(icd, collapse = ", "), years = years,
                 year_basis = year_basis, geography = geography, geo_scheme = geo_scheme,
                 age = if (is.character(age)) age else paste(age, collapse = ","),
                 by = by, sex = sex, provinces = provinces, discharge = discharge,
                 rates = rates, per = per, standard = if (rates) standard else NA,
                 n_records = nrow(d), package_version = inectab_version(),
                 run_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
  log <- tabulation_log(d, spec, bands, dataset, years, year_basis, geography, geo_scheme, cache_dir, pcol,
                        rates = rates, standard = standard)
  list(table = tibble::as_tibble(tab), standardized = standardized, log = log, codes = codes, params = params)
}

tidyr_expand <- function(...) {
  args <- list(...)
  g <- expand.grid(args, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  for (nm in names(args)) if (is.factor(args[[nm]])) g[[nm]] <- factor(g[[nm]], levels(args[[nm]]))
  tibble::as_tibble(g[do.call(order, unname(as.list(g))), , drop = FALSE])
}

province_codes <- function(geo_scheme) {
  p <- c(sprintf("%02d", 1:22), "23", "24", "88", "90")
  if (geo_scheme == "dpa_pre2007") p <- setdiff(p, c("23", "24"))
  p
}

#' Province names
#' @param code 2-digit province codes.
#' @return Character vector of names; `"NA"` becomes `"Unknown"`.
#' @export
province_label <- function(code) {
  lab <- c(`01` = "Azuay", `02` = "Bolivar", `03` = "Canar", `04` = "Carchi", `05` = "Cotopaxi",
           `06` = "Chimborazo", `07` = "El Oro", `08` = "Esmeraldas", `09` = "Guayas",
           `10` = "Imbabura", `11` = "Loja", `12` = "Los Rios", `13` = "Manabi",
           `14` = "Morona Santiago", `15` = "Napo", `16` = "Pastaza", `17` = "Pichincha",
           `18` = "Tungurahua", `19` = "Zamora Chinchipe", `20` = "Galapagos", `21` = "Sucumbios",
           `22` = "Orellana", `23` = "Santo Domingo de los Tsachilas", `24` = "Santa Elena",
           `88` = "Exterior", `90` = "Zonas no delimitadas", `NA` = "Unknown")
  unname(dplyr::coalesce(lab[code], code))
}

tabulation_log <- function(d, spec, bands, dataset, years, year_basis, geography, geo_scheme,
                           cache_dir, pcol, rates = FALSE, standard = "who") {
  out <- list()
  add <- function(level, issue, n = NA_integer_, file_year = NA_integer_, record_id = NA_integer_,
                  field = NA_character_, value = NA_character_) {
    out[[length(out) + 1]] <<- tibble::tibble(level = level, dataset = dataset,
                                              file_year = as.integer(file_year),
                                              record_id = as.integer(record_id), field = field,
                                              value = as.character(value), n = as.integer(n),
                                              issue = issue)
  }
  # selection
  for (i in which(!is.na(spec$warning))) add("selection", spec$warning[i], field = "icd", value = spec$item[i])
  # selected records with missing fields
  miss <- list(age = is.na(d$age_years), sex = is.na(d$sex), province = is.na(d[[pcol]]))
  for (f in names(miss)) {
    idx <- which(miss[[f]])
    if (length(idx)) {
      out[[length(out) + 1]] <- tibble::tibble(
        level = "record", dataset = dataset, file_year = d$file_year[idx], record_id = d$record_id[idx],
        field = f, value = NA_character_, n = 1L, issue = paste(f, "unknown in a selected record"))
    }
  }
  # harmonization log for the files read
  if (!is.null(cache_dir)) {
    hl <- read_cache_log(cache_dir)
    hl <- hl[hl$dataset == dataset, ]
    # files that can hold selected records: the selected years, plus later
    # files for defunciones by year of death (late registrations)
    hl <- if (year_basis == "file" || dataset == "egresos") hl[hl$file_year %in% years, ]
          else hl[hl$file_year >= min(years), ]
    sel <- paste(d$file_year, d$record_id)
    keep <- (hl$std_var == "icd10") | (paste(hl$file_year, hl$record_id) %in% sel)
    hl <- hl[keep, ]
    if (nrow(hl)) {
      out[[length(out) + 1]] <- tibble::tibble(
        level = ifelse(hl$std_var == "icd10" & !paste(hl$file_year, hl$record_id) %in% sel,
                       "harmonization (not selectable)", "harmonization"),
        dataset = dataset, file_year = hl$file_year, record_id = hl$record_id, field = hl$std_var,
        value = hl$raw_value, n = 1L, issue = hl$issue)
    }
  }
  # notes
  if (nrow(d)) {
    if ("age_band" %in% names(d) && max(bands$lower) > 95 && any(d$file_year <= 2012)) {
      add("note", "Ages above 98-99 are not recorded before 2013 (top-coding); the top age band is incomplete for those years.")
    }
    if (dataset == "defunciones" && year_basis == "event") {
      mx <- if (!is.null(cache_dir)) max(cache_manifest(cache_dir)$file_year[cache_manifest(cache_dir)$dataset == dataset]) else max(d$file_year)
      if (any(years >= mx)) add("note", sprintf("Deaths in %s include only registrations made up to %s; late registrations will appear in later files.", mx, mx))
      if (any(years <= 2012)) add("note", "Defunciones 2000-2013 files hold only deaths registered in the same year. Deaths up to 2012 registered late in 2013 or earlier are in no file; those registered from 2014 are included.")
    }
    if (dataset == "defunciones" && geography == "residence" && geo_scheme == "dpa_pre2007" && any(d$file_year >= 2015)) {
      add("note", "Defunciones 2015+ have no canton of residence: La Concordia residents stay in Pichincha (17) instead of zonas no delimitadas under dpa_pre2007.")
    }
    if (dataset == "egresos" && geography == "occurrence" && geo_scheme == "dpa_2024" && any(d$file_year <= 2001)) {
      add("note", "Egresos 2000-2001 have no establishment canton: discharges from Santo Domingo and Santa Elena stay in Pichincha (17) and Guayas (09).")
    }
    if (rates) {
      no_den <- sum(is.na(d$age_years) | !d$sex %in% c("1", "2") | !d[[pcol]] %in% sprintf("%02d", 1:24))
      if (no_den) add("note", "Selected records without a population denominator (unknown age, sex or province; sex Indeterminado; Exterior; zonas no delimitadas). They are counted in the table but left out of rates, so rates for totals are slightly low.", n = no_den)
      add("note", "Denominators: INEC estimaciones y proyecciones de poblacion, revision 2024, provincial by single age and sex, at 30 June. Provinces follow current boundaries in every year.")
      if (standard == "who" && any(bands$upper - bands$lower < 5 & bands$lower < 100)) {
        add("note", "WHO standard weights are given in 5-year groups; they were spread evenly over single ages to fit bands narrower than 5 years.")
      }
      if (geo_scheme == "asis" && any(d$file_year <= 2007)) add("note", "Boundaries as delivered: before 2008, counts for Pichincha (17) and Guayas (09) include Santo Domingo and Santa Elena, but the denominators do not. Use dpa_2024 for rates.")
      if (geography == "occurrence") add("note", "Rates by place of occurrence divide by the resident population of that province; they are not population-based rates for residents.")
      if (dataset == "egresos") add("note", "Egresos rates count discharges, not persons: one person can be discharged more than once.")
    }
    nz <- sum(d[[pcol]] %in% "90"); ne <- sum(d[[pcol]] %in% "88")
    if (ne) add("note", "Records with province 88 (Exterior).", n = ne)
    if (nz) add("note", "Records with province 90 (Zonas no delimitadas).", n = nz)
  }
  dplyr::bind_rows(tibble::tibble(level = character(), dataset = character(), file_year = integer(),
                                  record_id = integer(), field = character(), value = character(),
                                  n = integer(), issue = character()), !!!out)
}

#' Write a tabulation to tidy CSV files
#'
#' @param x Result of [tabulate_icd()].
#' @param dir Output folder; created if needed.
#' @param prefix File name prefix.
#' @return Paths written, invisibly: `<prefix>_table.csv`, `<prefix>_log.csv`,
#'   `<prefix>_codes.csv`, `<prefix>_params.csv`, and `<prefix>_standardized.csv`
#'   when rates were computed.
#' @export
write_tabulation <- function(x, dir, prefix = "inectab") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  p <- file.path(dir, paste0(prefix, c("_table.csv", "_log.csv", "_codes.csv", "_params.csv")))
  readr::write_csv(x$table, p[1], na = "")
  readr::write_csv(x$log, p[2], na = "")
  readr::write_csv(x$codes, p[3], na = "")
  readr::write_csv(params_table(x$params), p[4], na = "")
  if (!is.null(x$standardized)) {
    p <- c(p, file.path(dir, paste0(prefix, "_standardized.csv")))
    readr::write_csv(x$standardized, p[5], na = "")
  }
  invisible(p)
}

params_table <- function(params) {
  tibble::tibble(parameter = names(params),
                 value = vapply(params, function(v) paste(v, collapse = ", "), character(1)))
}
