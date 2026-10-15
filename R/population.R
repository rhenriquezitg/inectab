# Sheet name stems; non-ASCII letters are dropped before matching, so the Canar sheets match "caar".
prov_sheet_codes <- c(
  azuay = "01", bolivar = "02", caar = "03", carchi = "04", cotopaxi = "05",
  chimborazo = "06", el_oro = "07", esmeraldas = "08", guayas = "09", imbabura = "10",
  loja = "11", los_rios = "12", manabi = "13", morona = "14", napo = "15", pastaza = "16",
  pichincha = "17", tungurahua = "18", zamora = "19", galapagos = "20", sucumbios = "21",
  orellana = "22", santo_domingo = "23", santa_elena = "24"
)

#' Import INEC provincial population projections by single age and sex
#'
#' Reads `Tabulado_provincial_edad_simple_1990-2035_rev2024.xlsx` (INEC,
#' estimaciones y proyecciones de poblacion, revision 2024; values at 30 June).
#' Uses the men (`_h`) and women (`_m`) sheets of the 24 provinces and checks
#' that they add up to the both-sexes sheets (`_n`).
#'
#' @param path Path to the xlsx file.
#' @return Tidy tibble: `year`, `prov`, `sex` (`"1"` men, `"2"` women),
#'   `age` (0-100; 100 = 100 and over), `pop`.
#' @export
import_population <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) cli::cli_abort("Install {.pkg readxl}.")
  sheets <- readxl::excel_sheets(path)
  read_one <- function(sheet) {
    raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
    m <- as.matrix(raw)
    hit <- which(matrix(trimws(m) %in% c("1990", "1990.0"), nrow = nrow(m)), arr.ind = TRUE)
    if (!length(hit)) cli::cli_abort("No year header (1990) in sheet {.val {sheet}}.")
    hr <- hit[1, "row"]; yc <- hit[1, "col"]; lc <- yc - 1
    years <- suppressWarnings(as.integer(m[hr, yc:ncol(m)]))
    ok_col <- !is.na(years)
    body <- m[(hr + 1):nrow(m), , drop = FALSE]
    lab <- trimws(body[, lc])
    age <- ifelse(grepl("^100", lab), 100L, suppressWarnings(as.integer(lab)))
    keep <- !is.na(age)
    vals <- body[keep, yc:ncol(m), drop = FALSE][, ok_col, drop = FALSE]
    out <- data.frame(age = rep(age[keep], times = sum(ok_col)),
                      year = rep(years[ok_col], each = sum(keep)),
                      pop = as.numeric(vals))
    total <- as.numeric(body[which(lab == "Total")[1], yc:ncol(m)][ok_col])
    list(data = out, total = stats::setNames(total, years[ok_col]))
  }
  rows <- list(); checks <- list()
  for (p in names(prov_sheet_codes)) {
    sx <- list()
    for (s in c(h = "1", m = "2", n = "0")) {
      want <- paste0(p, "_", names(which(c(h = "1", m = "2", n = "0") == s)))
      key <- function(x) gsub("[^a-z_]", "", tolower(iconv(x, "UTF-8", "ASCII", sub = "")))
      sh <- sheets[key(sheets) == key(want)]
      if (length(sh) != 1) cli::cli_abort("Sheet {.val {want}} not found in {.file {path}}.")
      sx[[s]] <- read_one(sh)
      if (s != "0") {
        d <- sx[[s]]$data
        rows[[length(rows) + 1]] <- tibble::tibble(year = d$year, prov = prov_sheet_codes[[p]], sex = s,
                                                   age = d$age, pop = d$pop)
      }
    }
    # men + women = both sexes, and single ages add up to the sheet totals
    for (s in c("1", "2", "0")) {
      d <- sx[[s]]$data
      agg <- tapply(d$pop, d$year, sum)
      checks[[length(checks) + 1]] <- max(abs(agg - sx[[s]]$total[names(agg)]))
    }
    both <- tapply(sx[["0"]]$data$pop, paste(sx[["0"]]$data$year, sx[["0"]]$data$age), sum)
    hm <- tapply(c(sx[["1"]]$data$pop, sx[["2"]]$data$pop),
                 paste(c(sx[["1"]]$data$year, sx[["2"]]$data$year), c(sx[["1"]]$data$age, sx[["2"]]$data$age)), sum)
    checks[[length(checks) + 1]] <- max(abs(both - hm[names(both)]))
  }
  worst <- max(unlist(checks))
  if (worst > 2) cli::cli_warn("Population sheets do not add up: largest difference {worst}.")
  out <- dplyr::bind_rows(rows)
  out$year <- as.integer(out$year); out$age <- as.integer(out$age)
  dplyr::arrange(out, .data$year, .data$prov, .data$sex, .data$age)
}

#' Population denominators shipped with the package
#'
#' INEC estimaciones y proyecciones de poblacion, revision 2024, provincial
#' level by single age and sex, 1990-2035, at 30 June. Provinces follow the
#' current boundaries (Santo Domingo and Santa Elena exist in every year).
#'
#' @return Tibble: `year`, `prov`, `sex`, `age` (0-100, 100 = 100+), `pop`.
#' @export
inec_population <- function() {
  readr::read_csv(inectab_file("extdata", "population", "population_province_single_age.csv"),
                  col_types = "iccid", progress = FALSE)
}

#' Aggregate population to the bands and dimensions of a tabulation
#'
#' @param pop Output of [inec_population()].
#' @param years Years to keep.
#' @param bands Output of [age_bands()].
#' @param by Dimensions, any of `"year"`, `"age_band"`, `"sex"`, `"province"`.
#' @param geo_scheme `"dpa_2024"` or `"asis"` keep provinces 23 and 24;
#'   `"dpa_pre2007"` folds them into 17 and 09.
#' @param sex,provinces Optional filters, as in [tabulate_icd()].
#' @return Tibble with the `by` columns (labels as in [tabulate_icd()]) and `pop`.
#' @export
population_denominator <- function(pop, years, bands, by, geo_scheme = "dpa_2024",
                                   sex = NULL, provinces = NULL) {
  p <- pop[pop$year %in% years, ]
  if (geo_scheme == "dpa_pre2007") {
    p$prov[p$prov == "23"] <- "17"
    p$prov[p$prov == "24"] <- "09"
  }
  if (!is.null(sex)) p <- p[p$sex %in% sex, ]
  if (!is.null(provinces)) p <- p[p$prov %in% provinces, ]
  p$age_band <- as.character(cut_age(p$age, bands))
  p$sex <- c("1" = "Hombre", "2" = "Mujer")[p$sex]
  p$province <- p$prov
  grp <- c(if ("year" %in% by) "year", if ("age_band" %in% by) "age_band",
           if ("sex" %in% by) "sex", if ("province" %in% by) "province")
  dplyr::summarise(dplyr::group_by(p, dplyr::across(dplyr::all_of(grp))), pop = sum(.data$pop), .groups = "drop")
}
