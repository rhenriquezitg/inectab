#' Parse an ICD-10 selection
#'
#' Accepts codes, prefixes and ranges separated by commas, semicolons or
#' spaces. Formats are normalized (`A15.0`, `a150`, `A15X`).
#' \itemize{
#'   \item `A15` (or `A15X`): category A15 and all its subcategories.
#'   \item `A150` or `A15.0`: that subcategory only.
#'   \item `J` or `J1`: any code starting with these characters.
#'   \item `A15-A19`: categories A15 to A19 inclusive, with subcategories.
#'   \item `I21.0-I21.4`: subcategories in that range (records coded with the
#'     3-character category I21 are not included).
#'   \item `!C44`: exclude; applied after the inclusions.
#' }
#'
#' @param x Character vector; elements may hold several items.
#' @return A tibble with `item`, `type` (`prefix` or `range`), `exclude`,
#'   `from`, `to`, `n_ref` (number of reference codes matched) and `warning`.
#' @export
#' @examples
#' parse_icd("A15-A19, U07.1, !A17, J")
parse_icd <- function(x) {
  items <- unlist(strsplit(paste(x, collapse = ","), "[,;[:space:]]+"))
  items <- items[items != ""]
  if (!length(items)) cli::cli_abort("No ICD-10 code given.")
  ref <- icd10_reference()$code
  rows <- lapply(items, function(it) {
    excl <- startsWith(it, "!")
    body <- sub("^!", "", it)
    parts <- strsplit(body, "-", fixed = TRUE)[[1]]
    warn <- NA_character_
    if (length(parts) == 2) {
      a <- normalize_icd(parts[1]); b <- normalize_icd(parts[2])
      type <- "range"
      if (is.na(a) || is.na(b) || nchar(a) != nchar(b) || !grepl("^[A-Z][0-9]{2,3}$", a) ||
          !grepl("^[A-Z][0-9]{2,3}$", b)) {
        warn <- "range ends must both be 3- or both 4-character codes"
      } else if (a > b) {
        warn <- "range start is after range end"
      }
    } else if (length(parts) == 1) {
      a <- normalize_icd(parts[1]); b <- a; type <- "prefix"
      if (is.na(a) || !grepl("^[A-Z][0-9]{0,3}$", a)) warn <- "not a valid ICD-10 code or prefix"
    } else {
      a <- NA_character_; b <- NA_character_; type <- "invalid"
      warn <- "not a valid ICD-10 code, prefix or range"
    }
    n_ref <- if (is.na(warn)) sum(icd_match_one(ref, type, a, b)) else 0L
    if (is.na(warn) && n_ref == 0) warn <- "no code in the ICD-10 reference matches"
    tibble::tibble(item = it, type = type, exclude = excl, from = a, to = b,
                   n_ref = as.integer(n_ref), warning = warn)
  })
  dplyr::bind_rows(rows)
}

icd_match_one <- function(code, type, from, to) {
  if (type == "prefix") return(!is.na(code) & startsWith(code, from))
  n <- nchar(from)
  s <- substr(code, 1, n)
  !is.na(code) & nchar(code) >= n & s >= from & s <= to
}

#' Test which codes match an ICD-10 selection
#'
#' @param code Normalized ICD-10 codes (as in the `icd10` column).
#' @param spec A selection string, or the result of [parse_icd()].
#' @return Logical vector.
#' @export
#' @examples
#' icd_match(c("A150", "A17", "A199", "B20", "U071"), "A15-A19, !A17, U07")
icd_match <- function(code, spec) {
  if (!is.data.frame(spec)) spec <- parse_icd(spec)
  spec <- spec[is.na(spec$warning) | spec$warning == "no code in the ICD-10 reference matches", ]
  inc <- spec[!spec$exclude, ]; exc <- spec[spec$exclude, ]
  hit <- rep(FALSE, length(code))
  for (i in seq_len(nrow(inc))) hit <- hit | icd_match_one(code, inc$type[i], inc$from[i], inc$to[i])
  for (i in seq_len(nrow(exc))) hit <- hit & !icd_match_one(code, exc$type[i], exc$from[i], exc$to[i])
  hit
}

# Arrow-compatible filter expression for an ICD selection.
icd_filter_expr <- function(spec) {
  spec <- spec[is.na(spec$warning) | spec$warning == "no code in the ICD-10 reference matches", ]
  one <- function(type, from, to) {
    if (type == "prefix") return(rlang::expr(startsWith(icd10, !!from)))
    n <- nchar(from)
    rlang::expr(substr(icd10, 1L, !!n) >= !!from & substr(icd10, 1L, !!n) <= !!to &
                  nchar(icd10) >= !!n)
  }
  inc <- spec[!spec$exclude, ]; exc <- spec[spec$exclude, ]
  if (!nrow(inc)) cli::cli_abort("The ICD-10 selection has no valid inclusion item.")
  e <- Reduce(function(a, b) rlang::expr(!!a | !!b), Map(one, inc$type, inc$from, inc$to))
  e <- rlang::expr(!is.na(icd10) & (!!e))
  for (i in seq_len(nrow(exc))) {
    x <- one(exc$type[i], exc$from[i], exc$to[i])
    e <- rlang::expr(!!e & !(!!x))
  }
  e
}
