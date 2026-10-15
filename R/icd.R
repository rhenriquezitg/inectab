#' Normalize ICD-10 codes
#'
#' Upper case, no dot or spaces, no `X` filler on 3-character codes, and a
#' national 5th character (egresos 2017+, e.g. `S5290`) dropped.
#'
#' @param x Character vector of codes such as `"A15.0"`, `"a150"`, `"A15X"`.
#' @return Character vector of 3- or 4-character codes; `NA` stays `NA`.
#' @export
#' @examples
#' normalize_icd(c("A15.0", "a150", "A15X", "S5290", "I10"))
normalize_icd <- function(x) {
  x <- toupper(gsub("[[:space:].]", "", as.character(x)))
  x[x == ""] <- NA_character_
  x <- sub("^([A-Z][0-9]{2})X$", "\\1", x)
  long <- !is.na(x) & grepl("^[A-Z][0-9]{4}$", x)
  x[long] <- substr(x[long], 1, 4)
  x
}

# Decode INEC numeric cause code (defunciones 2003-2007).
# n = letter*10000 + category*100 + 4th digit*10 + flag; flag 2 = 3-char code, 3 = 4-char code.
decode_codcausa <- function(n) {
  n <- as.numeric(n)
  flag <- n %% 10
  core <- n %/% 10
  letter <- core %/% 1000
  ok <- !is.na(n) & letter >= 1 & letter <= 26 & flag %in% c(2, 3)
  out <- rep(NA_character_, length(n))
  out[ok] <- paste0(LETTERS[letter[ok]], sprintf("%02d", (core[ok] %/% 10) %% 100),
                    ifelse(flag[ok] == 3, as.character(core[ok] %% 10), ""))
  out
}

# Build the raw cause code for one file following the per-year rule in
# variables.csv (column `transform` of std_var icd10).
build_icd_raw <- function(df, dataset, year, src) {
  if (dataset == "defunciones" && year <= 2002) {
    c3 <- raw_chr(df[[src[1]]])
    c1 <- raw_chr(df[[src[2]]])
    c1[!is.na(c1) & c1 == "X"] <- NA
    out <- ifelse(is.na(c1), c3, paste0(c3, c1))
    if (year == 2002) {
      ref <- icd10_reference()$code
      has_sub <- unique(substr(ref[nchar(ref) == 4], 1, 3))
      drop <- !is.na(c1) & c1 == "0" & !c3 %in% has_sub
      out[drop] <- c3[drop]
    }
    return(out)
  }
  if (dataset == "defunciones" && year <= 2007) {
    return(decode_codcausa(raw_chr(df[[src[1]]])))
  }
  raw_chr(df[[src[1]]])
}
