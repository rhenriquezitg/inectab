#' Age band presets
#'
#' @return Named list of lower bounds (years). The last band is open.
#' \describe{
#'   \item{`5y`}{0-4, 5-9, ..., 80-84, 85+}
#'   \item{`5y_infant`}{<1, 1-4, 5-9, ..., 80-84, 85+}
#'   \item{`who`}{WHO World Standard Population groups: 0-4, ..., 95-99, 100+}
#'   \item{`10y`}{0-9, 10-19, ..., 70-79, 80+}
#'   \item{`broad`}{0-14, 15-49, 50-64, 65+}
#'   \item{`child`}{<1, 1-4, 5-14, 15+}
#' }
#' @export
age_presets <- function() {
  list(
    `5y` = seq(0, 85, 5),
    `5y_infant` = c(0, 1, seq(5, 85, 5)),
    who = seq(0, 100, 5),
    `10y` = seq(0, 80, 10),
    broad = c(0, 15, 50, 65),
    child = c(0, 1, 5, 15)
  )
}

#' Build age bands
#'
#' @param breaks A preset name from [age_presets()] or a numeric vector of
#'   lower bounds in years, starting at 0. The last band is open (`85+`).
#' @return A tibble with `lower`, `upper` (exclusive, `Inf` for the last
#'   band) and `label`.
#' @export
#' @examples
#' age_bands("5y_infant")
#' age_bands(c(0, 1, 5, 15, 50, 65))
age_bands <- function(breaks = "5y") {
  if (is.character(breaks)) {
    p <- age_presets()
    if (!breaks %in% names(p)) cli::cli_abort("Unknown age preset {.val {breaks}}. Use one of {.val {names(p)}}.")
    breaks <- p[[breaks]]
  }
  breaks <- sort(unique(as.numeric(breaks)))
  if (!length(breaks) || breaks[1] != 0) cli::cli_abort("Age breaks must start at 0.")
  upper <- c(breaks[-1], Inf)
  label <- ifelse(is.infinite(upper), paste0(breaks, "+"),
                  ifelse(breaks == 0 & upper == 1, "<1",
                         ifelse(upper - breaks == 1, as.character(breaks),
                                paste0(breaks, "-", upper - 1))))
  tibble::tibble(lower = breaks, upper = upper, label = label)
}

#' Assign age in years to bands
#' @param age_years Numeric ages in years (fractions allowed).
#' @param bands Output of [age_bands()].
#' @return Factor with the band labels plus `"Unknown"` for missing ages.
#' @export
cut_age <- function(age_years, bands) {
  i <- findInterval(age_years, bands$lower)
  lab <- ifelse(is.na(age_years), "Unknown", bands$label[pmax(i, 1)])
  factor(lab, levels = c(bands$label, "Unknown"))
}
