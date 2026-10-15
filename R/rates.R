#' Standard populations for direct age standardization
#'
#' @param standard `"who"`: WHO World Standard Population 2000-2025 (Ahmad et
#'   al., 2001), 5-year groups to 100+, spread evenly over single ages within
#'   each group. `"ecuador_<year>"` (e.g. `"ecuador_2020"`): Ecuador's
#'   population of that year by single age, both sexes, from
#'   [inec_population()].
#' @return Tibble with `age` (0-100, 100 = 100+) and `weight` (sums to 1).
#' @export
standard_population <- function(standard = "who") {
  if (standard == "who") {
    w5 <- c(8860, 8690, 8600, 8470, 8220, 7930, 7610, 7150, 6590, 6040, 5370, 4550, 3720,
            2960, 2210, 1520, 910, 440, 150, 40, 5)
    w <- c(rep(w5[1:20] / 5, each = 5), w5[21])
    return(tibble::tibble(age = 0:100, weight = w / sum(w)))
  }
  if (grepl("^ecuador_[0-9]{4}$", standard)) {
    y <- as.integer(sub("ecuador_", "", standard))
    p <- inec_population()
    p <- p[p$year == y, ]
    if (!nrow(p)) cli::cli_abort("No population for year {y}.")
    w <- tapply(p$pop, p$age, sum)
    return(tibble::tibble(age = as.integer(names(w)), weight = as.numeric(w) / sum(w)))
  }
  cli::cli_abort("Unknown standard {.val {standard}}. Use \"who\" or \"ecuador_<year>\".")
}

# Exact Poisson confidence limits for a rate n / pop.
pois_ci <- function(n, pop, per, level = 0.95) {
  a <- (1 - level) / 2
  lo <- ifelse(n == 0, 0, stats::qchisq(a, 2 * n) / 2)
  hi <- stats::qchisq(1 - a, 2 * (n + 1)) / 2
  list(lcl = lo / pop * per, ucl = hi / pop * per)
}

# Provinces with a denominator under a boundary scheme.
pop_tab_provinces <- function(pop, geo_scheme) {
  p <- unique(pop$prov)
  if (geo_scheme == "dpa_pre2007") p <- setdiff(p, c("23", "24"))
  p
}

# Rate numerators in the same shape as the count table.
rate_counts <- function(d, grp) {
  cnt <- dplyr::count(d, dplyr::across(dplyr::all_of(grp)), name = "n_rate")
  if ("sex_lab" %in% names(cnt)) {
    cnt$sex_lab <- as.character(cnt$sex_lab)
    names(cnt)[names(cnt) == "sex_lab"] <- "sex"
  }
  if ("age_band" %in% names(cnt)) cnt$age_band <- as.character(cnt$age_band)
  cnt
}

# Join numerators and denominators to a count table and compute crude rates.
add_rates <- function(tab, num, pop_tab, by, per) {
  keys <- intersect(c("year", "age_band", "sex", "province"), by)
  out <- if (length(keys)) dplyr::left_join(tab, num, by = keys) else dplyr::bind_cols(tab, num)
  out$n_rate[is.na(out$n_rate)] <- 0L
  out <- if (length(keys)) dplyr::left_join(out, pop_tab, by = keys) else dplyr::bind_cols(out, pop_tab)
  out$rate <- out$n_rate / out$pop * per
  ci <- pois_ci(out$n_rate, out$pop, per)
  out$rate_lcl <- ci$lcl
  out$rate_ucl <- ci$ucl
  out
}

# Direct age standardization with Fay-Feuer gamma intervals.
# cnt: counts with columns `keys`, age_band, n (complete grid, no Unknown band)
# pop_tab: population with the same keys + age_band, pop
# w: tibble age_band, weight (sums to 1)
standardize <- function(cnt, pop_tab, w, keys, per, level = 0.95) {
  d <- dplyr::left_join(cnt, pop_tab, by = c(keys, "age_band"))
  d <- dplyr::left_join(d, w, by = "age_band")
  a <- (1 - level) / 2
  g <- dplyr::group_by(d, dplyr::across(dplyr::all_of(keys)))
  dplyr::summarise(g,
    asr = sum(.data$weight * .data$n / .data$pop) * per,
    var = sum((.data$weight / .data$pop)^2 * .data$n) * per^2,
    wm = max(.data$weight / .data$pop) * per,
    crude_rate = sum(.data$n) / sum(.data$pop) * per,
    pop_total = sum(.data$pop),
    n_total = sum(.data$n),
    .groups = "drop") |>
    dplyr::mutate(
      asr_lcl = ifelse(.data$var > 0, stats::qgamma(a, shape = .data$asr^2 / .data$var,
                                                   scale = .data$var / .data$asr), 0),
      asr_ucl = stats::qgamma(1 - a, shape = (.data$asr + .data$wm)^2 / (.data$var + .data$wm^2),
                              scale = (.data$var + .data$wm^2) / (.data$asr + .data$wm))
    ) |>
    dplyr::mutate(asr_se = sqrt(.data$var)) |>
    dplyr::select(-"var", -"wm") |>
    dplyr::rename(n = "n_total", pop = "pop_total") |>
    dplyr::relocate("n", "pop", "crude_rate", .before = "asr")
}

band_weights <- function(bands, standard) {
  s <- standard_population(standard)
  s$age_band <- as.character(cut_age(s$age, bands))
  w <- tapply(s$weight, s$age_band, sum)
  tibble::tibble(age_band = names(w), weight = as.numeric(w))
}
