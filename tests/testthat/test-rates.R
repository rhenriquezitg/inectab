test_that("population denominators are complete and add up to the national total", {
  p <- inec_population()
  expect_equal(nrow(p), 46 * 24 * 2 * 101)
  expect_setequal(unique(p$prov), sprintf("%02d", 1:24))
  expect_equal(sum(p$pop[p$year == 2024]), 17966573)
  expect_equal(sum(p$pop[p$year == 2000]), 12480574)
})

test_that("standard populations sum to one", {
  w <- standard_population("who")
  expect_equal(nrow(w), 101)
  expect_equal(sum(w$weight), 1)
  e <- standard_population("ecuador_2020")
  expect_equal(sum(e$weight), 1)
  expect_error(standard_population("xyz"), "Unknown standard")
})

test_that("exact Poisson limits", {
  ci <- inectab:::pois_ci(10, 1000, 1000)
  expect_equal(round(ci$lcl, 3), 4.795)
  expect_equal(round(ci$ucl, 3), 18.390)
  expect_equal(inectab:::pois_ci(0, 1000, 1000)$lcl, 0)
})

test_that("age standardization returns the common rate when age-specific rates are equal", {
  b <- age_bands("broad")
  w <- inectab:::band_weights(b, "who")
  cnt <- tibble::tibble(age_band = b$label, n = c(10L, 20L, 30L, 40L))
  pop <- tibble::tibble(age_band = b$label, pop = c(1000, 2000, 3000, 4000))
  s <- inectab:::standardize(cnt, pop, w, character(), 1000)
  expect_equal(s$asr, 10)
  expect_equal(s$crude_rate, 10)
  expect_true(s$asr_lcl < 10 && s$asr_ucl > 10)
})

test_that("tabulate_icd adds rates and standardized rates", {
  d <- tibble::tibble(
    dataset = "defunciones", file_year = 2020L, record_id = 1:4, event_year = 2020L,
    sex = c("1", "1", "2", "2"), age_value = c(30L, 70L, 70L, 1L), age_unit = c("y", "y", "y", "m"),
    age_years = c(30, 70, 70, 1 / 12), prov_res = c("01", "01", "17", "88"), cant_res = NA_character_,
    prov_occ = "01", cant_occ = NA_character_, discharge_cond = NA_character_, icd10 = "A150")
  r <- tabulate_icd(d, "defunciones", "A15", 2020, by = c("year", "sex", "province"), rates = TRUE)
  p <- inec_population()
  az_m <- sum(p$pop[p$year == 2020 & p$prov == "01" & p$sex == "1"])
  row <- r$table[r$table$sex == "Hombre" & r$table$province == "01", ]
  expect_equal(row$pop, az_m)
  expect_equal(row$rate, 2 / az_m * 1e5)
  expect_true(is.na(r$table$pop[r$table$province == "88"][1]))
  expect_false(is.null(r$standardized))
  expect_false("age_band" %in% names(r$standardized))
  expect_true(any(grepl("without a population denominator", r$log$issue)))
  # national row: the resident abroad (province 88) is counted but not in the rate numerator
  nat <- tabulate_icd(d, "defunciones", "A15", 2020, by = "year", rates = TRUE)
  expect_equal(nat$table$n, 4L)
  expect_equal(nat$table$n_rate, 3L)
  expect_equal(nat$table$pop, sum(p$pop[p$year == 2020]))
  expect_equal(nat$table$rate, 3 / sum(p$pop[p$year == 2020]) * 1e5)
  expect_equal(nat$standardized$n, 3L)
})
