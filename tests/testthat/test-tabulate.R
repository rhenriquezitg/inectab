test_that("parse_icd normalizes items and flags problems", {
  p <- parse_icd("A15-A19, U07.1, !A17, J, I21.0-I21.4, ZZ9, A19-A15, A15-I210")
  expect_equal(p$type[1:5], c("range", "prefix", "prefix", "prefix", "range"))
  expect_equal(p$from[2], "U071")
  expect_true(p$exclude[3])
  expect_true(all(is.na(p$warning[1:5])))
  expect_match(p$warning[6], "no code|not a valid")
  expect_match(p$warning[7], "start is after")
  expect_match(p$warning[8], "both")
})

test_that("icd_match applies prefixes, ranges and exclusions", {
  code <- c("A150", "A17", "A171", "A199", "B20", "U071", "I210", "I21", "I215", NA)
  expect_equal(icd_match(code, "A15-A19, !A17, U07, I21.0-I21.4"),
               c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(icd_match(c("A15", "A150"), "A15X"), c(TRUE, TRUE))
  expect_equal(icd_match(c("A15", "A150"), "A15.0"), c(FALSE, TRUE))
})

test_that("age bands and cutting", {
  b <- age_bands("5y_infant")
  expect_equal(b$label[1:3], c("<1", "1-4", "5-9"))
  expect_equal(tail(b$label, 1), "85+")
  expect_equal(age_bands("who")$label[21], "100+")
  expect_error(age_bands(c(5, 10)), "start at 0")
  x <- cut_age(c(0.002, 0.5, 1, 4.99, 85, 120, NA), b)
  expect_equal(as.character(x), c("<1", "<1", "1-4", "1-4", "85+", "85+", "Unknown"))
})

fake <- function() {
  tibble::tibble(
    dataset = "defunciones", file_year = c(2007L, 2008L, 2008L, 2009L, 2009L, 2009L),
    record_id = 1:6, event_year = c(2007L, 2008L, 2007L, 2009L, 2009L, 2009L),
    sex = c("1", "2", "1", "2", NA, "1"), age_value = c(30L, 2L, 70L, 5L, 40L, NA),
    age_unit = c("y", "m", "y", "d", "y", NA), age_years = c(30, 2 / 12, 70, 5 / 365.25, 40, NA),
    prov_res = c("17", "23", "09", "24", "17", NA), cant_res = c("1706", "2301", "0917", "2401", "1701", NA),
    prov_occ = c("17", "23", "09", "24", "17", "17"), cant_occ = c("1706", "2301", "0917", "2401", "1701", "1701"),
    discharge_cond = NA_character_, icd10 = c("A150", "A162", "J189", "A199", "A17", "A15"))
}

test_that("tabulate_icd counts by event and file year", {
  r <- tabulate_icd(fake(), "defunciones", "A15-A19", 2007:2009, age = "broad",
                    by = "year", complete = TRUE)
  expect_equal(r$table$n, c(1L, 1L, 3L))
  r <- tabulate_icd(fake(), "defunciones", "A15-A19", 2007:2009, year_basis = "file", by = "year")
  expect_equal(r$table$n, c(1L, 1L, 3L))
  r <- tabulate_icd(fake(), "defunciones", "A15-A19, !A17", 2009, year_basis = "file", by = "year")
  expect_equal(r$table$n, 2L)
})

test_that("tabulate_icd applies the boundary scheme and logs unknowns", {
  r <- tabulate_icd(fake(), "defunciones", "A15-A19", 2007:2009, by = "province", complete = FALSE)
  expect_equal(r$table$province, c("17", "23", "24", "NA"))
  expect_equal(r$table$n, c(1L, 2L, 1L, 1L))  # 1706 in 2007 moves to 23
  r2 <- tabulate_icd(fake(), "defunciones", "A15-A19", 2007:2009, by = "province",
                     geo_scheme = "dpa_pre2007", complete = FALSE)
  expect_equal(r2$table$province, c("09", "17", "NA"))
  expect_equal(r2$table$n, c(1L, 3L, 1L))
  expect_true(all(c("age", "sex", "province") %in% r$log$field[r$log$level == "record"]))
  expect_equal(sum(r$table$n), r$params$n_records)
})

test_that("complete grid has every year x band x sex x province", {
  r <- tabulate_icd(fake(), "defunciones", "A15-A19", 2007:2009, age = "broad",
                    by = c("year", "age_band", "sex", "province"))
  expect_equal(sum(r$table$n), 5L)
  expect_true(all(c("Hombre", "Mujer", "Unknown") %in% r$table$sex))
  expect_true("Unknown" %in% r$table$age_band)
  expect_equal(nrow(dplyr::distinct(r$table, year, age_band, sex, province)), nrow(r$table))
})

test_that("write_tabulation writes four tidy CSV files", {
  r <- tabulate_icd(fake(), "defunciones", "A15", 2007:2009, by = "year")
  p <- write_tabulation(r, tempfile())
  expect_true(all(file.exists(p)))
  expect_equal(nrow(readr::read_csv(p[1], show_col_types = FALSE)), 3)
})
