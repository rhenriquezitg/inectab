test_that("cantons are built from 2-digit codes and special cases", {
  bc <- inectab:::build_cant
  expect_equal(bc(c("17", "9", "88", "90"), c("6", "17", "0", "2"), c("17", "09", "88", "90"),
                  "defunciones", 2005),
               c("1706", "0917", "8800", "9002"))
  expect_equal(bc("50", "01", "90", "egresos", 2002), "9000")
  expect_equal(bc("23", "2301", "23", "egresos", 2020), "2301")
  expect_equal(bc("17", NA, "17", "egresos", 2020), NA_character_)
})

test_that("geo schemes move Santo Domingo, Santa Elena and La Concordia", {
  d <- tibble::tibble(prov_res = c("17", "09", "90", "08", "23", "24", "23"),
                      cant_res = c("1706", "0917", "9002", "0808", "2301", "2401", NA),
                      prov_occ = NA_character_, cant_occ = NA_character_)
  now <- apply_geo_scheme(d, "dpa_2024")
  expect_equal(now$prov_res, c("23", "24", "23", "23", "23", "24", "23"))
  expect_equal(now$cant_res[1:4], c("2301", "2401", "2302", "2302"))
  old <- apply_geo_scheme(d, "dpa_pre2007")
  expect_equal(old$prov_res, c("17", "09", "90", "90", "17", "09", "17"))
  expect_identical(apply_geo_scheme(d, "asis"), d)
})
