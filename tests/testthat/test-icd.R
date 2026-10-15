test_that("normalize_icd handles dots, case, X filler and a 5th character", {
  expect_equal(normalize_icd(c("A15.0", "a150", "A15X", "A15", "S5290", " i10 ", "", NA)),
               c("A150", "A150", "A15", "A15", "S529", "I10", NA, NA))
})

test_that("codcausa decodes to 3- and 4-character codes", {
  expect_equal(inectab:::decode_codcausa(c(10003, 92193, 91002, 185402, 258993, NA)),
               c("A000", "I219", "I10", "R54", "Y899", NA))
})

test_that("reference contains COVID-19 codes", {
  expect_true(all(c("U071", "U072", "U07") %in% icd10_reference()$code))
})
