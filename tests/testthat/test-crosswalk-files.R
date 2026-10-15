# Structure of every crosswalk shipped with the package (no data needed).
schema <- inec_schema()

for (ds in c("defunciones", "egresos")) {
  for (y in 2000:2024) {
    test_that(paste("crosswalk files are complete:", ds, y), {
      cw <- read_crosswalk(ds, y)
      expect_named(cw$variables, c("raw_name", "raw_label", "spss_format", "n_value_labels",
                                   "std_var", "status", "transform"))
      expect_named(cw$values, c("std_var", "raw_name", "raw_code", "raw_label", "n_records",
                                "std_code", "std_label", "status", "note"))
      # every std var of this dataset is either fed by a raw variable or declared absent
      want <- schema$std_var[schema$datasets %in% c("both", ds) &
                               !schema$std_var %in% c("age_years", "icd10_raw")]
      expect_setequal(unique(cw$variables$std_var[cw$variables$std_var != ""]), want)
      # each std var has one source, except the cause built from two (causa + causa1)
      src <- table(cw$variables$std_var[cw$variables$std_var != "" & cw$variables$raw_name != ""])
      expect_true(all(src[names(src) != "icd10"] == 1))
      # no code observed in the full data is left unmapped
      expect_false(any(cw$values$status == "unmapped"))
      # standard codes come from the schema code lists
      prov <- cw$values$std_code[grepl("^prov_", cw$values$std_var) & cw$values$status == "mapped"]
      expect_true(all(prov %in% c(sprintf("%02d", 1:24), "88", "90")))
      if (y <= 2007) expect_false(any(prov %in% c("23", "24")))
      au <- cw$values$std_code[cw$values$std_var == "age_unit" & cw$values$status == "mapped"]
      expect_true(all(au %in% c("h", "d", "m", "y")))
    })
  }
}

test_that("egresos age unit codes change meaning in 2015", {
  v14 <- read_crosswalk("egresos", 2014)$values
  v15 <- read_crosswalk("egresos", 2015)$values
  u14 <- v14[v14$std_var == "age_unit", ]
  u15 <- v15[v15$std_var == "age_unit", ]
  expect_equal(u14$std_code[u14$raw_code == "1"], "d")
  expect_equal(u14$std_code[u14$raw_code == "3"], "y")
  expect_equal(u15$std_code[u15$raw_code == "1"], "h")
  expect_equal(u15$std_code[u15$raw_code == "4"], "y")
})

test_that("Exterior and zonas no delimitadas codes are recoded in early years", {
  v <- read_crosswalk("defunciones", 2003)$values
  expect_equal(unique(v$std_code[v$std_var == "prov_res" & v$raw_code == "90"]), "88")
  v <- read_crosswalk("egresos", 2002)$values
  expect_equal(unique(v$std_code[v$std_var == "prov_res" & v$raw_code == "50"]), "90")
  expect_equal(unique(v$std_code[v$std_var == "prov_res" & v$raw_code == "90"]), "88")
  v <- read_crosswalk("egresos", 2010)$values
  expect_equal(unique(v$std_code[v$std_var == "prov_res" & v$raw_code == "90"]), "90")
})
