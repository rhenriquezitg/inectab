# Runs only when the INEC files are available:
#   Sys.setenv(INECTAB_DATA_DIR = "C:/Datasets/ecuador")
# Each file is harmonized and checked against the counts in verification.csv
# and values.csv, which were computed independently (Python) on the full data.
data_dir <- Sys.getenv("INECTAB_DATA_DIR")
verif <- readr::read_csv(cw_path("verification.csv"), show_col_types = FALSE, na = "")

for (ds in c("defunciones", "egresos")) {
  for (y in 2000:2024) {
    test_that(paste("harmonized data match the crosswalk counts:", ds, y), {
      skip_if(data_dir == "", "INECTAB_DATA_DIR not set")
      path <- tryCatch(inec_sav_path(data_dir, ds, y), error = function(e) NULL)
      skip_if(is.null(path), "file not available")
      h <- harmonize_year(ds, y, path = path)
      v <- verif[verif$dataset == ds & verif$year == y, ]
      expect_equal(nrow(h$data), v$n_records[1])
      expect_named(h$data, std_columns)
      expect_equal(sum(h$log$issue == "unmapped code"), 0)
      expect_equal(sum(h$log$issue == "ICD-10 code not in reference"),
                   v$n_unmapped_or_invalid[v$std_var == "icd10"])
      expect_equal(sum(h$log$issue == "age out of range for unit"), 0)
      for (cv in c("cant_res", "cant_occ")) {
        expect_equal(sum(h$log$std_var == cv), v$n_unmapped_or_invalid[v$std_var == cv])
      }
      # category counts equal the record counts written in values.csv
      vals <- read_crosswalk(ds, y)$values
      for (sv in unique(vals$std_var)) {
        got <- table(h$data[[sv]])
        sub <- vals[vals$std_var == sv & vals$std_code != "", ]
        exp <- tapply(as.integer(sub$n_records), sub$std_code, sum)
        exp <- exp[exp > 0]
        expect_equal(as.integer(got[names(exp)]), as.integer(exp), label = paste(sv, "counts"))
      }
      # age in years is consistent with value and unit
      yrs <- h$data$age_unit %in% "y"
      expect_equal(h$data$age_years[yrs], as.numeric(h$data$age_value[yrs]))
      expect_true(all(h$data$age_years < 1 | !h$data$age_unit %in% c("h", "d", "m"), na.rm = TRUE))
    })
  }
}
