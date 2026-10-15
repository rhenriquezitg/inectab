# inectab

Harmonizes the INEC Ecuador SPSS files of defunciones generales and egresos hospitalarios (2000-2024) to one standard schema, caches them as Parquet, and counts records for any ICD-10 selection by year, age band, sex and province. Includes a Shiny app.

## Install

```r
install.packages(c("arrow", "haven", "dplyr", "readr", "stringr", "tibble", "cli", "rlang", "testthat",
                   "shiny", "bslib", "DT", "ggplot2", "tidyr", "zip", "markdown", "readxl"))
install.packages("path/to/inectab_0.3.2.tar.gz", repos = NULL, type = "source")
```

## Use

```r
library(inectab)
library(dplyr)

# One file, in memory
h <- harmonize_year("defunciones", 2024, data_dir = "C:/Datasets/ecuador")
h$data   # standard columns, see std_columns
h$log    # unmapped or invalid records

# All files to the cache (once; about 8 minutes for egresos, under 1 minute for defunciones)
build_cache("C:/Datasets/ecuador", "C:/Datasets/ecuador/cache")

# Query
open_cache("C:/Datasets/ecuador/cache") |>
  filter(dataset == "defunciones", event_year == 2024,
         substr(icd10, 1, 3) %in% c("A15", "A16", "A17", "A18", "A19")) |>
  collect() |>
  apply_geo_scheme("dpa_2024") |>
  count(prov_res)

read_cache_log("C:/Datasets/ecuador/cache")
cache_manifest("C:/Datasets/ecuador/cache")
```

`build_cache()` skips files already cached from the same source file (size and modification time) and package version.

## Tabulate

```r
r <- tabulate_icd("C:/Datasets/ecuador/cache", "defunciones",
                  icd = "A15-A19", years = 2015:2024,
                  year_basis = "event",        # year of death; or "file"
                  geography = "residence",     # or "occurrence"
                  geo_scheme = "dpa_2024",     # or "dpa_pre2007", "asis"
                  age = "5y",                  # preset or c(0, 1, 5, 15, 50, 65)
                  by = c("year", "age_band", "sex", "province"))
r$table    # tidy counts, zeros included
r$log      # selected records with unknown age/sex/province, harmonization issues, notes
r$codes    # ICD-10 codes found in the selection, with labels
write_tabulation(r, "C:/Datasets/ecuador/output", prefix = "tb_2015_2024")
```

ICD-10 selection (`parse_icd()`): `A15` or `A15X` = category with subcategories; `A15.0` / `A150` = that subcategory; `J`, `J1` = prefix; `A15-A19` = categories in range; `I21.0-I21.4` = subcategories in range; `!A17` = exclude.

Age presets (`age_presets()`): `5y`, `5y_infant`, `who` (0-4 ... 100+), `10y`, `broad`, `child`, or numeric lower bounds. Age is converted to years before banding.

Egresos only: `discharge = "dead"` or `"alive"`.

## Rates

```r
r <- tabulate_icd(cache, "defunciones", "I20-I25", 2015:2024,
                  by = c("year", "sex"), rates = TRUE, per = 1e5,
                  standard = "who")            # or "ecuador_2020"
r$table          # n, pop, crude rate with exact Poisson 95% limits per cell
r$standardized   # crude and directly age-standardized rates, Fay-Feuer 95% limits,
                 # by every breakdown except age band
```

Denominators: INEC estimaciones y proyecciones de poblacion, revision 2024, by province, sex and single age, 1990-2035, at 30 June (`inec_population()`; see `inst/extdata/population/README.md`). They follow current boundaries in every year: use `geo_scheme = "dpa_2024"` (or `"dpa_pre2007"`, which folds 23 and 24 into 17 and 09). Exterior, zonas no delimitadas, sex Indeterminado and unknown age, sex or province have no denominator; such records are counted but left out of rates, and the log says how many. Standard populations: `standard_population("who")` (WHO World Standard 2000-2025, 5-year weights spread over single ages) or `"ecuador_<year>"`.

To rebuild the denominators from a new INEC release: `import_population("Tabulado_provincial_edad_simple_....xlsx")`.

## App

```r
inectab::run_app("C:/Datasets/ecuador/cache")
```

Sidebar: dataset, ICD-10 selection (checked live against the reference), years, year basis, residence or occurrence, boundaries, age bands, breakdown, filters. Tabs: table (long or years as columns; CSV or ZIP download with table, log, codes and parameters), age-standardized rates, chart by year and sex (count, crude rate or age-standardized rate), codes found, log, cache manifest, notes. The app needs the CRAN builds of DT and bslib (the Debian/Ubuntu system packages of DT lack some JavaScript files).

## Standard columns

`dataset, file_year, record_id, event_year, event_month, reg_year, adm_year, adm_month, sex, age_value, age_unit, age_years, prov_res, cant_res, prov_occ, cant_occ, area_res, residente, place_occ, discharge_cond, los_days, sector, icd10, icd10_raw, icd10_valid`. Codes and definitions: `inec_schema()`.

Geography in the cache is as delivered by INEC, after code fixes (Exterior and zonas no delimitadas codes before 2004, 4-digit cantons). `apply_geo_scheme()` applies `"dpa_2024"` (current boundaries) or `"dpa_pre2007"` at query time.

## Crosswalks

`inst/extdata/crosswalks/`: one folder per dataset and year with `variables.csv`, `values.csv` and `changes.csv`, plus `semantic_changes.csv`, `geo_changes.csv` and `verification.csv`. See the README there.

## Tests

```r
testthat::test_local()                                   # crosswalk structure, ICD, geography
Sys.setenv(INECTAB_DATA_DIR = "C:/Datasets/ecuador")
testthat::test_local()                                   # also harmonizes every file found
```

The real-data tests compare each harmonized file with counts computed independently on the full data (`verification.csv`, `values.csv`): records, category counts per variable, invalid ICD-10 codes, canton issues.

## What the log reports

| issue | meaning |
|---|---|
| unmapped code | a code not in that year's `values.csv` (none in 2000-2024) |
| invalid value | year, month or length of stay not numeric or out of range (e.g. registration year 8888, 99, 1) |
| age out of range for unit | e.g. 40 months |
| age missing with known unit | |
| canton not in reference / canton outside its province | |
| ICD-10 code not in reference / ICD-10 code missing | |

## Online app

Live at <https://0wh4nr-rodrigo-henriquez.shinyapps.io/inectab/>. The app is deployed to shinyapps.io with `deploy/deploy_shinyapps.R`, which bundles the package code and a cache built with `build_cache()`. The cache is not stored in this repository.

## Data and license

Code: MIT. INEC data (defunciones generales, egresos hospitalarios, population projections) are published by INEC under CC BY 4.0; the crosswalks and population tables in `inst/extdata` derive from them. Cite INEC as the source of the data.
