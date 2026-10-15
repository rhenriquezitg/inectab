# INEC crosswalks: defunciones generales and egresos hospitalarios, 2000-2024

Built from the 50 SPSS files in `C:\Datasets\ecuador\defunciones` (DEF2000-DEF2024.sav) and `C:\Datasets\ecuador\egresos` (EGR2000-EGR2024.sav). Every mapping was checked against the full data, not only the codebooks.

## Files

| File | Content |
|---|---|
| `schema.csv` | The standard schema: 21 harmonized variables and their codes. |
| `variable_comparison_<ds>.csv` | Every raw variable (case-insensitive) by year. Cell = raw name as spelled that year. Columns `std_var`, first/last year, latest label. |
| `std_source_matrix_<ds>.csv` | Each standard variable by year: which raw variable feeds it. Blank = absent that year. |
| `<ds>/<year>/variables.csv` | All raw variables of that file: label, SPSS format, number of value labels, target `std_var`, transform rule. Standard variables absent that year are listed with status `absent this year -> NA`. |
| `<ds>/<year>/values.csv` | Code-level crosswalk for coded variables. One row per raw code that is labelled or observed, with record count, standard code and note. |
| `<ds>/<year>/changes.csv` | Automatic diff against the previous year: variables added/removed/renamed, codes added/removed, label changes. |
| `semantic_changes.csv` | Changes in meaning that a diff cannot see (same code, new meaning; scope of the file). Read this first. |
| `all_changes.csv` | All `changes.csv` stacked. |
| `geo_changes.csv` | Canton reassignments for two schemes: `dpa_2024` (current DPA) and `dpa_pre2007` (fold 23 and 24 back). |
| `reference/cantons.csv` | 4-digit canton reference (INEC labels 2011-2024 plus pre-2008 codes). |
| `reference/icd10_inec.csv` | ICD-10 codes and labels from INEC value labels (used to validate codes). |
| `verification.csv` | Per year and variable: records mapped, set to NA, unmapped or invalid, with examples. |
| `check_<ds>_<prov>_dpa2024.csv` | Province counts by year after applying `dpa_2024`. Continuity check. |

Raw codes in `values.csv` are written as R reads them with `haven::read_sav()` and `as.character()`: numeric 1 is `"1"`, text `"01"` stays `"01"`. Join on that.

## Standard variables

event_year, event_month, reg_year (def), adm_year/adm_month (egr), sex, age_value, age_unit, age_years, prov_res, cant_res, prov_occ, cant_occ, area_res, residente (def), place_occ (def), discharge_cond (egr), los_days (egr), sector (egr), icd10, icd10_raw.

Not harmonized and carried raw: education, ethnicity, marital status, maternal-death fields, cause lists (103, 80, 67, 298, 221), establishment type/class/entity, nationality, specialty, insurance.

## Rules that matter for tabulation

Year
- Defunciones 2000-2013 hold only deaths that occurred and were registered in the file year. From 2014 files are by registration year and include late registrations (1-3% of records). Deaths of 2013 registered late appear in the 2014+ files; deaths of 2012 or earlier registered in 2013 are in none of these files.
- Egresos have no discharge-year variable before 2013. The statistical year is the discharge year; every record of every file has event_year = file year.

Age
- Defunciones unit: 1 hours, 2 days, 3 months, 4 years, 9 unknown, all years. Missing age: 99 (2000-2012), 999 (2013+), always with unit 9.
- Egresos unit changes meaning in 2015. 2000-2014: 1 days, 2 months, 3 years, 9 unknown. 2015+: 1 hours, 2 days, 3 months, 4 years, 9 unknown. Never pool raw codes.
- Top-coding: before 2013 ages above 98-99 are not recorded (defunciones heap at 99; egresos top at 99 in 2000-2003 and 98 in 2004-2012). For pooled series, the open upper band should start at 95 or lower.

Province
- Exterior: code 90 in defunciones 2000-2003 and egresos 2000-2003; 88 afterwards. Confirmed by labels and by canton 0.
- Zonas no delimitadas: code 50 in egresos 2001-2003; 90 from 2004. Not coded separately in defunciones 2000-2003 or egresos 2000. Not used from 2018.
- Santo Domingo (23) and Santa Elena (24) appear in 2008. For 2000-2007, `geo_changes.csv` moves cantons 1706, 0917, 0926, 0915 to them. This works for defunciones (residence and place of death) and egresos residence. Egresos establishment canton starts in 2002, so establishment province 2000-2001 stays in 17 and 09.
- La Concordia moves twice: zona no delimitada (9002) to 2007, Esmeraldas (0808) 2008-2013, Santo Domingo (2302) from 2014. It shifts about 200 deaths and 1,500-2,500 discharges a year between provinces 08, 23 and 90. `dpa_2024` assigns it to 23 throughout.
- Defunciones: canton of residence ends in 2014. From 2015 residence is province only, so residence-based harmonization for 2015+ relies on the province code, which already follows the current DPA.

Cause
- Defunciones 2000-2002: causa + causa1 ('X' or 0 for 3-character categories). 2003-2007: decoded from codcausa. 2008+: causa4. All three sources agree where they overlap.
- Egresos 2017+: some diagnoses carry a 5th character (S and M chapters). icd10 keeps 4.
- 3-character codes such as A09, K85, I48, C80 are valid for the ICD-10 edition in use at the time; later updates subdivided them. Prefix matching in the tabulator handles both.

## Verification summary

Every observed code of sex, age unit, province, area, residente, place of death, discharge condition and sector maps in all 50 files (0 unmapped). Cantons: 2 records in defunciones 2011 have a canton outside their province. ICD-10: 25 invalid codes in defunciones (23 of them in 2014) and 6 in egresos 2000-2003, out of 1.74 million deaths and 25.1 million discharges. Ages are within range for every unit.
