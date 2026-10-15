# Population denominators

`population_province_single_age.csv`: INEC, Estimaciones y proyecciones de poblacion, revision 2024.
Source file: `Tabulado_provincial_edad_simple_1990-2035_rev2024.xlsx` (provincial, single age, by sex; values at 30 June).
Built with `inectab::import_population()`, which checks that single ages add up to each sheet's total and that men + women equal both sexes.

Columns: `year` (1990-2035), `prov` (01-24, current boundaries in every year), `sex` (1 men, 2 women), `age` (0-100; 100 = 100 and over), `pop`.
Province totals add up exactly to the national file `Tabulado_nacional_1950-2050_rev2024.xlsx` (e.g. 2000: 12,480,574; 2024: 17,966,573).

Not used yet: the cantonal file (`Tabulado_cantonal_2010-2035_rev2024.xlsx`, totals only, 2010-2035, canton names without codes).
