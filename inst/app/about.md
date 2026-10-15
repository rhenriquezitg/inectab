### What this app does

It counts INEC records (defunciones generales or egresos hospitalarios, 2000-2024) for an ICD-10 selection, by year, age band, sex and province. The data come from the harmonized cache built with `inectab::build_cache()`.

### ICD-10 selection

- `A15` or `A15X`: the category and its subcategories.
- `A15.0` or `A150`: that subcategory only.
- `J`, `J1`: any code that starts with these characters.
- `A15-A19`: categories A15 to A19. `I21.0-I21.4`: subcategories only.
- `!A17`: exclude, applied after the inclusions.

The cause is the underlying cause of death (defunciones) or the main diagnosis at discharge (egresos).

### Year

- Year of the event: year of death or discharge. All cached files are pooled, because late registrations of a death sit in later files.
- Year of the INEC file: the publication year. For defunciones 2014+ this is the registration year.

Defunciones files up to 2013 hold only deaths registered in the same year. The latest year is incomplete: late registrations arrive in later files.

### Boundaries

Santo Domingo de los Tsachilas (23) and Santa Elena (24) exist from 2008 in the data. "Current" moves their cantons to 23 and 24 in earlier years and assigns La Concordia to 23 throughout. "Pre-2007" folds them back into Pichincha (17) and Guayas (09). 88 = Exterior, 90 = Zonas no delimitadas (not used from 2018).

### Age

Age is converted to years from hours, days, months or years before banding. Before 2013, ages above 98-99 are not recorded.

### Rates

Denominators are INEC's population estimates and projections (revision 2024), by province, sex and single year of age, at 30 June, 1990-2035. They follow current boundaries in every year, so rates use the "Current" or "Pre-2007" boundaries; Exterior and zonas no delimitadas have no denominator. Records with unknown age, sex or province are counted but left out of rates.

- Crude rate per cell, with exact Poisson 95% limits.
- Age-standardized rate (direct method) with Fay-Feuer 95% limits, by every breakdown except age band. Standards: WHO World Standard Population 2000-2025, or Ecuador's own population of 2020 or 2024.
- By place of occurrence, the denominator is still the resident population of the province.
- Egresos rates count discharges, not persons.

### Log

Every run lists selected records with unknown age, sex or province; codes that failed harmonization; and notes on data limits that apply to the selection.
