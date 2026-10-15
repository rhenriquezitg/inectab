# Assemble and deploy the online app to shinyapps.io.
# The bundle holds the package code (sourced, not installed) and the harmonized
# cache built with inectab::build_cache().
#   Rscript deploy/deploy_shinyapps.R <cache_dir> [app_name]
# Account credentials: run rsconnect::setAccountInfo(name, token, secret) once.
args <- commandArgs(trailingOnly = TRUE)
cache_dir <- args[1]
app_name <- if (length(args) > 1) args[2] else "inectab"
stopifnot(file.exists(file.path(cache_dir, "manifest.csv")))
b <- file.path(tempdir(), "inectab-bundle")
unlink(b, recursive = TRUE); dir.create(file.path(b, "pkg"), recursive = TRUE)
file.copy(c("R", "inst", "DESCRIPTION", "LICENSE"), file.path(b, "pkg"), recursive = TRUE)
file.copy("deploy/app.R", b)
file.copy(cache_dir, b, recursive = TRUE)
file.rename(file.path(b, basename(cache_dir)), file.path(b, "cache"))
rsconnect::deployApp(b, appName = app_name, appTitle = "INEC ICD-10 tabulator (Ecuador)",
                     forceUpdate = TRUE, launch.browser = FALSE)
