# Entry point for shinyapps.io: sources the inectab package code and the
# harmonized cache shipped in this bundle (no package installation needed).
options(inectab.inst = normalizePath("pkg/inst"),
        inectab.version = read.dcf("pkg/DESCRIPTION", fields = "Version")[1, 1])
for (f in list.files("pkg/R", pattern = "[.]R$", full.names = TRUE)) source(f, local = globalenv())
.data <- rlang::.data
shiny::shinyOptions(inectab.cache_dir = normalizePath("cache"))

# Declare packages the app needs at runtime, so the deployment installs them.
if (FALSE) {
  library(arrow); library(bslib); library(cli); library(dplyr); library(DT); library(ggplot2)
  library(haven); library(markdown); library(readr); library(rlang); library(shiny)
  library(stringr); library(tibble); library(tidyr); library(zip)
}

source("pkg/inst/app/app.R", local = TRUE)$value
