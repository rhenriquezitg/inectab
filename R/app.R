#' Launch the Shiny tabulator
#'
#' @param cache_dir Cache folder built with [build_cache()]. Defaults to the
#'   `INECTAB_CACHE_DIR` environment variable.
#' @param ... Passed to [shiny::runApp()], e.g. `launch.browser`, `port`.
#' @return Runs the app.
#' @export
run_app <- function(cache_dir = Sys.getenv("INECTAB_CACHE_DIR"), ...) {
  need <- c("shiny", "bslib", "DT", "ggplot2", "tidyr", "zip", "markdown")
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) cli::cli_abort("Install {.pkg {miss}} to run the app.")
  app_dir <- inectab_file("app")
  shiny::shinyOptions(inectab.cache_dir = cache_dir)
  shiny::runApp(app_dir, ...)
}
