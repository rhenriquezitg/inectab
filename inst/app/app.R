# INEC ICD-10 tabulator: Shiny app. Launch with inectab::run_app().
library(shiny)
library(bslib)
if (!exists("tabulate_icd", mode = "function")) library(inectab)

default_cache <- getShinyOption("inectab.cache_dir", Sys.getenv("INECTAB_CACHE_DIR", ""))

sex_choices <- c(Hombre = "1", Mujer = "2", Indeterminado = "3")
prov_choices <- local({
  p <- c(sprintf("%02d", 1:24), "88", "90")
  stats::setNames(p, paste(p, province_label(p)))
})
age_choices <- c("5-year (0-4 ... 85+)" = "5y", "5-year with <1 and 1-4" = "5y_infant",
                 "WHO standard (0-4 ... 100+)" = "who", "10-year (0-9 ... 80+)" = "10y",
                 "Broad (0-14, 15-49, 50-64, 65+)" = "broad", "Child (<1, 1-4, 5-14, 15+)" = "child",
                 "Custom" = "custom")

ui <- page_sidebar(
  title = "INEC ICD-10 tabulator",
  theme = bs_theme(version = 5, bootswatch = "flatly", base_font = font_collection("system-ui", "sans-serif")),
  sidebar = sidebar(
    width = 340,
    textInput("cache_dir", "Cache folder", value = default_cache, placeholder = "C:/Datasets/ecuador/cache"),
    radioButtons("dataset", "Dataset", c("Defunciones generales" = "defunciones",
                                         "Egresos hospitalarios" = "egresos"), inline = FALSE),
    textAreaInput("icd", "ICD-10 codes", value = "A15-A19", rows = 2,
                  placeholder = "A15-A19, U07.1, J, !A17"),
    uiOutput("icd_check"),
    sliderInput("years", "Years", min = 2000, max = 2024, value = c(2015, 2024), step = 1, sep = ""),
    radioButtons("year_basis", "Year", c("Year of the event (pooled files)" = "event",
                                         "Year of the INEC file" = "file")),
    radioButtons("geography", "Province", c("Residence" = "residence", "Occurrence" = "occurrence"),
                 inline = TRUE),
    selectInput("geo_scheme", "Boundaries", c("Current (DPA 2024)" = "dpa_2024",
                                              "Pre-2007 (no 23, 24)" = "dpa_pre2007",
                                              "As delivered by INEC" = "asis")),
    selectInput("age_preset", "Age bands", age_choices),
    conditionalPanel("input.age_preset == 'custom'",
                     textInput("age_custom", "Lower bounds (years)", "0, 1, 5, 15, 50, 65")),
    checkboxGroupInput("by", "Break down by", c(Year = "year", "Age band" = "age_band", Sex = "sex",
                                                 Province = "province"),
                       selected = c("year", "age_band", "sex", "province"), inline = TRUE),
    checkboxInput("do_rates", "Rates (population denominators)", FALSE),
    conditionalPanel("input.do_rates",
      selectInput("per", "Per", c("100,000" = 1e5, "10,000" = 1e4, "1,000" = 1e3), selected = 1e5),
      selectInput("standard", "Standard population", c("WHO World Standard 2000-2025" = "who",
                                                       "Ecuador 2020" = "ecuador_2020",
                                                       "Ecuador 2024" = "ecuador_2024"))),
    accordion(
      open = FALSE,
      accordion_panel(
        "Filters",
        checkboxGroupInput("sex", "Sex", sex_choices, selected = c("1", "2", "3"), inline = TRUE),
        selectizeInput("provinces", "Provinces (empty = all)", prov_choices, multiple = TRUE),
        conditionalPanel("input.dataset == 'egresos'",
                         radioButtons("discharge", "Discharge condition",
                                      c(All = "all", Alive = "alive", Dead = "dead"), inline = TRUE)),
        checkboxInput("complete", "Show zero counts", TRUE)
      )
    ),
    actionButton("run", "Tabulate", class = "btn-primary", icon = icon("table"))
  ),
  layout_columns(
    fill = FALSE,
    value_box("Records selected", textOutput("vb_n"), theme = "primary", max_height = "110px"),
    value_box(textOutput("vb_mid_title", inline = TRUE), textOutput("vb_rows"), theme = "secondary", max_height = "110px"),
    value_box("Log entries", textOutput("vb_log"), theme = "secondary", max_height = "110px")
  ),
  navset_card_tab(
    full_screen = TRUE,
    nav_panel("Table",
              div(class = "d-flex gap-2 mb-2 flex-wrap",
                  checkboxInput("wide", "Years as columns", FALSE),
                  downloadButton("dl_table", "Table (CSV)", class = "btn-sm"),
                  downloadButton("dl_all", "Table, log, codes, parameters (ZIP)", class = "btn-sm")),
              DT::DTOutput("table")),
    nav_panel("Age-standardized",
              p(class = "text-muted small",
                "Crude and directly age-standardized rates with 95% limits, by every breakdown except age band. Needs Rates."),
              downloadButton("dl_std", "Standardized rates (CSV)", class = "btn-sm mb-2"),
              DT::DTOutput("std")),
    nav_panel("Chart",
              radioButtons("chart_measure", NULL, c("Count" = "n", "Crude rate" = "rate",
                                                    "Age-standardized rate" = "asr"), inline = TRUE),
              plotOutput("chart", height = "480px")),
    nav_panel("Codes found", p(class = "text-muted small",
                               "ICD-10 codes present in the selected records. Check that the selection catches what you intended."),
              DT::DTOutput("codes")),
    nav_panel("Log", p(class = "text-muted small",
                       "Records with unknown fields, invalid codes from harmonization and notes on data limits."),
              downloadButton("dl_log", "Log (CSV)", class = "btn-sm mb-2"),
              DT::DTOutput("log_table")),
    nav_panel("Cache", DT::DTOutput("manifest")),
    nav_panel("About", includeMarkdown(inectab_file("app", "about.md")))
  )
)

server <- function(input, output, session) {
  cache_ok <- reactive({
    d <- input$cache_dir
    nzchar(d) && file.exists(file.path(d, "manifest.csv"))
  })
  manifest <- reactive({
    req(cache_ok())
    cache_manifest(input$cache_dir)
  })

  observeEvent(list(manifest(), input$dataset), {
    m <- manifest()
    yrs <- m$file_year[m$dataset == input$dataset]
    if (length(yrs)) {
      updateSliderInput(session, "years", min = min(yrs), max = max(yrs),
                        value = c(max(min(yrs), max(yrs) - 9), max(yrs)))
    }
  })

  spec <- reactive({
    tryCatch(parse_icd(input$icd), error = function(e) NULL)
  })
  output$icd_check <- renderUI({
    s <- spec()
    if (is.null(s)) return(div(class = "text-danger small mb-2", "Enter at least one code."))
    ok <- is.na(s$warning)
    tags$ul(class = "small mb-2 ps-3",
            lapply(seq_len(nrow(s)), function(i) {
              txt <- paste0(s$item[i], if (s$exclude[i]) " (exclude)" else "", ": ",
                            if (ok[i]) paste(s$n_ref[i], "reference codes") else s$warning[i])
              tags$li(class = if (ok[i]) "text-muted" else "text-danger", txt)
            }))
  })

  age_arg <- reactive({
    if (input$age_preset != "custom") return(input$age_preset)
    b <- suppressWarnings(as.numeric(strsplit(input$age_custom, "[,; ]+")[[1]]))
    validate(need(length(b) && !anyNA(b) && min(b) == 0, "Custom age bands: numbers starting at 0."))
    b
  })

  result <- eventReactive(input$run, {
    validate(need(cache_ok(), "Cache folder not found. Build it with inectab::build_cache()."))
    validate(need(length(input$by) > 0, "Choose at least one breakdown."))
    s <- spec()
    validate(need(!is.null(s) && any(is.na(s$warning) & !s$exclude), "No valid ICD-10 code in the selection."))
    withProgress(message = "Tabulating", value = 0.3, {
      tabulate_icd(
        input$cache_dir, input$dataset, input$icd, seq(input$years[1], input$years[2]),
        year_basis = input$year_basis, geography = input$geography, geo_scheme = input$geo_scheme,
        age = age_arg(), by = input$by,
        sex = if (length(input$sex) < 3) input$sex else NULL,
        provinces = if (length(input$provinces)) input$provinces else NULL,
        discharge = if (input$dataset == "egresos") input$discharge else "all",
        complete = input$complete,
        rates = isTRUE(input$do_rates), per = as.numeric(input$per), standard = input$standard
      )
    })
  })

  shown_table <- reactive({
    t <- result()$table
    if (isTRUE(input$wide) && "year" %in% names(t)) {
      if ("rate" %in% names(t)) {
        t <- tidyr::pivot_wider(dplyr::select(t, -"n_rate", -"pop", -"rate_lcl", -"rate_ucl"),
                                names_from = "year", values_from = c("n", "rate"))
      } else {
        t <- tidyr::pivot_wider(t, names_from = "year", values_from = "n", values_fill = 0L)
      }
    }
    t
  })

  fmt <- function(x) format(x, big.mark = ",")
  output$vb_n <- renderText(fmt(result()$params$n_records))
  output$vb_mid_title <- renderText({
    if (is.null(result()$standardized)) "Table rows"
    else paste0("Crude rate per ", format(result()$params$per, big.mark = ",", scientific = FALSE), ", period")
  })
  output$vb_rows <- renderText({
    r <- result()
    if (is.null(r$standardized)) return(fmt(nrow(r$table)))
    t <- r$table[!is.na(r$table$pop), ]
    formatC(sum(t$n_rate) / sum(t$pop) * r$params$per, format = "f", digits = 2)
  })
  output$vb_log <- renderText(fmt(nrow(result()$log)))

  output$table <- DT::renderDT({
    t <- shown_table()
    dt <- DT::datatable(t, rownames = FALSE, filter = "top", options = list(pageLength = 25, scrollX = TRUE))
    rc <- intersect(c("rate", "rate_lcl", "rate_ucl"), names(t))
    if (length(rc)) dt <- DT::formatRound(dt, rc, 2)
    dt
  })
  output$std <- DT::renderDT({
    s <- result()$standardized
    validate(need(!is.null(s), "Tick Rates and tabulate again."))
    DT::formatRound(DT::datatable(s, rownames = FALSE, filter = "top", options = list(pageLength = 25, scrollX = TRUE)),
                    c("crude_rate", "asr", "asr_lcl", "asr_ucl", "asr_se"), 3)
  })
  output$codes <- DT::renderDT(DT::datatable(result()$codes, rownames = FALSE,
                                             options = list(pageLength = 25)))
  output$log_table <- DT::renderDT(DT::datatable(result()$log, rownames = FALSE, filter = "top",
                                           options = list(pageLength = 25, scrollX = TRUE)))
  output$manifest <- DT::renderDT(DT::datatable(manifest(), rownames = FALSE,
                                                options = list(pageLength = 50, scrollX = TRUE)))

  output$chart <- renderPlot({
    r <- result()
    m <- input$chart_measure
    validate(need(m == "n" || !is.null(r$standardized), "Tick Rates and tabulate again to chart rates."))
    if (m == "asr") {
      agg <- r$standardized
      validate(need("year" %in% names(agg), "The chart needs Year in the breakdown."))
      col <- if ("sex" %in% names(agg)) "sex" else NULL
      if ("province" %in% names(agg)) {
        validate(need(length(unique(agg$province)) == 1,
                      "Age-standardized rates by province: filter one province, or remove Province from the breakdown."))
      }
      agg$value <- agg$asr
      ylab <- paste("Age-standardized rate per", format(r$params$per, big.mark = ",", scientific = FALSE))
    } else {
      t <- r$table
      validate(need("year" %in% names(t), "The chart needs Year in the breakdown."))
      col <- if ("sex" %in% names(t)) "sex" else NULL
      if (m == "rate") t <- t[!is.na(t$pop), ]
      agg <- dplyr::summarise(dplyr::group_by(t, dplyr::across(dplyr::all_of(c("year", col)))),
                              value = if (m == "n") sum(.data$n) else sum(.data$n_rate) / sum(.data$pop) * r$params$per,
                              .groups = "drop")
      ylab <- if (m == "n") "Records" else paste("Crude rate per", format(r$params$per, big.mark = ",", scientific = FALSE))
    }
    if (!is.null(col)) agg <- agg[agg$sex %in% c("Hombre", "Mujer", "Indeterminado"), ]
    p <- ggplot2::ggplot(agg, ggplot2::aes(.data$year, .data$value)) +
      ggplot2::labs(x = NULL, y = ylab, title = paste(input$dataset, "-", input$icd)) +
      ggplot2::scale_x_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2]), by = max(1, floor(diff(l) / 10)))) +
      ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ","), limits = c(0, NA)) +
      ggplot2::theme_minimal(base_size = 14) + ggplot2::theme(legend.position = "top")
    if (is.null(col)) p + ggplot2::geom_line(linewidth = 1, colour = "#2C3E50") + ggplot2::geom_point(colour = "#2C3E50")
    else p + ggplot2::geom_line(ggplot2::aes(colour = .data$sex), linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(colour = .data$sex)) +
      ggplot2::scale_colour_manual(values = c(Hombre = "#2C7FB8", Mujer = "#D95F0E", Indeterminado = "#7F7F7F"), name = NULL)
  })

  stamp <- function() paste0(result()$params$dataset, "_", gsub("[^A-Za-z0-9]+", "_", input$icd), "_",
                             input$years[1], "-", input$years[2])
  output$dl_table <- downloadHandler(
    filename = function() paste0(stamp(), "_table.csv"),
    content = function(file) readr::write_csv(shown_table(), file, na = "")
  )
  output$dl_std <- downloadHandler(
    filename = function() paste0(stamp(), "_standardized.csv"),
    content = function(file) readr::write_csv(result()$standardized, file, na = "")
  )
  output$dl_log <- downloadHandler(
    filename = function() paste0(stamp(), "_log.csv"),
    content = function(file) readr::write_csv(result()$log, file, na = "")
  )
  output$dl_all <- downloadHandler(
    filename = function() paste0(stamp(), ".zip"),
    content = function(file) {
      d <- file.path(tempdir(), paste0("inectab_", as.integer(Sys.time())))
      p <- write_tabulation(result(), d, prefix = stamp())
      zip::zip(file, files = basename(p), root = d)
    }
  )
}

shinyApp(ui, server)
