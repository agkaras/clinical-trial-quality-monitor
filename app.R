# =============================================================================
# app.R — Clinical Trial Quality Monitor
# Cardiovascular & Vascular Metabolism
# Data source: ClinicalTrials.gov API v2
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)

# Source project modules
source("fetch_trials.R")
source("data_quality.R")
source("visualizations.R")


# =============================================================================
# Helper
# =============================================================================

icon_text <- function(icon_name, label) {
  tagList(bsicons::bs_icon(icon_name), label)
}


# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(

  title = "CV Trial Quality Monitor",
  theme = bs_theme(
    bootswatch  = "flatly",
    primary     = "#1565C0",
    base_font   = font_google("Inter"),
    heading_font = font_google("Inter")
  ),

  # -------------------------------------------------------------------
  # Tab 1: Overview
  # -------------------------------------------------------------------
  nav_panel(
    title = icon_text("bar-chart", "Overview"),

    layout_columns(
      fill = FALSE,
      col_widths = c(3, 3, 3, 3),

      value_box(
        title    = "Total Studies",
        value    = textOutput("n_total"),
        showcase = bsicons::bs_icon("clipboard2-pulse"),
        theme    = "primary"
      ),
      value_box(
        title    = "Recruiting Now",
        value    = textOutput("n_recruiting"),
        showcase = bsicons::bs_icon("person-plus"),
        theme    = "success"
      ),
      value_box(
        title    = "High Risk",
        value    = textOutput("n_high_risk"),
        showcase = bsicons::bs_icon("exclamation-triangle"),
        theme    = "danger"
      ),
      value_box(
        title    = "No Results Posted",
        value    = textOutput("n_no_results"),
        showcase = bsicons::bs_icon("file-earmark-x"),
        theme    = "warning"
      )
    ),

    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Study Status Distribution"),
        plotOutput("plot_status", height = "320px")
      ),
      card(
        card_header("Trials by Phase"),
        plotOutput("plot_phase", height = "320px")
      )
    ),

    layout_columns(
      col_widths = c(12),
      card(
        card_header("Study Start Timeline by Status"),
        plotOutput("plot_timeline", height = "300px")
      )
    )
  ),

  # -------------------------------------------------------------------
  # Tab 2: Data Quality
  # -------------------------------------------------------------------
  nav_panel(
    title = icon_text("shield-check", "Data Quality"),

    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        h5("Filters"),
        selectInput(
          "filter_status",
          "Study Status",
          choices  = c("All", "RECRUITING", "COMPLETED", "TERMINATED",
                       "ACTIVE_NOT_RECRUITING", "WITHDRAWN", "SUSPENDED"),
          selected = "All"
        ),
        selectInput(
          "filter_phase",
          "Phase",
          choices  = c("All"),
          selected = "All"
        ),
        selectInput(
          "filter_sponsor_class",
          "Sponsor Class",
          choices  = c("All", "INDUSTRY", "NIH", "FED", "OTHER_GOV",
                       "INDIV", "NETWORK", "UNKNOWN"),
          selected = "All"
        ),
        hr(),
        checkboxInput("flag_only", "Show flagged studies only", value = FALSE)
      ),

      layout_columns(
        col_widths = c(12),
        card(
          card_header("Quality Check Summary"),
          plotOutput("plot_flags", height = "340px")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Risk Profile by Sponsor Class"),
          plotOutput("plot_risk_sponsor", height = "300px")
        ),
        card(
          card_header("Enrollment by Phase"),
          plotOutput("plot_enrollment", height = "300px")
        )
      )
    )
  ),

  # -------------------------------------------------------------------
  # Tab 3: Study Explorer
  # -------------------------------------------------------------------
  nav_panel(
    title = icon_text("table", "Study Explorer"),

    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        h5("Search & Filter"),
        textInput("search_title", "Title contains", placeholder = "e.g. endothelial"),
        selectInput(
          "exp_status",
          "Status",
          choices  = c("All"),
          selected = "All"
        ),
        selectInput(
          "exp_phase",
          "Phase",
          choices  = c("All"),
          selected = "All"
        ),
        numericInput("enroll_min", "Min enrollment", value = 0, min = 0),
        numericInput("enroll_max", "Max enrollment", value = 10000, min = 0),
        checkboxGroupInput(
          "risk_filter",
          "Risk Level",
          choices  = c("Clean", "Low", "Medium", "High"),
          selected = c("Clean", "Low", "Medium", "High")
        ),
        downloadButton("download_csv", "Download filtered CSV",
                       class = "btn-sm btn-outline-primary w-100 mt-2")
      ),

      card(
        card_header("Filtered Studies"),
        DTOutput("table_studies")
      )
    )
  ),

  # -------------------------------------------------------------------
  # Tab 4: Top Sponsors
  # -------------------------------------------------------------------
  nav_panel(
    title = icon_text("building", "Sponsors"),

    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Top 15 Sponsors by Study Count"),
        plotOutput("plot_sponsors", height = "480px")
      ),
      card(
        card_header("Sponsor Summary Table"),
        DTOutput("table_sponsors")
      )
    )
  ),

  # Footer
  nav_spacer(),
  nav_item(
    tags$small(
      class = "text-muted me-3",
      "Data: ClinicalTrials.gov API v2 | Filters: Interventional, CV/vascular metabolism"
    )
  )
)


# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # -------------------------------------------------------------------
  # Data loading with progress
  # -------------------------------------------------------------------
  trials_raw <- reactive({
    withProgress(message = "Fetching trials from ClinicalTrials.gov...", {
      cache_path <- "data/trials_cache.rds"
      if (file.exists(cache_path)) {
        incProgress(0.5, detail = "Loading from cache...")
        df <- load_trials_cache(cache_path)
      } else {
        incProgress(0.2, detail = "Querying API...")
        df <- fetch_cv_trials(max_pages = 4, verbose = FALSE)
        incProgress(0.7, detail = "Saving cache...")
        save_trials_cache(df, cache_path)
      }
      df
    })
  })

  trials_qc <- reactive({
    df <- trials_raw()
    req(nrow(df) > 0)
    tryCatch(
      run_quality_checks(df),
      error = function(e) {
        message("QC error: ", e$message)
        NULL
      }
    )
  })

  # Populate dynamic filter choices after data load
  observe({
    df <- trials_qc()
    phases     <- c("All", sort(unique(df$phase[df$phase != "N/A"])))
    statuses   <- c("All", sort(unique(df$status[!is.na(df$status)])))

    updateSelectInput(session, "filter_phase",   choices = phases)
    updateSelectInput(session, "exp_status",     choices = statuses)
    updateSelectInput(session, "exp_phase",      choices = phases)
  })

  # -------------------------------------------------------------------
  # Tab 1: Overview metrics
  # -------------------------------------------------------------------
  output$n_total <- renderText({
    req(!is.null(trials_qc()), nrow(trials_qc()) > 0)
    nrow(trials_qc())
  })
  output$n_recruiting <- renderText({
    req(!is.null(trials_qc()), nrow(trials_qc()) > 0)
    sum(trials_qc()$status == "RECRUITING", na.rm = TRUE)
  })
  output$n_high_risk <- renderText({
    req(!is.null(trials_qc()), nrow(trials_qc()) > 0)
    sum(trials_qc()$risk_level == "High", na.rm = TRUE)
  })
  output$n_no_results <- renderText({
    req(!is.null(trials_qc()), nrow(trials_qc()) > 0)
    sum(trials_qc()$flag_no_results, na.rm = TRUE)
  })

  output$plot_status <- renderPlot({
    req(!is.null(trials_qc()), nrow(trials_qc()) > 0)
    plot_status_distribution(trials_qc())
  }, res = 96, height = 350)

  output$plot_phase <- renderPlot({
    req(!is.null(trials_qc()), nrow(trials_qc()) > 0)
    plot_phase_breakdown(trials_qc())
  }, res = 96, height = 350)

  output$plot_timeline <- renderPlot({
    req(!is.null(trials_qc()), nrow(trials_qc()) > 0)
    tl <- status_timeline(trials_qc())
    req(nrow(tl) > 0)
    plot_start_timeline(tl)
  }, res = 96, height = 320)

  # -------------------------------------------------------------------
  # Tab 2: Filtered data for QC tab
  # -------------------------------------------------------------------
  filtered_qc <- reactive({
    df <- trials_qc()

    if (input$filter_status != "All")
      df <- filter(df, status == input$filter_status)
    if (input$filter_phase != "All")
      df <- filter(df, phase == input$filter_phase)
    if (input$filter_sponsor_class != "All")
      df <- filter(df, sponsor_class == input$filter_sponsor_class)
    if (input$flag_only)
      df <- filter(df, risk_level %in% c("Low", "Medium", "High"))

    df
  })

  output$plot_flags <- renderPlot({
    req(nrow(filtered_qc()) > 0)
    sq <- quality_summary(filtered_qc())
    plot_quality_flags(sq)
  }, res = 96, height = 360)

  output$plot_risk_sponsor <- renderPlot({
    req(nrow(filtered_qc()) > 0)
    plot_risk_by_sponsor(filtered_qc())
  }, res = 96, height = 320)

  output$plot_enrollment <- renderPlot({
    req(nrow(filtered_qc()) > 0)
    plot_enrollment_by_phase(filtered_qc())
  }, res = 96, height = 320)

  # -------------------------------------------------------------------
  # Tab 3: Study Explorer
  # -------------------------------------------------------------------
  filtered_explorer <- reactive({
    df <- trials_qc()

    if (nchar(input$search_title) > 0)
      df <- filter(df, grepl(input$search_title, title, ignore.case = TRUE))
    if (input$exp_status != "All")
      df <- filter(df, status == input$exp_status)
    if (input$exp_phase != "All")
      df <- filter(df, phase == input$exp_phase)

    df <- filter(df,
      is.na(enrollment_count) |
        (enrollment_count >= input$enroll_min &
           enrollment_count <= input$enroll_max)
    )

    df <- filter(df, risk_level %in% input$risk_filter)
    df
  })

  output$table_studies <- renderDT({
    filtered_explorer() |>
      select(
        NCT_ID      = nct_id,
        Title       = title,
        Status      = status,
        Phase       = phase,
        Enrollment  = enrollment_count,
        Sponsor     = sponsor_name,
        `Start`     = start_date,
        `Completion`= completion_date,
        Risk        = risk_level,
        `Risk Score`= risk_score
      ) |>
      datatable(
        filter   = "top",
        rownames = FALSE,
        options  = list(pageLength = 15, scrollX = TRUE),
        escape   = FALSE
      ) |>
      formatStyle(
        "Risk",
        backgroundColor = styleEqual(
          c("Clean", "Low", "Medium", "High"),
          c("#E8F5E9", "#F9FBE7", "#FFF3E0", "#FFEBEE")
        )
      )
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("cv_trials_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(filtered_explorer(), file, row.names = FALSE)
    }
  )

  # -------------------------------------------------------------------
  # Tab 4: Sponsors
  # -------------------------------------------------------------------
  output$plot_sponsors <- renderPlot({
    req(nrow(trials_qc()) > 0)
    plot_top_sponsors(trials_qc(), n = 15)
  }, res = 96, height = 500)

  output$table_sponsors <- renderDT({
    trials_qc() |>
      count(sponsor_name, sponsor_class, name = "n_studies") |>
      arrange(desc(n_studies)) |>
      slice_head(n = 30) |>
      rename(Sponsor = sponsor_name, Class = sponsor_class, Studies = n_studies) |>
      datatable(rownames = FALSE, options = list(pageLength = 15))
  })
}


# =============================================================================
# Launch
# =============================================================================

shinyApp(ui = ui, server = server)
