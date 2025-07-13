# ShinyLive App for Tree Planting Survey Data
# Enhanced version with improved data loading, error handling, and performance
# This app visualizes key indicators from the ETH 2025 Decurrence Tree Value Survey

# ---- Load Libraries ----
library(shiny)
library(tidyverse)
library(googlesheets4)
library(janitor)
library(DT)
library(plotly)
library(scales)
library(shinycssloaders)
library(bslib)
library(promises)
library(future)

plan(multisession)

# ---- App Configuration ----
sheet_url <- "https://docs.google.com/spreadsheets/d/1VHn2zFbGbpmB-inzJZUeNfrTwEJ--pBB5MjRmxPAVT0/edit?gid=15730184#gid=15730184"
sheet_name <- "Farmer Detail"
tree_types <- c("gesho", "grevillea", "decurrens", "wanza", "papaya", "moringa", "coffee", "guava", "lemon")

# ---- Color Palette ----
primary_green   <- "#2E7D32"
secondary_green <- "#4CAF50"
light_green     <- "#E8F5E8"
accent_orange   <- "#FF8F00"
accent_blue   <- "#1976D2"
accent_purple   <- "#7B1FA2"
warm_red        <- "#D32F2F"
nature_colors   <- c("#2E7D32", "#4CAF50", "#FF8F00", "#1976D2", "#7B1FA2", "#D32F2F", "#795548", "#607D8B", "#FF5722", "#009688")
nature_colors_extended <- c(nature_colors, "#FFC107", "#00BCD4", "#8BC34A", "#E91E63", "#9C27B0", "#3F51B5", "#CDDC39", "#FFEB3B", "#FF9800", "#F44336")

# ---- Data Loading Functions ----
local_data_path <- file.path("data", "farmer_data.rds")

# Helper: process raw Google Sheet data
process_sheet_data <- function(df) {
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::select(cluster, woreda, kebele, purpose_for_taking_t_seedling_check_one, name_of_farmer, sex_m_f, age, male_youth_16_35_yrs, female_youth_16_35_yrs, is_this_a_repeat_customer_yes_no, total_of_gesho_seedlings, gesho_price, grevillea_price, total_of_grevillea_seedlings, total_of_decurrens_seedlings, decurrens_price, total_of_wanza_seedlings, wanza_price, total_of_papaya_seedlings, papaya_price, total_of_moringa_seedlings, moringa_price, total_of_coffee_seedlings, coffee_price, total_of_guava_seedlings, guava_price, total_of_lmon_seedlings, lemon_price, mobile_number_if_any, female_youth_16_35_yrs_2) %>%
    dplyr::rename(
      purpose = purpose_for_taking_t_seedling_check_one,
      repeat_customer = is_this_a_repeat_customer_yes_no,
      gesho = total_of_gesho_seedlings,
      grevillea = total_of_grevillea_seedlings,
      decurrens = total_of_decurrens_seedlings,
      wanza = total_of_wanza_seedlings,
      papaya = total_of_papaya_seedlings,
      moringa = total_of_moringa_seedlings,
      coffee = total_of_coffee_seedlings,
      guava = total_of_guava_seedlings,
      lemon = total_of_lmon_seedlings,
      mobile_number = mobile_number_if_any,
      sex = sex_m_f
    ) %>%
    dplyr::mutate(
      total_seedling = gesho + grevillea + decurrens + wanza + papaya + moringa + coffee + guava + lemon,
      has_phone = !is.na(mobile_number) & mobile_number != "",
      age_group = dplyr::case_when(
        age < 25 ~ "Under 25",
        age >= 25 & age < 35 ~ "25-34",
        age >= 35 & age < 45 ~ "35-44",
        age >= 45 & age < 55 ~ "45-54",
        age >= 55 ~ "55+",
        TRUE ~ "Unknown"
      ),
      total_revenue = (gesho * gesho_price) + (grevillea * grevillea_price) + 
                      (decurrens * decurrens_price) + (wanza * wanza_price) + 
                      (papaya * papaya_price) + (moringa * moringa_price) + 
                      (coffee * coffee_price) + (guava * guava_price) + 
                      (lemon * lemon_price)
    )
  expected_cols <- c("cluster", "woreda", "kebele", "purpose", "name_of_farmer", "sex", "age", "male_youth_16_35_yrs", "female_youth_16_35_yrs", "repeat_customer", "gesho", "gesho_price", "grevillea", "grevillea_price", "decurrens", "decurrens_price", "wanza", "wanza_price", "papaya", "papaya_price", "moringa", "moringa_price", "coffee", "coffee_price", "guava", "guava_price", "lemon", "lemon_price", "mobile_number", "female_youth_16_35_yrs_2", "total_seedling", "has_phone", "age_group", "total_revenue")
  missing_cols <- setdiff(expected_cols, names(df))
  attr(df, "missing_columns") <- missing_cols
  df
}

# Main data loader: load from local file if exists, else fetch and save
load_data <- function() {
  tryCatch({
    if (file.exists(local_data_path)) {
      df <- readRDS(local_data_path)
    } else {
      df_raw <- googlesheets4::read_sheet(sheet_url, sheet = sheet_name)
      df <- process_sheet_data(df_raw)
      saveRDS(df, local_data_path)
    }
    df
  }, error = function(e) {
    structure(list(message = paste("Data load failed:", e$message)), class = "data_load_error")
  })
}

# Helper: force refresh from Google Sheets and overwrite local file
refresh_data_from_gsheets <- function() {
  tryCatch({
    df_raw <- googlesheets4::read_sheet(sheet_url, sheet = sheet_name)
    df <- process_sheet_data(df_raw)
    saveRDS(df, local_data_path)
    df
  }, error = function(e) {
    structure(list(message = paste("Data refresh failed:", e$message)), class = "data_load_error")
  })
}

# ---- Helper function for empty plots ----
plotly_empty <- function(message = "No data available for the selected filters.") {
  plotly::plot_ly() %>%
    plotly::layout(
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      annotations = list(
        x = 0.5, y = 0.5, text = message, xref = "paper", yref = "paper",
        showarrow = FALSE, font = list(size = 14, color = "#6C757D")
      ),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
}

# ---- UI Definition ----
ui <- fluidPage(
  theme = bs_theme(
    version = 5, bg = "#FFFFFF", fg = "#2C3E50", primary = "#2E7D32",
    secondary = "#4CAF50", success = "#4CAF50", info = "#1976D2",
    warning = "#FF8F00", danger = "#D32F2F", base_font = font_google("Inter"),
    heading_font = font_google("Poppins"), code_font = font_google("JetBrains Mono")
  ),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$style(HTML('\
      body { background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%); font-family: "Inter", sans-serif; line-height: 1.6; }\
      .main-header { background: linear-gradient(135deg, #2E7D32 0%, #4CAF50 100%); color: white; padding: 25px 0; margin-bottom: 30px; border-radius: 0 0 20px 20px; box-shadow: 0 8px 32px rgba(46, 125, 50, 0.3); }\
      .header-content { display: flex; align-items: center; justify-content: center; gap: 20px; }\
      .header-icon { font-size: 3rem; color: #E8F5E8; text-shadow: 2px 2px 4px rgba(0,0,0,0.3); }\
      .header-title { font-family: "Poppins", sans-serif; font-size: 2.5rem; font-weight: 700; margin: 0; text-shadow: 2px 2px 4px rgba(0,0,0,0.3); }\
      .header-subtitle { font-size: 1.1rem; opacity: 0.9; margin-top: 5px; font-weight: 300; }\
      .sidebar-panel { background: white; border-radius: 15px; box-shadow: 0 4px 20px rgba(0,0,0,0.1); padding: 0; border: none; }\
      .filter-section { background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); padding: 20px; border-radius: 15px 15px 0 0; border-bottom: 3px solid #2E7D32; }\
      .filter-title { color: #2E7D32; font-weight: 600; margin-bottom: 15px; display: flex; align-items: center; gap: 10px; }\
      .options-section { padding: 20px; }\
      .value-box { background: white; border-radius: 15px; padding: 25px 20px; text-align: center; box-shadow: 0 4px 20px rgba(0,0,0,0.1); border: none; transition: all 0.3s ease; position: relative; overflow: hidden; }\
      .value-box:hover { transform: translateY(-5px); box-shadow: 0 8px 30px rgba(0,0,0,0.15); }\
      .value-box::before { content: ""; position: absolute; top: 0; left: 0; right: 0; height: 4px; background: linear-gradient(90deg, #2E7D32, #4CAF50); }\
      .value-box-icon { font-size: 2.5rem; margin-bottom: 15px; opacity: 0.8; }\
      .value-box-value { font-size: 2.2rem; font-weight: 700; color: #2C3E50; margin-bottom: 8px; }\
      .value-box-subtitle { color: #6C757D; font-size: 0.95rem; font-weight: 500; text-transform: uppercase; letter-spacing: 0.5px; }\
      .chart-container { background: white; border-radius: 15px; padding: 25px; margin-bottom: 25px; box-shadow: 0 4px 20px rgba(0,0,0,0.1); border: 1px solid #f0f0f0; }\
      .chart-title { color: #2C3E50; font-weight: 600; margin-bottom: 20px; padding-bottom: 10px; border-bottom: 2px solid #f0f0f0; display: flex; align-items: center; gap: 10px; }\
      .chart-icon { color: #4CAF50; font-size: 1.2rem; }\
      .nav-tabs .nav-link.active { background: linear-gradient(135deg, #2E7D32 0%, #4CAF50 100%); color: white; border: none; }\
      .loading-message { text-align: center; padding: 50px; color: #6C757D; }\
      .error-container { background: #fff3cd; border: 1px solid #ffeaa7; border-radius: 10px; padding: 20px; margin: 20px; }\
      .refresh-btn { margin: 10px; }\
    '))
  ),
  
  # Page Header
  div(class = "main-header",
    div(class = "container-fluid",
      div(class = "header-content",
        tags$i(class = "fas fa-seedling header-icon"),
        div(
          h1(class = "header-title", "ETH 2025 Seedling Distribution Dashboard")
        )
      )
    )
  ),
  
  # Main Content
  div(class = "container-fluid",
    # Loading/Error/Success UI
    conditionalPanel(
      condition = "output.data_status == 'loading'",
      div(class = "loading-message",
          withSpinner(
            div(
              h4("Loading Data..."),
              p("Please wait while we fetch the latest data from Google Sheets.")
            ),
            type = 4, color = primary_green
          )
      )
    ),
    conditionalPanel(
      condition = "output.data_status == 'error'",
      div(class = "error-container",
          h4(style = "color: #D32F2F;", 
             tags$i(class = "fas fa-exclamation-triangle", title = "Error icon"), 
             " Data Loading Error"),
          verbatimTextOutput("error_details"),
          actionButton("retry_load", "Retry Loading Data", 
                      class = "btn btn-primary refresh-btn",
                      icon = icon("refresh"),
                      title = "Try loading the data again")
      )
    ),
    conditionalPanel(
      condition = "output.data_status == 'success'",
      # Main dashboard UI
      sidebarLayout(
        sidebarPanel(
          width = 3, class = "sidebar-panel",
          div(class = "filter-section",
            h4(class = "filter-title", tags$i(class = "fas fa-filter", title = "Filter icon"), "Data Filters"),
            uiOutput("purpose_filter"),
            uiOutput("cluster_filter"),
            uiOutput("woreda_filter"),
            actionButton("refresh_data", "Refresh Data", 
                        class = "btn btn-outline-primary btn-sm refresh-btn",
                        icon = icon("refresh"),
                        title = "Reload the latest data from Google Sheets")
          ),
          div(class = "options-section",
            h4(class = "filter-title", tags$i(class = "fas fa-cog", title = "Settings icon"), "Display Options"),
            checkboxInput("show_percentages", "Show Percentages in Charts", value = TRUE),
            hr(),
            h5("Data Info:"),
            verbatimTextOutput("data_info", placeholder = TRUE),
            textOutput("last_refresh")
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            id = "main_tabs", type = "tabs",
            # Overview Tab
            tabPanel(title = tagList(tags$i(class = "fas fa-chart-pie", title = "Overview of all data"), "Overview"), value = "overview",
              fluidRow(
                column(3, div(class = "value-box", 
                             tags$i(class = "fas fa-users value-box-icon", title = "Total number of farmers"), 
                             div(class = "value-box-value", textOutput("total_farmers")), 
                             div(class = "value-box-subtitle", "Total Farmers"))),
                column(3, div(class = "value-box", 
                             tags$i(class = "fas fa-seedling value-box-icon", title = "Total seedlings distributed"), 
                             div(class = "value-box-value", textOutput("total_seedlings")), 
                             div(class = "value-box-subtitle", "Total Seedlings"))),
                column(3, div(class = "value-box", 
                             tags$i(class = "fas fa-chart-line value-box-icon", title = "Average seedlings per farmer"), 
                             div(class = "value-box-value", textOutput("avg_seedlings")), 
                             div(class = "value-box-subtitle", "Avg. per Farmer"))),
                column(3, div(class = "value-box", 
                             tags$i(class = "fas fa-mobile-alt value-box-icon", title = "Percentage of farmers with phones"), 
                             div(class = "value-box-value", textOutput("phone_ownership")), 
                             div(class = "value-box-subtitle", "Have Phones")))
              ),
              fluidRow(
                column(6, div(class = "chart-container", 
                             h4(class = "chart-title", tags$i(class = "chart-icon fas fa-chart-pie", title = "Tree species distribution"), "Tree Species Distribution"), 
                             withSpinner(plotlyOutput("donut_plot"), type = 4, color = accent_blue))),
                column(6, div(class = "chart-container", 
                             h4(class = "chart-title", tags$i(class = "chart-icon fas fa-bullseye", title = "Purpose analysis"), "Purpose Analysis"), 
                             withSpinner(plotlyOutput("purpose_plot"), type = 4, color = accent_blue)))
              )
            ),
            tabPanel(title = tagList(tags$i(class = "fas fa-globe-africa", title = "Geographic Analysis"), "Geographic"),
                     fluidRow(
                       column(6, div(class = "chart-container",
                                    h4(class = "chart-title", tags$i(class = "chart-icon fas fa-users", title = "Farmers per cluster"), "Farmers per Cluster"),
                                    withSpinner(plotlyOutput("cluster_farmers"), type = 4, color = accent_blue))),
                       column(6, div(class = "chart-container",
                                    h4(class = "chart-title", tags$i(class = "chart-icon fas fa-seedling", title = "Seedlings per cluster"), "Average Seedlings per Cluster"),
                                    withSpinner(plotlyOutput("cluster_seedlings"), type = 4, color = accent_blue)))
                     ),
                     fluidRow(
                       column(12, div(class = "chart-container",
                                     h4(class = "chart-title", tags$i(class = "chart-icon fas fa-map-marked-alt", title = "Farmers per woreda"), "Farmers per Woreda"),
                                     withSpinner(plotlyOutput("woreda_plot"), type = 4, color = accent_blue)))
                     )
            ),
            tabPanel(title = tagList(tags$i(class = "fas fa-user-friends", title = "Demographics"), "Demographics"),
                     fluidRow(
                       column(6, div(class = "chart-container",
                                    h4(class = "chart-title", tags$i(class = "chart-icon fas fa-birthday-cake", title = "Age distribution"), "Age Distribution"),
                                    withSpinner(plotlyOutput("age_plot"), type = 4, color = accent_blue))),
                       column(6, div(class = "chart-container",
                                    h4(class = "chart-title", tags$i(class = "chart-icon fas fa-venus-mars", title = "Gender distribution"), "Gender Distribution"),
                                    withSpinner(plotlyOutput("gender_plot"), type = 4, color = accent_blue)))
                     ),
                     fluidRow(
                       column(6, div(class = "chart-container",
                                    h4(class = "chart-title", tags$i(class = "chart-icon fas fa-redo-alt", title = "Repeat customers"), "Repeat Customers"),
                                    withSpinner(plotlyOutput("repeat_plot"), type = 4, color = accent_blue))),
                       column(6, div(class = "chart-container",
                                    h4(class = "chart-title", tags$i(class = "chart-icon fas fa-child", title = "Youth participation"), "Youth Participation"),
                                    withSpinner(plotlyOutput("youth_plot"), type = 4, color = accent_blue)))
                     )
            ),
            tabPanel(title = tagList(tags$i(class = "fas fa-tree", title = "Tree Analysis"), "Trees"),
                     fluidRow(
                       column(12, div(class = "chart-container",
                                     h4(class = "chart-title", tags$i(class = "chart-icon fas fa-th", title = "Tree species per cluster"), "Tree Species per Cluster (Heatmap)"),
                                     withSpinner(plotlyOutput("tree_cluster_heatmap"), type = 4, color = accent_blue)))
                     ),
                     fluidRow(
                       column(12, div(class = "chart-container",
                                     h4(class = "chart-title", tags$i(class = "chart-icon fas fa-table", title = "Tree statistics"), "Tree Statistics"),
                                     withSpinner(DT::dataTableOutput("tree_stats_table"), type = 4, color = accent_blue)))
                     )
            ),
            tabPanel(title = tagList(tags$i(class = "fas fa-mobile-alt", title = "Phone Ownership"), "Phone Data"),
                     fluidRow(
                       column(6, div(class = "chart-container",
                                    h4(class = "chart-title", tags$i(class = "chart-icon fas fa-percent", title = "Phone ownership per tree species"), "Phone Ownership by Tree Species"),
                                    withSpinner(plotlyOutput("phone_tree_species"), type = 4, color = accent_blue))),
                       column(6, div(class = "chart-container",
                                    h4(class = "chart-title", tags$i(class = "chart-icon fas fa-table", title = "Tree species for phone owners"), "Tree Species for Phone Owners"),
                                    withSpinner(DT::dataTableOutput("phone_owners_species_table"), type = 4, color = accent_blue)))
                     ),
                     fluidRow(
                       column(6, div(class = "chart-container",
                                    h4(class = "chart-title", tags$i(class = "chart-icon fas fa-table", title = "Phone ownership by woreda"), "Phone Ownership by Woreda"),
                                    withSpinner(DT::dataTableOutput("phone_stats_table"), type = 4, color = accent_blue))),
                       column(6, div(class = "chart-container",
                                    h4(class = "chart-title", tags$i(class = "chart-icon fas fa-table", title = "Avg. Seedlings per Farmer by Phone Ownership"), "Avg. Seedlings per Farmer by Phone Ownership"),
                                    withSpinner(DT::dataTableOutput("phone_avg_seedlings_table"), type = 4, color = accent_blue)))
                     )
            )
          )
        )
      )
    )
  )
)

# ---- Server Definition ----
server <- function(input, output, session) {
  # Reactive values to store data and state
  values <- reactiveValues(
    data = NULL,
    data_status = "loading", # "loading", "success", "error"
    error_message = NULL,
    last_update = Sys.time()
  )
  
  # ---- Data Loading ----
  load_data_reactive <- function() {
    values$data_status <- "loading"
    
    future_promise({
      load_data()
    }) %...>% (function(result) {
      if (inherits(result, "data_load_error")) {
        values$data_status <- "error"
        values$error_message <- result$message
        values$data <- NULL
      } else {
        values$data_status <- "success"
        values$data <- result
        values$error_message <- NULL
        values$last_update <- Sys.time()
      }
    }) %...!% (function(error) {
      values$data_status <- "error"
      values$error_message <- paste("Unexpected error:", error$message)
      values$data <- NULL
    })
  }

  # Add a separate refresh function to force update from Google Sheets
  refresh_data_reactive <- function() {
    values$data_status <- "loading"
    future_promise({
      refresh_data_from_gsheets()
    }) %...>% (function(result) {
      if (inherits(result, "data_load_error")) {
        values$data_status <- "error"
        values$error_message <- result$message
        values$data <- NULL
      } else {
        values$data_status <- "success"
        values$data <- result
        values$error_message <- NULL
        values$last_update <- Sys.time()
      }
    }) %...!% (function(error) {
      values$data_status <- "error"
      values$error_message <- paste("Unexpected error:", error$message)
      values$data <- NULL
    })
  }
  
  # Load data on startup
  observe({
    load_data_reactive()
  })
  
  # Retry button
  observeEvent(input$retry_load, {
    load_data_reactive()
  })
  
  # Refresh button
  observeEvent(input$refresh_data, {
    refresh_data_reactive()
  })
  
  # Data status output
  output$data_status <- reactive({
    values$data_status
  })
  outputOptions(output, "data_status", suspendWhenHidden = FALSE)
  
  # Error details output
  output$error_details <- renderText({
    if (!is.null(values$error_message)) {
      values$error_message
    } else {
      "Unknown error occurred."
    }
  })
  
  # Dynamic filter inputs
  output$purpose_filter <- renderUI({
    if (values$data_status == "success" && !is.null(values$data)) {
      purposes <- unique(values$data$purpose)
      purposes <- purposes[!is.na(purposes)]
      selectInput("purpose", "Purpose:", 
                 choices = c("All", purposes), 
                 selected = "All")
    }
  })
  
  output$cluster_filter <- renderUI({
    if (values$data_status == "success" && !is.null(values$data)) {
      clusters <- unique(values$data$cluster)
      clusters <- clusters[!is.na(clusters)]
      selectInput("cluster", "Cluster:", 
                 choices = c("All", clusters), 
                 selected = "All")
    }
  })

  output$woreda_filter <- renderUI({
    if (values$data_status == "success" && !is.null(values$data)) {
      woredas <- unique(values$data$woreda)
      woredas <- woredas[!is.na(woredas)]
      selectInput("woreda", "Woreda:", 
                 choices = c("All", woredas), 
                 selected = "All")
    }
  })
  
  # Data info output
  output$data_info <- renderText({
    if (values$data_status == "success" && !is.null(values$data)) {
      paste(
        "Rows:", nrow(values$data),
        "\nLast Updated:", format(values$last_update, "%Y-%m-%d %H:%M"),
        "\nMissing Columns:", paste(attr(values$data, "missing_columns"), collapse = ", ")
      )
    }
  })
  
  # Reactive filtered data
  filtered_df <- reactive({
    req(values$data_status == "success", values$data)
    data <- values$data
    if (!is.null(input$purpose) && input$purpose != "All") {
      data <- data %>% filter(purpose == input$purpose)
    }
    if (!is.null(input$cluster) && input$cluster != "All") {
      data <- data %>% filter(cluster == input$cluster)
    }
    if (!is.null(input$woreda) && input$woreda != "All") {
      data <- data %>% filter(woreda == input$woreda)
    }
    return(data)
  })
  
  # ---- Value Box Outputs ----
  output$total_farmers <- renderText({
    req(filtered_df())
    prettyNum(nrow(filtered_df()), big.mark = ",")
  })
  
  output$total_seedlings <- renderText({
    req(filtered_df())
    prettyNum(sum(filtered_df()$total_seedling, na.rm = TRUE), big.mark = ",")
  })
  
  output$avg_seedlings <- renderText({
    req(filtered_df())
    round(mean(filtered_df()$total_seedling, na.rm = TRUE), 1)
  })
  
  output$phone_ownership <- renderText({
    req(filtered_df())
    paste0(round(mean(filtered_df()$has_phone, na.rm = TRUE) * 100, 1), "%")
  })
  
  # ---- Plot Outputs ----
  
  # Donut Plot
  output$donut_plot <- renderPlotly({
    validate(need(filtered_df(), "No data loaded."))
    df <- filtered_df() %>%
      select(all_of(tree_types)) %>%
      pivot_longer(cols = everything(), names_to = "tree_type", values_to = "count") %>%
      group_by(tree_type) %>%
      summarise(total_count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
      filter(total_count > 0)
    validate(need(nrow(df) > 0, "No tree data available for selected filters."))
    plot_ly(df, labels = ~str_to_title(tree_type), values = ~total_count, 
            type = 'pie', hole = 0.4,
            textinfo = if(isTRUE(input$show_percentages)) 'label+percent' else 'label+value',
            marker = list(colors = nature_colors_extended[1:nrow(df)])) %>%
      layout(showlegend = TRUE, 
             title = list(text = "", font = list(size = 14)))
  })
  
  # Purpose Plot
  output$purpose_plot <- renderPlotly({
    req(filtered_df())
    df <- filtered_df() %>%
      group_by(purpose) %>%
      summarise(farmers = n(), seedlings = sum(total_seedling, na.rm = TRUE), .groups = 'drop') %>%
      filter(!is.na(purpose))
    total_farmers <- sum(df$farmers)
    total_seedlings <- sum(df$seedlings)
    df_long <- df %>%
      mutate(
        farmers_pct = 100 * farmers / total_farmers,
        seedlings_pct = 100 * seedlings / total_seedlings
      ) %>%
      select(purpose, farmers_pct, seedlings_pct) %>%
      pivot_longer(cols = c(farmers_pct, seedlings_pct), names_to = "type", values_to = "percent") %>%
      mutate(type = recode(type, farmers_pct = "% of Farmers", seedlings_pct = "% of Seedlings"))
    if (nrow(df_long) == 0) {
      return(plotly_empty("No purpose data available."))
    }
    plot_ly(df_long, x = ~purpose, y = ~percent, color = ~type, colors = c(primary_green, accent_orange),
            type = 'bar', text = ~sprintf("%.1f%%", percent), textposition = 'auto') %>%
      layout(
        barmode = 'group',
        yaxis = list(title = "Percentage", range = c(0, 100)),
        xaxis = list(title = "Purpose"),
        legend = list(title = list(text = ""), x = 0.01, y = 0.99)
      )
  })
  
  # Geographic plots
  output$cluster_farmers <- renderPlotly({
    req(filtered_df())
    
    df <- filtered_df() %>%
      group_by(cluster) %>%
      summarise(farmers = n(), .groups = 'drop') %>%
      filter(!is.na(cluster))
    
    if (nrow(df) == 0) {
      return(plotly_empty("No cluster data available."))
    }
    
    plot_ly(df, x = ~reorder(cluster, farmers), y = ~farmers, 
            type = 'bar', marker = list(color = secondary_green)) %>%
      layout(xaxis = list(title = "Cluster"), yaxis = list(title = "Number of Farmers"))
  })
  
  output$cluster_seedlings <- renderPlotly({
    req(filtered_df())
    
    df <- filtered_df() %>%
      group_by(cluster) %>%
      summarise(avg_seedlings = mean(total_seedling, na.rm = TRUE), .groups = 'drop') %>%
      filter(!is.na(cluster))
    
    if (nrow(df) == 0) {
      return(plotly_empty("No cluster data available."))
    }
    
    plot_ly(df, x = ~reorder(cluster, avg_seedlings), y = ~avg_seedlings, 
            type = 'bar', marker = list(color = accent_orange)) %>%
      layout(xaxis = list(title = "Cluster"), yaxis = list(title = "Average Seedlings per Farmer"))
  })
  
  output$woreda_plot <- renderPlotly({
    req(filtered_df())
    
    df <- filtered_df() %>%
      group_by(woreda) %>%
      summarise(farmers = n(), .groups = 'drop') %>%
      filter(!is.na(woreda))
    
    if (nrow(df) == 0) {
      return(plotly_empty("No woreda data available."))
    }
    
    plot_ly(df, x = ~reorder(woreda, farmers), y = ~farmers, 
            type = 'bar', marker = list(color = primary_green)) %>%
      layout(xaxis = list(title = "Woreda"), yaxis = list(title = "Number of Farmers"))
  })
  
  # Demographics plots
  output$age_plot <- renderPlotly({
    req(filtered_df())
    df <- filtered_df() %>%
      filter(!is.na(age), age > 0, age < 120) %>%
      mutate(age_group = cut(age, 
                           breaks = c(0, 25, 35, 45, 55, Inf), 
                           labels = c("Under 25", "25-34", "35-44", "45-54", "55+"),
                           include.lowest = TRUE)) %>%
      count(age_group, .drop = FALSE)
    total <- sum(df$n)
    df <- df %>% mutate(percent = 100 * n / total)
    if (nrow(df) == 0 || sum(df$n) == 0) {
      return(plotly_empty("No age data available."))
    }
    plot_ly(df, x = ~age_group, y = ~percent, type = 'bar',
            marker = list(color = nature_colors[1:nrow(df)]),
            text = ~sprintf("%.1f%%", percent), textposition = 'auto') %>%
      layout(xaxis = list(title = "Age Group"), yaxis = list(title = "Percent of Farmers", range = c(0, 100)))
  })
  
  output$gender_plot <- renderPlotly({
    req(filtered_df())
    
    df <- filtered_df() %>%
      filter(!is.na(sex), sex != "Unknown") %>%
      count(sex)
    
    if (nrow(df) == 0) {
      return(plotly_empty("No gender data available."))
    }
    
    plot_ly(df, labels = ~sex, values = ~n, type = 'pie',
            marker = list(colors = c(primary_green, accent_orange))) %>%
      layout(showlegend = TRUE)
  })
  
  output$repeat_plot <- renderPlotly({
    req(filtered_df())
    
    df <- filtered_df() %>%
      filter(!is.na(repeat_customer), repeat_customer != "Unknown") %>%
      count(repeat_customer)
    
    if (nrow(df) == 0) {
      return(plotly_empty("No repeat customer data available."))
    }
    
    plot_ly(df, labels = ~repeat_customer, values = ~n, type = 'pie',
            marker = list(colors = c(accent_blue, warm_red))) %>%
      layout(showlegend = TRUE)
  })
  
  output$youth_plot <- renderPlotly({
    req(filtered_df())
    
    df <- filtered_df() %>%
      summarise(
        `Male Youth` = sum(male_youth_16_35_yrs, na.rm = TRUE),
        `Female Youth` = sum(female_youth_16_35_yrs, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(everything(), names_to = "Gender", values_to = "Count")
    
    if (sum(df$Count) == 0) {
      return(plotly_empty("No youth data available."))
    }
    
    plot_ly(df, x = ~Gender, y = ~Count, type = 'bar',
            marker = list(color = c(accent_blue, accent_purple))) %>%
      layout(xaxis = list(title = "Youth Category"), yaxis = list(title = "Count"))
  })
  
  # Tree analysis plots
  output$tree_cluster_heatmap <- renderPlotly({
    req(filtered_df())
    
    df <- filtered_df() %>%
      select(cluster, all_of(tree_types)) %>%
      filter(!is.na(cluster)) %>%
      pivot_longer(-cluster, names_to = "tree_type", values_to = "count") %>%
      group_by(cluster, tree_type) %>%
      summarise(total_count = sum(count, na.rm = TRUE), .groups = 'drop')
    
    if (nrow(df) == 0) {
      return(plotly_empty("No tree-cluster data available."))
    }
    
    plot_ly(df, x = ~str_to_title(tree_type), y = ~cluster, z = ~total_count, 
            type = "heatmap",
            colorscale = list(c(0, light_green), c(1, primary_green))) %>%
      layout(xaxis = list(title = "Tree Species"), yaxis = list(title = "Cluster"))
  })
  
  # Tables
  output$tree_stats_table <- DT::renderDataTable({
    validate(need(filtered_df(), "No data loaded."))
    df <- filtered_df() %>%
      select(all_of(tree_types)) %>%
      pivot_longer(everything(), names_to = "Tree Type", values_to = "count") %>%
      group_by(`Tree Type`) %>%
      summarise(
        `Total Seedlings` = sum(count, na.rm = TRUE),
        `Farmers Buying` = sum(count > 0, na.rm = TRUE),
        `Avg per Farmer` = round(mean(count[count > 0], na.rm = TRUE), 1),
        `Min per Farmer` = ifelse(sum(count > 0, na.rm = TRUE) > 0, min(count[count > 0], na.rm = TRUE), NA),
        `Max per Farmer` = ifelse(sum(count > 0, na.rm = TRUE) > 0, max(count[count > 0], na.rm = TRUE), NA),
        .groups = 'drop'
      ) %>%
      mutate(`Tree Type` = str_to_title(`Tree Type`)) %>%
      arrange(desc(`Total Seedlings`))
    datatable(df, options = list(pageLength = 10, dom = 'tp'), rownames = FALSE)
  })
  
  # Phone ownership plots
  output$phone_tree_species <- renderPlotly({
    req(filtered_df())
    
    df <- filtered_df() %>%
      select(has_phone, all_of(tree_types)) %>%
      pivot_longer(cols = all_of(tree_types), names_to = "tree_type", values_to = "count") %>%
      filter(count > 0) %>%
      group_by(tree_type, has_phone) %>%
      summarise(n = n(), .groups = 'drop') %>%
      group_by(tree_type) %>%
      mutate(percent = n / sum(n) * 100) %>%
      filter(has_phone == TRUE)
    
    if (nrow(df) == 0) {
      return(plotly_empty("No phone ownership data available."))
    }
    
    plot_ly(df, x = ~str_to_title(tree_type), y = ~percent, type = 'bar',
            marker = list(color = primary_green),
            text = ~sprintf("%.1f%%", percent), textposition = 'outside') %>%
      layout(xaxis = list(title = "Tree Species"), 
             yaxis = list(title = '% Phone Ownership', range = c(0, 100)))
  })
  
  output$phone_owners_species_table <- DT::renderDataTable({
    validate(need(filtered_df(), "No data loaded."))
    # Count for phone owners
    df_owners <- filtered_df() %>%
      filter(has_phone) %>%
      select(all_of(tree_types)) %>%
      pivot_longer(cols = everything(), names_to = "Tree Species", values_to = "count") %>%
      filter(count > 0) %>%
      group_by(`Tree Species`) %>%
      summarise(Owners = n(), .groups = 'drop')
    # Total farmers for each species (owners + non-owners)
    df_total <- filtered_df() %>%
      select(all_of(tree_types)) %>%
      pivot_longer(cols = everything(), names_to = "Tree Species", values_to = "count") %>%
      filter(count > 0) %>%
      group_by(`Tree Species`) %>%
      summarise(TotalFarmers = n(), .groups = 'drop')
    df <- left_join(df_owners, df_total, by = "Tree Species") %>%
      mutate(`Tree Species` = str_to_title(`Tree Species`)) %>%
      arrange(desc(Owners))
    datatable(df, options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
  })
  
output$phone_avg_seedlings_table <- DT::renderDataTable({
  validate(need(filtered_df(), "No data loaded."))
  df <- filtered_df()
  # For each tree type, calculate avg per farmer for phone owners and non-owners, only for those who took > 0 seedlings
  results <- lapply(tree_types, function(tree) {
    df_tree <- df %>% filter(!is.na(.data[[tree]]), .data[[tree]] > 0)
    avg_non_owner <- df_tree %>% filter(!has_phone) %>% summarise(avg = mean(.data[[tree]], na.rm = TRUE)) %>% pull(avg)
    avg_owner <- df_tree %>% filter(has_phone) %>% summarise(avg = mean(.data[[tree]], na.rm = TRUE)) %>% pull(avg)
    tibble(
      `Tree Species` = str_to_title(tree),
      `Non-Owner` = round(avg_non_owner, 2),
      `Phone Owner` = round(avg_owner, 2),
      `Difference` = round((avg_owner - avg_non_owner), 2)
    )
  })
  df_out <- bind_rows(results)
  datatable(df_out, options = list(pageLength = 10, dom = 'tp'), rownames = FALSE)
})
}

# ---- Run Application ----
shinyApp(ui = ui, server = server)