# ShinyLive App for Tree Planting Survey Data
# Enhanced version with modern design, improved colors and comprehensive analytics
# This app visualizes key indicators from the ETH 2025 Decurrence Tree Value Survey

library(shiny)
library(tidyverse)
library(googlesheets4)
library(janitor)
library(DT)
library(plotly)
library(shinydashboard)
library(scales)
library(shinythemes)
library(shinycssloaders)
library(bslib)

# Google Sheet URL and sheet name
sheet_url <- "https://docs.google.com/spreadsheets/d/1VHn2zFbGbpmB-inzJZUeNfrTwEJ--pBB5MjRmxPAVT0/edit?gid=15730184#gid=15730184"
sheet_name <- "Farmer Detail"

# ---- Enhanced Color Palette and Tree Types ----
primary_green   <- "#2E7D32"
secondary_green <- "#4CAF50"
light_green     <- "#E8F5E8"
accent_orange   <- "#FF8F00"
accent_blue     <- "#1976D2"
accent_purple   <- "#7B1FA2"
warm_red        <- "#D32F2F"
nature_colors   <- c("#2E7D32", "#4CAF50", "#FF8F00", "#1976D2", "#7B1FA2", "#D32F2F", "#795548", "#607D8B", "#FF5722", "#009688")
tree_types <- c("gesho", "grevillea", "decurrens", "wanza", "papaya", "moringa", "coffee", "guava", "lemon")

# ---- Data Loading and Cleaning ----
# Placeholder for data, will be loaded in the server
df <- data.frame()

# ---- Enhanced UI with Modern Design ----
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#2C3E50",
    primary = "#2E7D32",
    secondary = "#4CAF50",
    success = "#4CAF50",
    info = "#1976D2",
    warning = "#FF8F00",
    danger = "#D32F2F",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins"),
    code_font = font_google("JetBrains Mono")
  ),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$style(HTML('
      /* Global Styles */
      body {
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        font-family: "Inter", sans-serif;
        line-height: 1.6;
      }
      
      /* Header Styles */
      .main-header {
        background: linear-gradient(135deg, #2E7D32 0%, #4CAF50 100%);
        color: white;
        padding: 25px 0;
        margin-bottom: 30px;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 8px 32px rgba(46, 125, 50, 0.3);
      }
      
      .header-content {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 20px;
      }
      
      .header-icon {
        font-size: 3rem;
        color: #E8F5E8;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
      }
      
      .header-title {
        font-family: "Poppins", sans-serif;
        font-size: 2.5rem;
        font-weight: 700;
        margin: 0;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
      }
      
      .header-subtitle {
        font-size: 1.1rem;
        opacity: 0.9;
        margin-top: 5px;
        font-weight: 300;
      }
      
      /* Sidebar Styles */
      .sidebar-panel {
        background: white;
        border-radius: 15px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        padding: 0;
        border: none;
      }
      
      .filter-section {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        padding: 20px;
        border-radius: 15px 15px 0 0;
        border-bottom: 3px solid #2E7D32;
      }
      
      .filter-title {
        color: #2E7D32;
        font-weight: 600;
        margin-bottom: 15px;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      .options-section {
        padding: 20px;
      }
      
      .form-select, .form-control {
        border: 2px solid #e9ecef;
        border-radius: 10px;
        padding: 12px 15px;
        transition: all 0.3s ease;
        font-size: 0.95rem;
      }
      
      .form-select:focus, .form-control:focus {
        border-color: #4CAF50;
        box-shadow: 0 0 0 0.2rem rgba(76, 175, 80, 0.25);
      }
      
      .form-check-input:checked {
        background-color: #4CAF50;
        border-color: #4CAF50;
      }
      
      /* Value Box Styles */
      .value-box {
        background: white;
        border-radius: 15px;
        padding: 25px 20px;
        text-align: center;
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        border: none;
        transition: all 0.3s ease;
        position: relative;
        overflow: hidden;
      }
      
      .value-box:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 30px rgba(0,0,0,0.15);
      }
      
      .value-box::before {
        content: "";
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, #2E7D32, #4CAF50);
      }
      
      .value-box-icon {
        font-size: 2.5rem;
        margin-bottom: 15px;
        opacity: 0.8;
      }
      
      .value-box-value {
        font-size: 2.2rem;
        font-weight: 700;
        color: #2C3E50;
        margin-bottom: 8px;
      }
      
      .value-box-subtitle {
        color: #6C757D;
        font-size: 0.95rem;
        font-weight: 500;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      /* Chart Container Styles */
      .chart-container {
        background: white;
        border-radius: 15px;
        padding: 25px;
        margin-bottom: 25px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        border: 1px solid #f0f0f0;
        transition: all 0.3s ease;
      }
      
      .chart-container:hover {
        box-shadow: 0 8px 30px rgba(0,0,0,0.15);
      }
      
      .chart-title {
        color: #2C3E50;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #f0f0f0;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      .chart-icon {
        color: #4CAF50;
        font-size: 1.2rem;
      }
      
      /* Tab Styles */
      .nav-tabs {
        border-bottom: 3px solid #f0f0f0;
        margin-bottom: 25px;
      }
      
      .nav-tabs .nav-link {
        border: none;
        border-radius: 10px 10px 0 0;
        padding: 15px 25px;
        font-weight: 600;
        color: #6C757D;
        transition: all 0.3s ease;
        margin-right: 5px;
      }
      
      .nav-tabs .nav-link:hover {
        background-color: #f8f9fa;
        color: #2E7D32;
      }
      
      .nav-tabs .nav-link.active {
        background: linear-gradient(135deg, #2E7D32 0%, #4CAF50 100%);
        color: white;
        border: none;
      }
      
      /* Table Styles */
      .dataTables_wrapper {
        font-family: "Inter", sans-serif;
      }
      
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        color: #6C757D;
        font-size: 0.9rem;
      }
      
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        border-radius: 8px;
        margin: 0 2px;
      }
      
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: #4CAF50 !important;
        border-color: #4CAF50 !important;
      }
      
      /* Loading Spinner */
      .spinner-border {
        color: #4CAF50;
      }
      
      /* Responsive Design */
      @media (max-width: 768px) {
        .header-title {
          font-size: 1.8rem;
        }
        
        .header-icon {
          font-size: 2rem;
        }
        
        .chart-container {
          padding: 15px;
          margin-bottom: 15px;
        }
        
        .value-box {
          margin-bottom: 15px;
        }
        
        .nav-tabs .nav-link {
          padding: 10px 15px;
          font-size: 0.9rem;
        }
      }
      
      /* Accessibility Improvements */
      .btn:focus, .form-control:focus, .form-select:focus {
        outline: 2px solid #4CAF50;
        outline-offset: 2px;
      }
      
      /* Animation Classes */
      .fade-in {
        animation: fadeIn 0.5s ease-in;
      }
      
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(20px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      /* Tooltip Styles */
      .info-tooltip {
        position: relative;
        cursor: help;
        color: #6C757D;
        margin-left: 8px;
      }
      
      .info-tooltip:hover::after {
        content: attr(data-tooltip);
        position: absolute;
        bottom: 125%;
        left: 50%;
        transform: translateX(-50%);
        background: #2C3E50;
        color: white;
        padding: 8px 12px;
        border-radius: 6px;
        font-size: 0.8rem;
        white-space: nowrap;
        z-index: 1000;
        box-shadow: 0 4px 12px rgba(0,0,0,0.2);
      }
      
      .info-tooltip:hover::before {
        content: "";
        position: absolute;
        bottom: 115%;
        left: 50%;
        transform: translateX(-50%);
        border: 5px solid transparent;
        border-top-color: #2C3E50;
        z-index: 1000;
      }
    '))
  ),
  
  # Enhanced Header
  div(class = "main-header",
    div(class = "container-fluid",
      div(class = "header-content",
        tags$i(class = "fas fa-seedling header-icon"),
        div(
          h1(class = "header-title", "ETH 2025 Seedling Distribution Dashboard"),
          p(class = "header-subtitle", "Comprehensive Analytics for Tree Planting Survey Data")
        )
      )
    )
  ),
  
  div(class = "container-fluid",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        class = "sidebar-panel",
        
        div(class = "filter-section",
          h4(class = "filter-title",
            tags$i(class = "fas fa-filter"),
            "Data Filters"
          ),
          p("Filter the data to focus your analysis on specific segments.", 
            style = "color: #6C757D; font-size: 0.9rem; margin-bottom: 20px;"),
          
          selectInput("purpose", 
                     label = tags$label(tags$i(class = "fas fa-bullseye", style = "margin-right: 8px;"), "Purpose:"),
                     choices = c("All"), 
                     selected = "All"),
          
          selectInput("cluster", 
                     label = tags$label(tags$i(class = "fas fa-map-marker-alt", style = "margin-right: 8px;"), "Cluster:"),
                     choices = c("All"), 
                     selected = "All")
        ),
        
        div(class = "options-section",
          h4(class = "filter-title",
            tags$i(class = "fas fa-cog"),
            "Display Options"
          ),
          
          div(class = "form-check",
            checkboxInput("show_percentages", 
                         label = tags$span(tags$i(class = "fas fa-percentage", style = "margin-right: 8px;"), "Show Percentages in Charts"), 
                         value = TRUE)
          ),
          
          hr(style = "margin: 20px 0; border-color: #e9ecef;"),
          
          div(class = "text-center",
            tags$i(class = "fas fa-lightbulb", style = "color: #FF8F00; margin-right: 8px;"),
            tags$span("Tip: Hover over charts for detailed information", 
                     style = "color: #6C757D; font-size: 0.85rem; font-style: italic;")
          )
        )
      ),
      
      mainPanel(
        width = 9,
        
        tabsetPanel(
          id = "main_tabs",
          type = "tabs",
          
          # Overview Tab
          tabPanel(
            title = tagList(tags$i(class = "fas fa-chart-pie"), "Overview"),
            value = "overview",
            
            div(class = "fade-in",
              # Value Boxes Row
              fluidRow(
                column(3, 
                  div(class = "value-box",
                    tags$i(class = "fas fa-users value-box-icon", style = "color: #2E7D32;"),
                    div(class = "value-box-value", textOutput("total_farmers", inline = TRUE)),
                    div(class = "value-box-subtitle", "Total Farmers")
                  )
                ),
                column(3,
                  div(class = "value-box",
                    tags$i(class = "fas fa-seedling value-box-icon", style = "color: #4CAF50;"),
                    div(class = "value-box-value", textOutput("total_seedlings", inline = TRUE)),
                    div(class = "value-box-subtitle", "Total Seedlings")
                  )
                ),
                column(3,
                  div(class = "value-box",
                    tags$i(class = "fas fa-chart-line value-box-icon", style = "color: #FF8F00;"),
                    div(class = "value-box-value", textOutput("avg_seedlings", inline = TRUE)),
                    div(class = "value-box-subtitle", "Avg. per Farmer")
                  )
                ),
                column(3,
                  div(class = "value-box",
                    tags$i(class = "fas fa-mobile-alt value-box-icon", style = "color: #1976D2;"),
                    div(class = "value-box-value", textOutput("phone_ownership", inline = TRUE)),
                    div(class = "value-box-subtitle", "Have Phones")
                  )
                )
              ),
              
              br(),
              
              # Charts Row
              fluidRow(
                column(6,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-tree chart-icon"),
                      "Tree Species Distribution",
                      tags$i(class = "fas fa-info-circle info-tooltip", 
                            `data-tooltip` = "Distribution of seedlings by tree species")
                    ),
                    withSpinner(plotlyOutput("donut_plot", height = "400px"), 
                               color = "#4CAF50", type = 4)
                  )
                ),
                column(6,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-bullseye chart-icon"),
                      "Purpose Analysis",
                      tags$i(class = "fas fa-info-circle info-tooltip", 
                            `data-tooltip` = "Comparison of farmers and seedlings by purpose")
                    ),
                    withSpinner(plotlyOutput("purpose_plot", height = "400px"), 
                               color = "#4CAF50", type = 4)
                  )
                )
              )
            )
          ),
          
          # Phone Ownership Tab
          tabPanel(
            title = tagList(tags$i(class = "fas fa-mobile-alt"), "Phone Ownership"),
            value = "phone",
            
            div(class = "fade-in",
              fluidRow(
                column(6,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-mobile-alt chart-icon"),
                      "Phone Ownership by Tree Species"
                    ),
                    withSpinner(plotlyOutput("phone_tree_species", height = "400px"), 
                               color = "#4CAF50", type = 4)
                  )
                ),
                column(6,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-table chart-icon"),
                      "Phone Owners per Species"
                    ),
                    withSpinner(DT::dataTableOutput("phone_owners_species_table"), 
                               color = "#4CAF50", type = 4)
                  )
                )
              ),
              
              br(),
              
              fluidRow(
                column(12,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-chart-bar chart-icon"),
                      "Phone Ownership Statistics by Woreda"
                    ),
                    withSpinner(DT::dataTableOutput("phone_stats_table"), 
                               color = "#4CAF50", type = 4)
                  )
                )
              )
            )
          ),
          
          # Geographic Analysis Tab
          tabPanel(
            title = tagList(tags$i(class = "fas fa-map-marked-alt"), "Geographic Analysis"),
            value = "geographic",
            
            div(class = "fade-in",
              fluidRow(
                column(6,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-users chart-icon"),
                      "Farmers by Cluster"
                    ),
                    withSpinner(plotlyOutput("cluster_farmers", height = "400px"), 
                               color = "#4CAF50", type = 4)
                  )
                ),
                column(6,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-seedling chart-icon"),
                      "Average Seedlings by Cluster"
                    ),
                    withSpinner(plotlyOutput("cluster_seedlings", height = "400px"), 
                               color = "#4CAF50", type = 4)
                  )
                )
              ),
              
              br(),
              
              fluidRow(
                column(12,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-map chart-icon"),
                      "Distribution by Woreda"
                    ),
                    withSpinner(plotlyOutput("woreda_plot", height = "400px"), 
                               color = "#4CAF50", type = 4)
                  )
                )
              )
            )
          ),
          
          # Demographics Tab
          tabPanel(
            title = tagList(tags$i(class = "fas fa-users"), "Demographics"),
            value = "demographics",
            
            div(class = "fade-in",
              fluidRow(
                column(6,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-birthday-cake chart-icon"),
                      "Age Distribution"
                    ),
                    withSpinner(plotlyOutput("age_plot", height = "400px"), 
                               color = "#4CAF50", type = 4)
                  )
                ),
                column(6,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-venus-mars chart-icon"),
                      "Gender Distribution"
                    ),
                    withSpinner(plotlyOutput("gender_plot", height = "400px"), 
                               color = "#4CAF50", type = 4)
                  )
                )
              ),
              
              br(),
              
              fluidRow(
                column(6,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-redo chart-icon"),
                      "Repeat Customers"
                    ),
                    withSpinner(plotlyOutput("repeat_plot", height = "400px"), 
                               color = "#4CAF50", type = 4)
                  )
                ),
                column(6,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-child chart-icon"),
                      "Youth Participation"
                    ),
                    withSpinner(plotlyOutput("youth_plot", height = "400px"), 
                               color = "#4CAF50", type = 4)
                  )
                )
              )
            )
          ),
          
          # Tree Species Analysis Tab
          tabPanel(
            title = tagList(tags$i(class = "fas fa-tree"), "Tree Species Analysis"),
            value = "tree_analysis",
            
            div(class = "fade-in",
              fluidRow(
                column(12,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-fire chart-icon"),
                      "Tree Species Popularity Heatmap"
                    ),
                    withSpinner(plotlyOutput("tree_cluster_heatmap", height = "500px"), 
                               color = "#4CAF50", type = 4)
                  )
                )
              ),
              
              br(),
              
              fluidRow(
                column(12,
                  div(class = "chart-container",
                    h4(class = "chart-title",
                      tags$i(class = "fas fa-chart-bar chart-icon"),
                      "Tree Species Statistics"
                    ),
                    withSpinner(DT::dataTableOutput("tree_stats_table"), 
                               color = "#4CAF50", type = 4)
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# ---- Enhanced Server with Complete Functions ----
server <- function(input, output, session) {
  
  # Reactive value to hold the main dataframe
  data_rv <- reactiveVal(data.frame())
  
  # ---- Initial Data Load and Filter Update ----
  observeEvent(TRUE, {
    showNotification("Loading data from Google Sheets...", type = "message", duration = 3)
    
    tryCatch({
      raw <- read_sheet(sheet_url, sheet = sheet_name)
      
      num_cols <- c("age", "male_youth_16_35_yrs", "female_youth_16_35_yrs", "gesho", "gesho_price", "grevillea", "grevillea_price", "decurrens", "decurrens_price", "wanza", "wanza_price", "papaya", "papaya_price", "moringa", "moringa_price", "coffee", "coffee_price", "guava", "guava_price", "lemon", "lemon_price")
      
      clean <- raw %>%
        clean_names() %>%
        select(
          cluster, woreda, kebele, purpose_for_taking_t_seedling_check_one, name_of_farmer, 
          sex_m_f, age, male_youth_16_35_yrs, female_youth_16_35_yrs, 
          is_this_a_repeat_customer_yes_no, total_of_gesho_seedlings, gesho_price, 
          grevillea_price, total_of_grevillea_seedlings, total_of_decurrens_seedlings, 
          decurrens_price, total_of_wanza_seedlings, wanza_price, total_of_papaya_seedlings, 
          papaya_price, total_of_moringa_seedlings, moringa_price, total_of_coffee_seedlings, 
          coffee_price, total_of_guava_seedlings, guava_price, total_of_lemon_seedlings, 
          lemon_price, mobile_number_if_any, female_youth_16_35_yrs_2
        ) %>%
        rename(
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
          lemon = total_of_lemon_seedlings,
          mobile_number = mobile_number_if_any,
          sex = sex_m_f
        ) %>%
        mutate(across(any_of(num_cols), as.numeric)) %>%
        mutate(
          total_seedling = rowSums(across(any_of(tree_types)), na.rm = TRUE),
          has_phone = !is.na(mobile_number) & mobile_number != ""
        )
      data_rv(clean) # Update reactive value on success
      
      # Update filter choices
      updateSelectInput(session, "purpose", choices = c("All", unique(clean$purpose)))
      updateSelectInput(session, "cluster", choices = c("All", unique(clean$cluster)))
      
    }, error = function(e) {
      message("Error during data loading: ", e$message)
      showNotification("Error loading data. Please check your Google Sheet and internet connection.", type = "error", duration = NULL)
    })
  }, once = TRUE)
  
  # ---- Reactive filtered data ----
  filtered_df <- reactive({
    data <- data_rv()
    if (nrow(data) == 0) return(data)
    if (input$purpose != "All") {
      data <- data %>% filter(purpose == input$purpose)
    }
    if (input$cluster != "All") {
      data <- data %>% filter(cluster == input$cluster)
    }
    return(data)
  })
  
  # ---- Value Box Outputs ----
  output$total_farmers <- renderText({
    prettyNum(nrow(filtered_df()), big.mark = ",")
  })
  
  output$total_seedlings <- renderText({
    prettyNum(sum(filtered_df()$total_seedling, na.rm = TRUE), big.mark = ",")
  })
  
  output$avg_seedlings <- renderText({
    round(mean(filtered_df()$total_seedling, na.rm = TRUE), 1)
  })
  
  output$phone_ownership <- renderText({
    paste0(round(mean(filtered_df()$has_phone, na.rm = TRUE) * 100, 1), "%")
  })
  
  # ---- Enhanced Donut Chart ----
  output$donut_plot <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0) return(plotly_empty())
    
    tree_data <- data %>%
      select(all_of(tree_types)) %>%
      pivot_longer(cols = everything(), names_to = "tree_type", values_to = "count") %>%
      group_by(tree_type) %>%
      summarise(total_count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
      filter(total_count > 0) %>%
      mutate(percentage = round((total_count / sum(total_count)) * 100, 1))
    
    p <- plot_ly(tree_data, 
                 labels = ~str_to_title(tree_type), 
                 values = ~total_count,
                 type = 'pie',
                 hole = 0.4,
                 textinfo = if(input$show_percentages) 'label+percent' else 'label+value',
                 hoverinfo = 'label+value+percent',
                 textposition = 'outside',
                 marker = list(colors = nature_colors,
                              line = list(color = '#FFFFFF', width = 3))) %>%
      layout(
        title = list(text = "", font = list(size = 16, color = primary_green)),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.02, y = 0.5, font = list(size = 12)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(family = "Inter, sans-serif")
      )
    
    return(p)
  })
  
  # ---- Enhanced Purpose Plot ----
  output$purpose_plot <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0) return(plotly_empty())
    
    purpose_data <- data %>%
      group_by(purpose) %>%
      summarise(
        farmers = n(),
        seedlings = sum(total_seedling, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        farmers_pct = round((farmers / sum(farmers)) * 100, 1),
        seedlings_pct = round((seedlings / sum(seedlings)) * 100, 1)
      )
    
    p <- plot_ly(purpose_data, x = ~purpose, y = ~farmers_pct,
                type = 'bar', name = 'Farmers (%)',
                marker = list(color = primary_green, 
                             line = list(color = 'white', width = 1)),
                text = ~paste0(farmers_pct, "%"),
                hovertemplate = '<b>%{x}</b><br>Farmers: %{y}%<extra></extra>',
                textposition = 'outside') %>%
      add_trace(y = ~seedlings_pct, name = 'Seedlings (%)',
                marker = list(color = accent_orange,
                             line = list(color = 'white', width = 1)),
                text = ~paste0(seedlings_pct, "%"), 
                textposition = 'outside',
                hovertemplate = '<b>%{x}</b><br>Seedlings: %{y}%<extra></extra>') %>%
      layout(
        title = list(text = "", font = list(size = 16, color = primary_green)),
        xaxis = list(title = "Purpose", titlefont = list(color = primary_green)),
        yaxis = list(title = "Percentage", titlefont = list(color = primary_green)),
        barmode = 'group',
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        legend = list(font = list(size = 12)),
        font = list(family = "Inter, sans-serif")
      )
    
    return(p)
  })
  
  # ---- Phone ownership by tree species ----
  output$phone_tree_species <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0) return(plotly_empty())
    
    tree_phone_data <- data %>%
      select(has_phone, all_of(tree_types)) %>%
      pivot_longer(cols = all_of(tree_types), names_to = "tree_type", values_to = "count") %>%
      filter(count > 0) %>%
      group_by(tree_type, has_phone) %>%
      summarise(n = n(), .groups = 'drop') %>%
      pivot_wider(names_from = has_phone, values_from = n, values_fill = 0, names_prefix = "phone_") %>%
      rename(has_phone = phone_TRUE, no_phone = phone_FALSE) %>%
      mutate(total = has_phone + no_phone,
             percent_owners = ifelse(total > 0, round((has_phone / total) * 100, 1), 0))

    p <- plot_ly(tree_phone_data, 
                x = ~str_to_title(tree_type), 
                y = ~percent_owners,
                type = 'bar', 
                marker = list(color = primary_green,
                             line = list(color = 'white', width = 1)),
                text = ~paste0(percent_owners, "%"), 
                hovertemplate = '<b>%{x}</b><br>Phone Ownership: %{y}%<extra></extra>',
                textposition = 'outside') %>%
      layout(
        title = list(text = "", font = list(size = 16, color = primary_green)),
        xaxis = list(title = "Tree Species", titlefont = list(color = primary_green)),
        yaxis = list(title = "% of Phone Owners", titlefont = list(color = primary_green)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(family = "Inter, sans-serif")
      )
    
    return(p)
  })

  # ---- Phone owners table ----
  output$phone_owners_species_table <- DT::renderDataTable({
    data <- filtered_df()
    if (nrow(data) == 0) return(datatable(data.frame()))
    
    phone_species <- data %>%
      filter(has_phone) %>%
      select(all_of(tree_types)) %>%
      pivot_longer(cols = everything(), names_to = "Tree Species", values_to = "count") %>%
      filter(count > 0) %>%
      group_by(`Tree Species`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = round((Count / sum(Count)) * 100, 1),
             `Tree Species` = str_to_title(`Tree Species`)) %>%
      arrange(desc(Count))
    
    datatable(phone_species, 
              options = list(
                pageLength = 10, 
                scrollX = TRUE, 
                dom = 'tp',
                columnDefs = list(list(className = 'dt-center', targets = 1:2))
              ),
              rownames = FALSE) %>%
      formatStyle('Count', 
                  background = styleColorBar(range(phone_species$Count, na.rm=TRUE), primary_green),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = 'white')
  })
  
  # ---- Phone stats table ----
  output$phone_stats_table <- DT::renderDataTable({
    data <- filtered_df()
    if (nrow(data) == 0) return(datatable(data.frame()))
    
    phone_stats <- data %>%
      group_by(woreda) %>%
      summarise(
        `Total Farmers` = n(),
        `Phone Owners` = sum(has_phone, na.rm = TRUE),
        `Phone Ownership %` = round(mean(has_phone, na.rm = TRUE) * 100, 1),
        `Avg Seedlings` = round(mean(total_seedling, na.rm = TRUE), 1),
        .groups = 'drop'
      ) %>%
      arrange(desc(`Phone Ownership %`))
    
    datatable(phone_stats, 
              options = list(
                pageLength = 10, 
                scrollX = TRUE, 
                dom = 'tp',
                columnDefs = list(list(className = 'dt-center', targets = 1:4))
              ),
              rownames = FALSE) %>%
      formatStyle('Phone Ownership %', 
                  background = styleColorBar(range(phone_stats$`Phone Ownership %`, na.rm=TRUE), accent_blue),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = 'white')
  })
  
  # ---- Cluster farmers plot ----
  output$cluster_farmers <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0) return(plotly_empty())
    
    cluster_data <- data %>%
      group_by(cluster) %>%
      summarise(farmers = n(), .groups = 'drop') %>%
      arrange(desc(farmers))
    
    p <- plot_ly(cluster_data, 
                x = ~reorder(cluster, farmers), 
                y = ~farmers,
                type = 'bar',
                marker = list(color = secondary_green,
                             line = list(color = 'white', width = 1)),
                text = ~farmers,
                hovertemplate = '<b>%{x}</b><br>Farmers: %{y}<extra></extra>',
                textposition = 'outside') %>%
      layout(
        title = list(text = "", font = list(size = 16, color = primary_green)),
        xaxis = list(title = "Cluster", titlefont = list(color = primary_green)),
        yaxis = list(title = "Number of Farmers", titlefont = list(color = primary_green)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(family = "Inter, sans-serif")
      )
    
    return(p)
  })
  
  # ---- Cluster seedlings plot ----
  output$cluster_seedlings <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0) return(plotly_empty())
    
    cluster_seedlings <- data %>%
      group_by(cluster) %>%
      summarise(avg_seedlings = round(mean(total_seedling, na.rm = TRUE), 1), .groups = 'drop') %>%
      arrange(desc(avg_seedlings))
    
    p <- plot_ly(cluster_seedlings, 
                x = ~reorder(cluster, avg_seedlings), 
                y = ~avg_seedlings,
                type = 'bar',
                marker = list(color = accent_orange,
                             line = list(color = 'white', width = 1)),
                text = ~avg_seedlings,
                hovertemplate = '<b>%{x}</b><br>Avg Seedlings: %{y}<extra></extra>',
                textposition = 'outside') %>%
      layout(
        title = list(text = "", font = list(size = 16, color = primary_green)),
        xaxis = list(title = "Cluster", titlefont = list(color = primary_green)),
        yaxis = list(title = "Average Seedlings per Farmer", titlefont = list(color = primary_green)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(family = "Inter, sans-serif")
      )
    
    return(p)
  })
  
  # ---- Woreda plot ----
  output$woreda_plot <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0) return(plotly_empty())
    
    woreda_data <- data %>%
      group_by(woreda) %>%
      summarise(
        farmers = n(),
        seedlings = sum(total_seedling, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(farmers))
    
    p <- plot_ly(woreda_data, x = ~reorder(woreda, farmers), y = ~farmers,
                type = 'bar', name = 'Farmers',
                marker = list(color = primary_green,
                             line = list(color = 'white', width = 1)),
                hovertemplate = '<b>%{x}</b><br>Farmers: %{y}<extra></extra>') %>%
      add_trace(y = ~seedlings, name = 'Seedlings',
                marker = list(color = accent_orange,
                             line = list(color = 'white', width = 1)),
                hovertemplate = '<b>%{x}</b><br>Seedlings: %{y}<extra></extra>',
                yaxis = 'y2') %>%
      layout(
        title = list(text = "", font = list(size = 16, color = primary_green)),
        xaxis = list(title = "Woreda", titlefont = list(color = primary_green)),
        yaxis = list(title = "Number of Farmers", titlefont = list(color = primary_green)),
        yaxis2 = list(title = "Total Seedlings", overlaying = 'y', side = 'right',
                     titlefont = list(color = accent_orange)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        legend = list(font = list(size = 12)),
        font = list(family = "Inter, sans-serif")
      )
    
    return(p)
  })
  
  # ---- Age plot ----
  output$age_plot <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0) return(plotly_empty())
    
    age_data <- data %>%
      filter(!is.na(age)) %>%
      mutate(age_group = case_when(
        age < 25 ~ "Under 25",
        age >= 25 & age < 35 ~ "25-34",
        age >= 35 & age < 45 ~ "35-44",
        age >= 45 & age < 55 ~ "45-54",
        age >= 55 ~ "55+"
      )) %>%
      group_by(age_group) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = round((count / sum(count)) * 100, 1))
    
    p <- plot_ly(age_data, 
                x = ~age_group, 
                y = ~count,
                type = 'bar',
                marker = list(color = nature_colors[1:nrow(age_data)],
                             line = list(color = 'white', width = 1)),
                text = ~paste0(count, " (", percentage, "%)"),
                hovertemplate = '<b>%{x}</b><br>Count: %{y}<br>Percentage: %{text}<extra></extra>',
                textposition = 'outside') %>%
      layout(
        title = list(text = "", font = list(size = 16, color = primary_green)),
        xaxis = list(title = "Age Group", titlefont = list(color = primary_green)),
        yaxis = list(title = "Number of Farmers", titlefont = list(color = primary_green)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(family = "Inter, sans-serif")
      )
    
    return(p)
  })
  
  # ---- Gender plot ----
  output$gender_plot <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0) return(plotly_empty())
    
    gender_data <- data %>%
      filter(!is.na(sex)) %>%
      group_by(sex) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = round((count / sum(count)) * 100, 1))
    
    p <- plot_ly(gender_data, 
                labels = ~sex, 
                values = ~count,
                type = 'pie', 
                textinfo = 'label+percent',
                hovertemplate = '<b>%{label}</b><br>Count: %{value}<br>Percentage: %{percent}<extra></extra>',
                marker = list(colors = c(primary_green, accent_orange),
                             line = list(color = 'white', width = 3))) %>%
      layout(
        title = list(text = "", font = list(size = 16, color = primary_green)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        legend = list(font = list(size = 12)),
        font = list(family = "Inter, sans-serif")
      )
    
    return(p)
  })
  
  # ---- Repeat customers plot ----
  output$repeat_plot <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0) return(plotly_empty())
    
    repeat_data <- data %>%
      filter(!is.na(repeat_customer)) %>%
      group_by(repeat_customer) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = round((count / sum(count)) * 100, 1))
    
    p <- plot_ly(repeat_data, 
                labels = ~repeat_customer, 
                values = ~count,
                type = 'pie', 
                textinfo = 'label+percent',
                hovertemplate = '<b>%{label}</b><br>Count: %{value}<br>Percentage: %{percent}<extra></extra>',
                marker = list(colors = c(accent_blue, warm_red),
                             line = list(color = 'white', width = 3))) %>%
      layout(
        title = list(text = "", font = list(size = 16, color = primary_green)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        legend = list(font = list(size = 12)),
        font = list(family = "Inter, sans-serif")
      )
    
    return(p)
  })
  
  # ---- Youth participation plot ----
  output$youth_plot <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0) return(plotly_empty())
    
    youth_data <- data %>%
      summarise(
        `Male Youth` = sum(male_youth_16_35_yrs, na.rm = TRUE),
        `Female Youth` = sum(female_youth_16_35_yrs, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = everything(), names_to = "Gender", values_to = "Count")
    
    p <- plot_ly(youth_data, 
                x = ~Gender, 
                y = ~Count,
                type = 'bar',
                marker = list(color = c(accent_blue, accent_purple),
                             line = list(color = 'white', width = 1)),
                text = ~Count,
                hovertemplate = '<b>%{x}</b><br>Count: %{y}<extra></extra>',
                textposition = 'outside') %>%
      layout(
        title = list(text = "", font = list(size = 16, color = primary_green)),
        xaxis = list(title = "Youth Category", titlefont = list(color = primary_green)),
        yaxis = list(title = "Number of Youth", titlefont = list(color = primary_green)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(family = "Inter, sans-serif")
      )
    
    return(p)
  })
  
  # ---- Tree species heatmap ----
  output$tree_cluster_heatmap <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0) return(plotly_empty())
    
    heatmap_data <- data %>%
      select(cluster, all_of(tree_types)) %>%
      pivot_longer(cols = all_of(tree_types), names_to = "tree_type", values_to = "count") %>%
      group_by(cluster, tree_type) %>%
      summarise(total_count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = tree_type, values_from = total_count, values_fill = 0)
    
    heatmap_matrix <- as.matrix(heatmap_data[, -1])
    rownames(heatmap_matrix) <- heatmap_data$cluster
    
    p <- plot_ly(z = ~heatmap_matrix, 
                type = "heatmap",
                x = str_to_title(colnames(heatmap_matrix)),
                y = rownames(heatmap_matrix),
                colorscale = list(c(0, light_green), c(0.5, secondary_green), c(1, primary_green)),
                hovertemplate = '<b>%{y} - %{x}</b><br>Count: %{z}<extra></extra>') %>%
      layout(
        title = list(text = "", font = list(size = 16, color = primary_green)),
        xaxis = list(title = "Tree Type", titlefont = list(color = primary_green)),
        yaxis = list(title = "Cluster", titlefont = list(color = primary_green)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(family = "Inter, sans-serif")
      )
    
    return(p)
  })
  
  # ---- Tree statistics table ----
  output$tree_stats_table <- DT::renderDataTable({
    data <- filtered_df()
    if (nrow(data) == 0) return(datatable(data.frame()))
    
    tree_stats <- data %>%
      select(all_of(tree_types)) %>%
      pivot_longer(cols = everything(), names_to = "Tree Type", values_to = "count") %>%
      group_by(`Tree Type`) %>%
      summarise(
        `Total Seedlings` = sum(count, na.rm = TRUE),
        `Farmers Buying` = sum(count > 0, na.rm = TRUE),
        `Avg per Farmer` = round(mean(count[count > 0], na.rm = TRUE), 1),
        `Max Purchase` = max(count, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(`Tree Type` = str_to_title(`Tree Type`)) %>%
      arrange(desc(`Total Seedlings`))
    
    datatable(tree_stats, 
              options = list(
                pageLength = 10, 
                scrollX = TRUE, 
                dom = 'tp',
                columnDefs = list(list(className = 'dt-center', targets = 1:4))
              ),
              rownames = FALSE) %>%
      formatStyle('Total Seedlings', 
                  background = styleColorBar(range(tree_stats$`Total Seedlings`, na.rm=TRUE), primary_green),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = 'white') %>%
      formatStyle('Avg per Farmer', 
                  background = styleColorBar(range(tree_stats$`Avg per Farmer`, na.rm=TRUE), accent_orange),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = 'white')
  })
}

# ---- Run the application ----
shinyApp(ui = ui, server = server)