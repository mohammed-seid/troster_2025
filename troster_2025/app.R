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
library(shinycssloaders) # For loading spinners

# Google Sheet URL and sheet name
sheet_url <- "https://docs.google.com/spreadsheets/d/1VHn2zFbGbpmB-inzJZUeNfrTwEJ--pBB5MjRmxPAVT0/edit?gid=15730184#gid=15730184"
sheet_name <- "Farmer Detail"


# ---- Color Palette and Tree Types ----
primary_green   <- "#388E3C"
secondary_green <- "#66BB6A"
light_green     <- "#C8E6C9"
accent_orange   <- "#FFA726"
nature_colors   <- c("#388E3C", "#66BB6A", "#FFA726", "#42A5F5", "#8D6E63", "#C8E6C9", "#FFD54F", "#8BC34A", "#FF7043", "#26A69A")
tree_types <- c("gesho", "grevillea", "decurrens", "wanza", "papaya", "moringa", "coffee", "guava", "lemon")

# ---- Data Loading and Cleaning ----
# Read and clean data, with type validation and error handling
df <- tryCatch({
  raw <- read_sheet(sheet_url, sheet = sheet_name)
  clean <- raw %>%
    clean_names() %>%
    select(cluster, woreda, kebele, purpose_for_taking_t_seedling_check_one, name_of_farmer, sex_m_f, age, male_youth_16_35_yrs, female_youth_16_35_yrs, is_this_a_repeat_customer_yes_no, total_of_gesho_seedlings, gesho_price, grevillea_price, total_of_grevillea_seedlings, total_of_decurrens_seedlings, decurrens_price, total_of_wanza_seedlings, wanza_price, total_of_papaya_seedlings, papaya_price, total_of_moringa_seedlings, moringa_price, total_of_coffee_seedlings, coffee_price, total_of_guava_seedlings, guava_price, total_of_lmon_seedlings, lemon_price, mobile_number_if_any, female_youth_16_35_yrs_2) %>%
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
      lemon = total_of_lmon_seedlings,
      mobile_number = mobile_number_if_any,
      sex = sex_m_f
    )
  # Ensure numeric columns are numeric
  num_cols <- c("age", "male_youth_16_35_yrs", "female_youth_16_35_yrs", "gesho", "gesho_price", "grevillea", "grevillea_price", "decurrens", "decurrens_price", "wanza", "wanza_price", "papaya", "papaya_price", "moringa", "moringa_price", "coffee", "coffee_price", "guava", "guava_price", "lemon", "lemon_price")
  for (col in num_cols) {
    if (col %in% names(clean)) clean[[col]] <- as.numeric(clean[[col]])
  }
  # Add total_seedling column
  clean$total_seedling <- rowSums(clean[, tree_types], na.rm = TRUE)
  # Add has_phone column
  clean$has_phone <- !is.na(clean$mobile_number) & clean$mobile_number != ""
  clean
}, error = function(e) {
  showNotification("Error loading data. Please check your Google Sheet and internet connection.", type = "error")
  data.frame()
})
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML('
      .btn-primary {
        background: linear-gradient(135deg, #4CAF50 0%, #2E7D32 100%);
        border: none;
        border-radius: 8px;
        transition: all 0.3s ease;
      }
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(76, 175, 80, 0.4);
      }
      @media (max-width: 768px) {
        .title-panel { text-align: center; }
        .chart-container { margin-bottom: 15px; }
      }
    ')),
  ),
  # Enhanced Title Panel
  div(class = "title-panel",
    fluidRow(
      column(12,
        div(style = "display: flex; align-items: center; justify-content: center;",
          tags$img(src = "https://asset.brandfetch.io/id20mQhr9M/id4I_2sY1G.png", 
                   height = "60px", 
                   style = "margin-right: 20px; filter: brightness(0) invert(1);"),
          h1("🌱 ETH 2025 Seedling Distribution Dashboard", 
             style = "margin: 0; font-size: 2.2em;")
        )
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      wellPanel(
        h4("🎯 Data Filters"),
        helpText("Filter the data by purpose and cluster to focus your analysis."),
        selectInput("purpose", "Select Purpose:", 
                    choices = c("All", unique(df$purpose)), 
                    selected = "All"),
        selectInput("cluster", "Select Cluster:", 
                    choices = c("All", unique(df$cluster)), 
                    selected = "All")
      ),
      wellPanel(
        h4("⚙️ Display Options"),
        checkboxInput("show_percentages", "Show Percentages in Plots", value = TRUE),
        hr(),
        div(style = "text-align: center; color: #666; font-size: 0.9em;",
          "💡 Tip: Hover over charts for detailed information")
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        # Overview Tab
        tabPanel("📊 Overview",
          fluidRow(
            valueBoxOutput("total_farmers_box", width = 3),
            valueBoxOutput("total_seedlings_box", width = 3),
            valueBoxOutput("avg_seedlings_box", width = 3),
            valueBoxOutput("phone_ownership_box", width = 3)
          ),
          div(class = "section-divider"),
          fluidRow(
            column(6,
              div(class = "chart-container",
                tags$div(style = "position: relative;",
                  h4(HTML("🌳 Tree Species Distribution <i class='fa fa-info-circle info-icon'></i><span class='custom-tooltip'>Shows the proportion of each tree species distributed to farmers. Hover over the chart for details.</span>")),
                  withSpinner(plotlyOutput("donut_plot", height = "400px"), color = "#4CAF50")
                )
              )
            ),
            column(6,
              div(class = "chart-container",
                tags$div(style = "position: relative;",
                  h4(HTML("🎯 Purpose Distribution <i class='fa fa-info-circle info-icon'></i><span class='custom-tooltip'>Compares the percentage of farmers and seedlings for each purpose. Both metrics are shown as grouped bars.</span>")),
                  withSpinner(plotlyOutput("purpose_plot", height = "400px"), color = "#4CAF50")
                )
              )
            )
          )
        ),
        # Phone Ownership Analysis Tab
        tabPanel("📱 Phone Ownership",
          fluidRow(
            column(6,
              div(class = "chart-container",
                tags$div(style = "position: relative;",
                  h4(HTML("📱 Phone Ownership by Tree Species <i class='fa fa-info-circle info-icon'></i><span class='custom-tooltip'>Shows the percentage of phone owners among buyers of each tree species.</span>")),
                  withSpinner(plotlyOutput("phone_tree_species", height = "400px"), color = "#4CAF50")
                )
              )
            ),
            column(6,
              div(class = "chart-container",
                tags$div(style = "position: relative;",
                  h4(HTML("📊 Phone Owners per Species <i class='fa fa-info-circle info-icon'></i><span class='custom-tooltip'>Table of the number of phone owners for each tree species.</span>")),
                  withSpinner(DT::dataTableOutput("phone_owners_species_table"), color = "#4CAF50")
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(12,
              div(class = "chart-container",
                tags$div(style = "position: relative;",
                  h4(HTML("📈 Phone Ownership Statistics by Woreda <i class='fa fa-info-circle info-icon'></i><span class='custom-tooltip'>Summary of phone ownership and average seedlings by woreda.</span>")),
                  withSpinner(DT::dataTableOutput("phone_stats_table"), color = "#4CAF50")
                )
              )
            )
          )
        ),
        # Geographic Analysis Tab
        tabPanel("🗺️ Geographic Analysis",
          fluidRow(
            column(6,
              div(class = "chart-container",
                h4("👥 Farmers by Cluster"),
                withSpinner(plotlyOutput("cluster_farmers", height = "400px"), color = "#4CAF50")
              )
            ),
            column(6,
              div(class = "chart-container",
                h4("🌱 Average Seedlings by Cluster"),
                withSpinner(plotlyOutput("cluster_seedlings", height = "400px"), color = "#4CAF50")
              )
            )
          ),
          br(),
          fluidRow(
            column(12,
              div(class = "chart-container",
                h4("🏘️ Distribution by Woreda"),
                withSpinner(plotlyOutput("woreda_plot", height = "400px"), color = "#4CAF50")
              )
            )
          )
        ),
        # Demographics Tab
        tabPanel("👥 Demographics",
          fluidRow(
            column(6,
              div(class = "chart-container",
                h4("📅 Age Distribution"),
                withSpinner(plotlyOutput("age_plot", height = "400px"), color = "#4CAF50")
              )
            ),
            column(6,
              div(class = "chart-container",
                h4("⚧️ Gender Distribution"),
                withSpinner(plotlyOutput("gender_plot", height = "400px"), color = "#4CAF50")
              )
            )
          ),
          br(),
          fluidRow(
            column(6,
              div(class = "chart-container",
                h4("🔄 Repeat Customers"),
                withSpinner(plotlyOutput("repeat_plot", height = "400px"), color = "#4CAF50")
              )
            ),
            column(6,
              div(class = "chart-container",
                h4("👨‍👩‍👧‍👦 Youth Participation"),
                withSpinner(plotlyOutput("youth_plot", height = "400px"), color = "#4CAF50")
              )
            )
          )
        ),
        # Tree Species Analysis Tab
        tabPanel("🌳 Tree Species Analysis",
          fluidRow(
            column(12,
              div(class = "chart-container",
                h4("🔥 Tree Species Popularity Heatmap"),
                withSpinner(plotlyOutput("tree_cluster_heatmap", height = "500px"), color = "#4CAF50")
              )
            )
          ),
          br(),
          fluidRow(
            column(12,
              div(class = "chart-container",
                h4("📈 Tree Species Statistics"),
                withSpinner(DT::dataTableOutput("tree_stats_table"), color = "#4CAF50")
              )
            )
          )
        )
      )
    )
  )
)

# Enhanced Server with Fixed Functions
server <- function(input, output, session) {
  
  # ---- Reactive filtered data ----
  filtered_df <- reactive({
    data <- df
    if (nrow(data) == 0) return(data)
    if (input$purpose != "All") {
      data <- data %>% filter(purpose == input$purpose)
    }
    if (input$cluster != "All") {
      data <- data %>% filter(cluster == input$cluster)
    }
    return(data)
  })

  # ---- Enhanced Value Boxes ----
  output$total_farmers_box <- renderValueBox({
    valueBox(
      value = tags$div(class = "animated-value", prettyNum(nrow(filtered_df()), big.mark = ",")),
      subtitle = tags$span("Total Farmers", class = "subtitle"),
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$total_seedlings_box <- renderValueBox({
    valueBox(
      value = tags$div(class = "animated-value", prettyNum(sum(filtered_df()$total_seedling, na.rm = TRUE), big.mark = ",")),
      subtitle = tags$span("Total Seedlings", class = "subtitle"),
      icon = icon("tree"),
      color = "yellow"
    )
  })

  output$avg_seedlings_box <- renderValueBox({
    avg_seedlings <- mean(filtered_df()$total_seedling, na.rm = TRUE)
    valueBox(
      value = tags$div(class = "animated-value", round(avg_seedlings, 1)),
      subtitle = tags$span("Avg. Seedlings per Farmer", class = "subtitle"),
      icon = icon("seedling"),
      color = "orange"
    )
  })
  
  output$phone_ownership_box <- renderValueBox({
    phone_pct <- mean(filtered_df()$has_phone, na.rm = TRUE) * 100
    valueBox(
      value = tagList(
        tags$div(class = "animated-value", paste0(round(phone_pct, 1), "%")),
        tags$div(class = "progress-bar-bg",
          tags$div(class = "progress-bar-fill", style = paste0("width: ", round(phone_pct, 1), "%"))
        )
      ),
      subtitle = tags$span("Have a Phone", class = "subtitle"),
      icon = icon("mobile-alt"),
      color = "purple"
    )
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
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
    
    return(p)
  })
  
  # Enhanced Purpose Plot
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
        legend = list(font = list(size = 12))
      )
    
    return(p)
  })

    
  # Phone ownership by tree species
  output$phone_tree_species <- renderPlotly({
    tree_phone_data <- filtered_df() %>%
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
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
    
    return(p)
  })

  # Phone owners table with enhanced styling
  output$phone_owners_species_table <- DT::renderDataTable({
    data <- filtered_df()
    phone_species <- data %>%
      filter(has_phone) %>%
      select(all_of(tree_types)) %>%
      pivot_longer(cols = everything(), names_to = "Tree Species", values_to = "count") %>%
      filter(count > 0) %>%
      group_by(`Tree Species`) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = round((count / sum(count)) * 100, 1))
    datatable(phone_species, 
              options = list(
                pageLength = 10, 
                scrollX = TRUE, 
                dom = 'tp',
                columnDefs = list(list(className = 'dt-center', targets = 1:2))
              ),
              rownames = FALSE) %>%
      formatStyle('count', 
                  background = styleColorBar(range(phone_species$count, na.rm=TRUE), primary_green),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = 'white')
  })
  

  output$gender_plot <- renderPlotly({
    gender_data <- filtered_df() %>%
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
        legend = list(font = list(size = 12))
      )
    return(p)
  })

  # Tree species heatmap
  output$tree_cluster_heatmap <- renderPlotly({
    heatmap_data <- filtered_df() %>%
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
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
    
    return(p)
  })
  
  # Tree statistics table
  output$tree_stats_table <- DT::renderDataTable({
    tree_stats <- filtered_df() %>%
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