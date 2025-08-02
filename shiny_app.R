# Bundesliga Player Statistics Spider Chart Shiny App
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(fmsb)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Bundesliga Player Statistics - Spider Charts"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Player Comparison", tabName = "comparison", icon = icon("chart-line")),
      menuItem("Data Table", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 10px;
        }
      "))
    ),
    
    tabItems(
      # Player Comparison Tab
      tabItem(tabName = "comparison",
        fluidRow(
          box(
            title = "Player Selection & Chart Configuration", 
            status = "primary", 
            solidHeader = TRUE,
            width = 4,
            height = "auto",
            
            # Player selection
            selectInput("player1", "Select Player 1:",
                       choices = NULL,
                       selected = NULL),
            
            selectInput("player2", "Select Player 2 (Optional):",
                       choices = NULL,
                       selected = NULL),
            
            selectInput("player3", "Select Player 3 (Optional):",
                       choices = NULL,
                       selected = NULL),
            
            # Position filter
            selectInput("position_filter", "Filter by Position:",
                       choices = c("All Positions" = "all"),
                       selected = "all"),
            
            # Squad filter
            selectInput("squad_filter", "Filter by Squad:",
                       choices = c("All Squads" = "all"),
                       selected = "all"),
            
            hr(),
            
            # Metric selection
            h4("Select Metrics for Spider Chart:"),
            
            h5("Attacking Metrics:"),
            checkboxInput("use_goals", "Goals (Gls)", value = TRUE),
            checkboxInput("use_assists", "Assists (Ast)", value = TRUE),
            checkboxInput("use_xg", "Expected Goals (xG)", value = TRUE),
            checkboxInput("use_xag", "Expected Assists (xAG)", value = TRUE),
            
            h5("Playing Time & Discipline:"),
            checkboxInput("use_min90s", "Minutes per 90 (Min90s)", value = TRUE),
            checkboxInput("use_starts", "Starts", value = FALSE),
            checkboxInput("use_cards", "Yellow Cards (CrdY)", value = FALSE),
            
            h5("Advanced Metrics:"),
            checkboxInput("use_prgc", "Progressive Carries (PrgC)", value = TRUE),
            checkboxInput("use_prgp", "Progressive Passes (PrgP)", value = TRUE),
            checkboxInput("use_prgr", "Progressive Receptions (PrgR)", value = FALSE),
            
            hr(),
            
            # Chart customization
            h4("Chart Options:"),
            sliderInput("chart_alpha", "Chart Transparency:",
                       min = 0.1, max = 1, value = 0.6, step = 0.1),
            
            checkboxInput("show_grid", "Show Grid Lines", value = TRUE),
            
            actionButton("update_chart", "Update Chart", 
                        class = "btn-primary btn-block")
          ),
          
          box(
            title = "Player Statistics Spider Chart", 
            status = "success", 
            solidHeader = TRUE,
            width = 8,
            height = "600px",
            
            plotOutput("spider_chart", height = "550px")
          )
        ),
        
        fluidRow(
          box(
            title = "Player Statistics Summary", 
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            
            DT::dataTableOutput("player_summary")
          )
        )
      ),
      
      # Data Table Tab
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Bundesliga Player Statistics Dataset", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            DT::dataTableOutput("full_data_table")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive data loading
  bundesliga_data <- reactive({
    # You'll need to load your data here
    # For demonstration, creating sample structure
    # Replace this with: read.csv("your_bundesliga_data.csv")
    
    # Sample data structure - replace with actual data loading
    data.frame(
      Rk = 1:100,
      Player = paste("Player", 1:100),
      Nation = sample(c("GER", "FRA", "ESP", "BRA", "ARG"), 100, replace = TRUE),
      Pos = sample(c("GK", "DF", "MF", "FW"), 100, replace = TRUE),
      Squad = sample(c("Bayern Munich", "Borussia Dortmund", "RB Leipzig", "Bayer Leverkusen"), 100, replace = TRUE),
      Age = sample(18:35, 100, replace = TRUE),
      MP = sample(1:34, 100, replace = TRUE),
      Starts = sample(0:34, 100, replace = TRUE),
      Min90s = round(runif(100, 0, 34), 1),
      Gls = sample(0:30, 100, replace = TRUE),
      Ast = sample(0:20, 100, replace = TRUE),
      xG = round(runif(100, 0, 25), 2),
      xAG = round(runif(100, 0, 15), 2),
      CrdY = sample(0:15, 100, replace = TRUE),
      CrdR = sample(0:3, 100, replace = TRUE),
      PrgC = sample(0:200, 100, replace = TRUE),
      PrgP = sample(0:150, 100, replace = TRUE),
      PrgR = sample(0:100, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
  })
  
  # Update player choices based on filters
  observe({
    data <- bundesliga_data()
    
    # Filter data based on position and squad
    if(input$position_filter != "all") {
      data <- data[data$Pos == input$position_filter, ]
    }
    
    if(input$squad_filter != "all") {
      data <- data[data$Squad == input$squad_filter, ]
    }
    
    player_choices <- setNames(data$Player, paste(data$Player, "-", data$Squad, "(", data$Pos, ")"))
    
    updateSelectInput(session, "player1", choices = c("Select a player..." = "", player_choices))
    updateSelectInput(session, "player2", choices = c("None" = "", player_choices))
    updateSelectInput(session, "player3", choices = c("None" = "", player_choices))
  })
  
  # Update filter choices
  observe({
    data <- bundesliga_data()
    
    # Update position filter
    positions <- c("All Positions" = "all", unique(data$Pos))
    updateSelectInput(session, "position_filter", choices = positions)
    
    # Update squad filter
    squads <- c("All Squads" = "all", sort(unique(data$Squad)))
    updateSelectInput(session, "squad_filter", choices = squads)
  })
  
  # Prepare data for spider chart
  spider_data <- eventReactive(input$update_chart, {
    req(input$player1)
    
    data <- bundesliga_data()
    selected_players <- c(input$player1)
    
    if(input$player2 != "") selected_players <- c(selected_players, input$player2)
    if(input$player3 != "") selected_players <- c(selected_players, input$player3)
    
    # Get selected metrics
    selected_metrics <- c()
    if(input$use_goals) selected_metrics <- c(selected_metrics, "Gls")
    if(input$use_assists) selected_metrics <- c(selected_metrics, "Ast")
    if(input$use_xg) selected_metrics <- c(selected_metrics, "xG")
    if(input$use_xag) selected_metrics <- c(selected_metrics, "xAG")
    if(input$use_min90s) selected_metrics <- c(selected_metrics, "Min90s")
    if(input$use_starts) selected_metrics <- c(selected_metrics, "Starts")
    if(input$use_cards) selected_metrics <- c(selected_metrics, "CrdY")
    if(input$use_prgc) selected_metrics <- c(selected_metrics, "PrgC")
    if(input$use_prgp) selected_metrics <- c(selected_metrics, "PrgP")
    if(input$use_prgr) selected_metrics <- c(selected_metrics, "PrgR")
    
    if(length(selected_metrics) == 0) {
      return(NULL)
    }
    
    # Filter data for selected players
    player_data <- data[data$Player %in% selected_players, c("Player", selected_metrics)]
    
    # Normalize data to 0-100 scale for spider chart
    normalized_data <- player_data
    for(metric in selected_metrics) {
      max_val <- max(data[[metric]], na.rm = TRUE)
      min_val <- min(data[[metric]], na.rm = TRUE)
      if(max_val > min_val) {
        normalized_data[[metric]] <- ((player_data[[metric]] - min_val) / (max_val - min_val)) * 100
      } else {
        normalized_data[[metric]] <- 50  # If all values are the same
      }
    }
    
    # Prepare for fmsb radar chart (add max and min rows)
    chart_data <- rbind(
      rep(100, length(selected_metrics)),  # Max values
      rep(0, length(selected_metrics)),    # Min values
      normalized_data[, selected_metrics]
    )
    
    rownames(chart_data) <- c("Max", "Min", normalized_data$Player)
    
    list(
      chart_data = chart_data,
      player_names = normalized_data$Player,
      metrics = selected_metrics
    )
  }, ignoreNULL = FALSE)
  
  # Create spider chart
  output$spider_chart <- renderPlot({
    spider_info <- spider_data()
    
    if(is.null(spider_info)) {
      plot.new()
      text(0.5, 0.5, "Please select at least one metric and update the chart", 
           cex = 1.5, col = "gray")
      return()
    }
    
    # Colors for different players
    colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57")
    
    # Create the radar chart
    par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
    
    radarchart(
      spider_info$chart_data,
      axistype = if(input$show_grid) 1 else 0,
      pcol = colors[1:length(spider_info$player_names)],
      pfcol = scales::alpha(colors[1:length(spider_info$player_names)], input$chart_alpha),
      plwd = 3,
      plty = 1,
      cglcol = "grey",
      cglty = 1,
      axislabcol = "black",
      caxislabels = seq(0, 100, 25),
      cglwd = 0.8,
      vlcex = 1.2,
      title = "Player Statistics Comparison"
    )
    
    # Add legend
    legend(
      x = "topright",
      legend = spider_info$player_names,
      col = colors[1:length(spider_info$player_names)],
      lty = 1,
      lwd = 3,
      cex = 1.1,
      bty = "n"
    )
    
  })
  
  # Player summary table
  output$player_summary <- DT::renderDataTable({
    req(input$player1)
    
    data <- bundesliga_data()
    selected_players <- c(input$player1)
    
    if(input$player2 != "") selected_players <- c(selected_players, input$player2)
    if(input$player3 != "") selected_players <- c(selected_players, input$player3)
    
    summary_data <- data[data$Player %in% selected_players, 
                        c("Player", "Squad", "Pos", "Age", "MP", "Gls", "Ast", "xG", "xAG", "Min90s")]
    
    DT::datatable(
      summary_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't'
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("xG", "xAG", "Min90s"), digits = 2)
  })
  
  # Full data table
  output$full_data_table <- DT::renderDataTable({
    data <- bundesliga_data()
    
    DT::datatable(
      data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        searchHighlight = TRUE
      ),
      filter = 'top',
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("xG", "xAG", "Min90s"), digits = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# Instructions for use:
# 1. Install required packages: install.packages(c("shiny", "shinydashboard", "DT", "plotly", "dplyr", "fmsb", "scales"))
# 2. Replace the sample data creation with your actual data loading:
#    bundesliga_data <- read.csv("your_bundesliga_data.csv")
# 3. Run the app with: shiny::runApp()
# 4. The app will open in your default browser

# Features included:
# - Interactive player selection with filtering by position and squad
# - Customizable metric selection for spider charts
# - Support for comparing up to 3 players simultaneously
# - Normalized data visualization (0-100 scale)
# - Data table view of full dataset
# - Responsive design with professional styling
