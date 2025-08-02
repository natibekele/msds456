# Enhanced Bundesliga Player Statistics Spider Chart Shiny App
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(fmsb)
library(scales)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Bundesliga Player Statistics - Enhanced Spider Charts"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Player Comparison", tabName = "comparison", icon = icon("chart-line")),
      menuItem("Similar Players", tabName = "similar", icon = icon("users")),
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
        .similar-player-card {
          background: #fff;
          border: 1px solid #ddd;
          border-radius: 8px;
          padding: 15px;
          margin: 10px 0;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .similarity-score {
          background: #3c8dbc;
          color: white;
          padding: 5px 10px;
          border-radius: 15px;
          font-weight: bold;
          display: inline-block;
          margin-bottom: 10px;
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
            
            # Per-90 normalization toggle
            div(style = "background-color: #e8f4fd; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                h4("📊 Statistics Mode", style = "margin-top: 0;"),
                radioButtons("stats_mode", NULL,
                           choices = list(
                             "Per 90 Minutes (Recommended)" = "per90",
                             "Total Season Stats" = "total"
                           ),
                           selected = "per90"),
                helpText("Per-90 stats provide fairer comparison between players with different playing time.")
            ),
            
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
            checkboxInput("use_goals", "Goals", value = TRUE),
            checkboxInput("use_assists", "Assists", value = TRUE),
            checkboxInput("use_xg", "Expected Goals (xG)", value = TRUE),
            checkboxInput("use_xag", "Expected Assists (xAG)", value = TRUE),
            
            h5("Playing Time & Discipline:"),
            conditionalPanel(
              condition = "input.stats_mode == 'total'",
              checkboxInput("use_min90s", "Minutes Played", value = FALSE)
            ),
            checkboxInput("use_starts", "Starts", value = FALSE),
            checkboxInput("use_cards", "Yellow Cards", value = FALSE),
            
            h5("Advanced Metrics:"),
            checkboxInput("use_prgc", "Progressive Carries", value = TRUE),
            checkboxInput("use_prgp", "Progressive Passes", value = TRUE),
            checkboxInput("use_prgr", "Progressive Receptions", value = FALSE),
            
            hr(),
            
            # Chart customization
            h4("Chart Options:"),
            radioButtons("chart_type", "Visualization Type:",
                        choices = list(
                          "Interactive Spider Chart" = "interactive_spider",
                          "Static Spider Chart" = "static_spider"
                        ),
                        selected = "interactive_spider"),
            
            sliderInput("chart_alpha", "Chart Transparency:",
                       min = 0.1, max = 1, value = 0.6, step = 0.1),
            
            checkboxInput("show_grid", "Show Grid Lines", value = TRUE),
            
            actionButton("update_chart", "Update Chart", 
                        class = "btn-primary btn-block")
          ),
          
          box(
            title = "Player Statistics Visualization", 
            status = "success", 
            solidHeader = TRUE,
            width = 8,
            height = "650px",
            
            conditionalPanel(
              condition = "input.chart_type == 'interactive_spider'",
              plotlyOutput("interactive_spider_chart", height = "600px")
            ),
            
            conditionalPanel(
              condition = "input.chart_type == 'static_spider'",
              plotOutput("spider_chart", height = "600px")
            )
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
      
      # Similar Players Tab
      tabItem(tabName = "similar",
        fluidRow(
          box(
            title = "Find Similar Players", 
            status = "primary", 
            solidHeader = TRUE,
            width = 4,
            
            h4("🔍 Player Similarity Analysis"),
            
            selectInput("reference_player", "Select Reference Player:",
                       choices = NULL,
                       selected = NULL),
            
            # Position constraint
            checkboxInput("same_position_only", "Same Position Only", value = TRUE),
            
            # Squad constraint
            checkboxInput("exclude_teammates", "Exclude Teammates", value = TRUE),
            
            # Number of similar players
            sliderInput("num_similar", "Number of Similar Players:",
                       min = 3, max = 10, value = 5),
            
            # Metrics for similarity calculation
            h5("Metrics for Similarity Calculation:"),
            checkboxInput("sim_attacking", "Attacking Stats", value = TRUE),
            checkboxInput("sim_creative", "Creative Stats", value = TRUE),
            checkboxInput("sim_advanced", "Advanced Stats", value = TRUE),
            
            # Minimum playing time filter
            sliderInput("min_minutes", "Minimum Minutes Played:",
                       min = 0, max = 1000, value = 200, step = 50),
            
            actionButton("find_similar", "Find Similar Players", 
                        class = "btn-success btn-block"),
            
            hr(),
            
            h5("How it works:"),
            helpText("Uses cosine similarity to find players with similar statistical profiles. 
                     Similarity score of 1.0 = identical profile, 0.0 = completely different.")
          ),
          
          box(
            title = "Similar Players Results", 
            status = "success", 
            solidHeader = TRUE,
            width = 8,
            
            conditionalPanel(
              condition = "input.reference_player == '' || input.reference_player == null",
              div(style = "text-align: center; padding: 50px;",
                  h3("👆 Select a reference player to find similar players", 
                     style = "color: #999;"))
            ),
            
            uiOutput("similar_players_results")
          )
        ),
        
        fluidRow(
          box(
            title = "Similarity Comparison Chart", 
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            
            plotlyOutput("similarity_comparison_chart", height = "500px")
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
  
  # Reactive data loading with per-90 calculations
  bundesliga_data <- reactive({
    # Sample data structure - replace with actual data loading
    # data <- read.csv("your_bundesliga_data.csv")
    
    data <- data.frame(
      Rk = 1:100,
      Player = paste("Player", 1:100),
      Nation = sample(c("GER", "FRA", "ESP", "BRA", "ARG"), 100, replace = TRUE),
      Pos = sample(c("GK", "DF", "MF", "FW"), 100, replace = TRUE),
      Squad = sample(c("Bayern Munich", "Borussia Dortmund", "RB Leipzig", "Bayer Leverkusen", 
                      "Union Berlin", "Freiburg", "Eintracht Frankfurt", "Wolfsburg"), 100, replace = TRUE),
      Age = sample(18:35, 100, replace = TRUE),
      MP = sample(5:34, 100, replace = TRUE),
      Starts = sample(0:34, 100, replace = TRUE),
      Min90s = round(runif(100, 5, 34), 1),
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
    
    # Calculate per-90 statistics
    data$Gls_per90 <- round(data$Gls / data$Min90s, 2)
    data$Ast_per90 <- round(data$Ast / data$Min90s, 2)
    data$xG_per90 <- round(data$xG / data$Min90s, 2)
    data$xAG_per90 <- round(data$xAG / data$Min90s, 2)
    data$CrdY_per90 <- round(data$CrdY / data$Min90s, 2)
    data$PrgC_per90 <- round(data$PrgC / data$Min90s, 2)
    data$PrgP_per90 <- round(data$PrgP / data$Min90s, 2)
    data$PrgR_per90 <- round(data$PrgR / data$Min90s, 2)
    data$Starts_per90 <- round(data$Starts / data$Min90s, 2)
    
    return(data)
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
    updateSelectInput(session, "reference_player", choices = c("Select a player..." = "", player_choices))
  })
  
  # Update filter choices
  observe({
    data <- bundesliga_data()
    
    # Update position filter
    positions <- c("All Positions" = "all", sort(unique(data$Pos)))
    updateSelectInput(session, "position_filter", choices = positions)
    
    # Update squad filter
    squads <- c("All Squads" = "all", sort(unique(data$Squad)))
    updateSelectInput(session, "squad_filter", choices = squads)
  })
  
  # Function to get metric names based on stats mode
  get_metric_names <- function(base_metrics, stats_mode) {
    if(stats_mode == "per90") {
      # Convert to per-90 versions
      per90_mapping <- c(
        "Gls" = "Gls_per90",
        "Ast" = "Ast_per90", 
        "xG" = "xG_per90",
        "xAG" = "xAG_per90",
        "CrdY" = "CrdY_per90",
        "PrgC" = "PrgC_per90",
        "PrgP" = "PrgP_per90",
        "PrgR" = "PrgR_per90",
        "Starts" = "Starts_per90",
        "Min90s" = "Min90s"
      )
      
      result <- c()
      for(metric in base_metrics) {
        if(metric %in% names(per90_mapping)) {
          result <- c(result, per90_mapping[metric])
        } else {
          result <- c(result, metric)
        }
      }
      return(unname(result))
    } else {
      return(base_metrics)
    }
  }
  
  # Prepare data for spider chart
  spider_data <- eventReactive(input$update_chart, {
    req(input$player1)
    
    data <- bundesliga_data()
    selected_players <- c(input$player1)
    
    if(input$player2 != "") selected_players <- c(selected_players, input$player2)
    if(input$player3 != "") selected_players <- c(selected_players, input$player3)
    
    # Get selected base metrics
    base_metrics <- c()
    if(input$use_goals) base_metrics <- c(base_metrics, "Gls")
    if(input$use_assists) base_metrics <- c(base_metrics, "Ast")
    if(input$use_xg) base_metrics <- c(base_metrics, "xG")
    if(input$use_xag) base_metrics <- c(base_metrics, "xAG")
    if(input$use_min90s && input$stats_mode == "total") base_metrics <- c(base_metrics, "Min90s")
    if(input$use_starts) base_metrics <- c(base_metrics, "Starts")
    if(input$use_cards) base_metrics <- c(base_metrics, "CrdY")
    if(input$use_prgc) base_metrics <- c(base_metrics, "PrgC")
    if(input$use_prgp) base_metrics <- c(base_metrics, "PrgP")
    if(input$use_prgr) base_metrics <- c(base_metrics, "PrgR")
    
    if(length(base_metrics) == 0) {
      return(NULL)
    }
    
    # Convert to actual metric names based on stats mode
    selected_metrics <- get_metric_names(base_metrics, input$stats_mode)
    
    # Filter data for selected players
    player_data <- data[data$Player %in% selected_players, c("Player", selected_metrics)]
    
    # Use percentile normalization instead of min-max
    normalized_data <- player_data
    for(metric in selected_metrics) {
      if(all(!is.na(data[[metric]]))) {
        normalized_data[[metric]] <- percent_rank(data[[metric]]) * 100
      } else {
        normalized_data[[metric]] <- 50
      }
    }
    
    # Create display names for metrics
    display_names <- base_metrics
    if(input$stats_mode == "per90") {
      display_names <- paste(base_metrics, "(per 90)")
    }
    
    # Prepare for fmsb radar chart (add max and min rows)
    chart_data <- rbind(
      rep(100, length(selected_metrics)),  # Max values (100th percentile)
      rep(0, length(selected_metrics)),    # Min values (0th percentile)
      normalized_data[, selected_metrics]
    )
    
    colnames(chart_data) <- display_names
    rownames(chart_data) <- c("Max", "Min", normalized_data$Player)
    
    list(
      chart_data = chart_data,
      player_names = normalized_data$Player,
      metrics = selected_metrics,
      display_names = display_names,
      raw_data = player_data
    )
  }, ignoreNULL = FALSE)
  
  # Create interactive spider chart with Plotly
  output$interactive_spider_chart <- renderPlotly({
    spider_info <- spider_data()
    
    if(is.null(spider_info)) {
      p <- plot_ly() %>%
        add_annotations(
          text = "Please select at least one metric and update the chart",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16, color = "gray")
        ) %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
      return(p)
    }
    
    # Colors for different players
    colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57")
    
    # Create plotly radar chart
    p <- plot_ly(type = 'scatterpolar', mode = 'lines+markers')
    
    for(i in 1:length(spider_info$player_names)) {
      player_name <- spider_info$player_names[i]
      values <- as.numeric(spider_info$chart_data[player_name, ])
      raw_values <- as.numeric(spider_info$raw_data[spider_info$raw_data$Player == player_name, spider_info$metrics])
      
      # Create hover text with raw values and percentiles
      hover_text <- paste(
        "<b>", spider_info$display_names, "</b><br>",
        "Value: ", round(raw_values, 2), "<br>",
        "Percentile: ", round(values, 1), "th",
        "<extra></extra>"
      )
      
      p <- p %>%
        add_trace(
          r = values,
          theta = spider_info$display_names,
          name = player_name,
          line = list(color = colors[i], width = 3),
          marker = list(color = colors[i], size = 8),
          fill = 'toself',
          fillcolor = scales::alpha(colors[i], input$chart_alpha),
          hovertemplate = hover_text
        )
    }
    
    p <- p %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = input$show_grid,
            range = c(0, 100),
            tickvals = c(0, 25, 50, 75, 100),
            ticktext = c("0th", "25th", "50th", "75th", "100th")
          ),
          angularaxis = list(
            tickfont = list(size = 12)
          )
        ),
        title = list(
          text = paste("Player Comparison -", 
                      ifelse(input$stats_mode == "per90", "Per 90 Minutes", "Season Totals"),
                      "<br><sub>Values shown as league percentiles</sub>"),
          font = list(size = 16)
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          y = -0.1,
          xanchor = 'center'
        ),
        margin = list(t = 80, b = 80, l = 80, r = 80)
      )
    
    return(p)
  })
  
  # Keep static spider chart for backwards compatibility
  output$spider_chart <- renderPlot({
    spider_info <- spider_data()
    
    if(is.null(spider_info)) {
      plot.new()
      text(0.5, 0.5, "Please select at least one metric and update the chart", 
           cex = 1.5, col = "gray")
      return()
    }
    
    colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57")
    
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
      caxislabels = c("0th", "25th", "50th", "75th", "100th"),
      cglwd = 0.8,
      vlcex = 1.2,
      title = paste("Player Statistics Comparison -", 
                   ifelse(input$stats_mode == "per90", "Per 90 Minutes", "Season Totals"))
    )
    
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
  
  # Similar Players Analysis
  similar_players_data <- eventReactive(input$find_similar, {
    req(input$reference_player)
    
    data <- bundesliga_data()
    
    # Filter by minimum minutes
    data <- data[data$Min90s >= input$min_minutes/90, ]
    
    # Get reference player data
    ref_player_data <- data[data$Player == input$reference_player, ]
    
    if(nrow(ref_player_data) == 0) {
      return(NULL)
    }
    
    # Filter candidates
    candidates <- data
    
    if(input$same_position_only) {
      candidates <- candidates[candidates$Pos == ref_player_data$Pos, ]
    }
    
    if(input$exclude_teammates) {
      candidates <- candidates[candidates$Squad != ref_player_data$Squad, ]
    }
    
    # Remove the reference player from candidates
    candidates <- candidates[candidates$Player != input$reference_player, ]
    
    if(nrow(candidates) == 0) {
      return(list(error = "No eligible players found with current filters"))
    }
    
    # Select metrics for similarity calculation
    similarity_metrics <- c()
    
    if(input$sim_attacking) {
      similarity_metrics <- c(similarity_metrics, "Gls_per90", "xG_per90")
    }
    
    if(input$sim_creative) {
      similarity_metrics <- c(similarity_metrics, "Ast_per90", "xAG_per90")
    }
    
    if(input$sim_advanced) {
      similarity_metrics <- c(similarity_metrics, "PrgC_per90", "PrgP_per90", "PrgR_per90")
    }
    
    if(length(similarity_metrics) == 0) {
      return(list(error = "Please select at least one metric category for similarity calculation"))
    }
    
    # Calculate cosine similarity
    ref_vector <- as.numeric(ref_player_data[similarity_metrics])
    ref_vector[is.na(ref_vector)] <- 0
    
    similarities <- c()
    
    for(i in 1:nrow(candidates)) {
      candidate_vector <- as.numeric(candidates[i, similarity_metrics])
      candidate_vector[is.na(candidate_vector)] <- 0
      
      # Cosine similarity calculation
      dot_product <- sum(ref_vector * candidate_vector)
      magnitude_ref <- sqrt(sum(ref_vector^2))
      magnitude_candidate <- sqrt(sum(candidate_vector^2))
      
      if(magnitude_ref == 0 || magnitude_candidate == 0) {
        similarity <- 0
      } else {
        similarity <- dot_product / (magnitude_ref * magnitude_candidate)
      }
      
      similarities <- c(similarities, similarity)
    }
    
    # Add similarity scores to candidates
    candidates$similarity_score <- similarities
    
    # Sort by similarity and get top N
    candidates <- candidates[order(candidates$similarity_score, decreasing = TRUE), ]
    top_similar <- head(candidates, input$num_similar)
    
    return(list(
      reference_player = ref_player_data,
      similar_players = top_similar,
      similarity_metrics = similarity_metrics
    ))
  })
  
  # Render similar players results
  output$similar_players_results <- renderUI({
    similar_data <- similar_players_data()
    
    if(is.null(similar_data)) {
      return(div(style = "text-align: center; padding: 50px;",
                h3("Click 'Find Similar Players' to see results", style = "color: #999;")))
    }
    
    if(!is.null(similar_data$error)) {
      return(div(class = "alert alert-warning",
                h4("⚠️ ", similar_data$error)))
    }
    
    # Create cards for each similar player
    player_cards <- lapply(1:nrow(similar_data$similar_players), function(i) {
      player <- similar_data$similar_players[i, ]
      
      div(class = "similar-player-card",
          div(class = "similarity-score",
              paste("Similarity:", round(player$similarity_score * 100, 1), "%")),
          h4(player$Player, style = "margin: 0; color: #333;"),
          p(style = "margin: 5px 0; color: #666;",
            paste(player$Squad, "•", player$Pos, "• Age:", player$Age)),
          div(style = "display: flex; justify-content: space-between; margin-top: 10px;",
              div(
                strong("Goals/90: "), round(player$Gls_per90, 2), br(),
                strong("Assists/90: "), round(player$Ast_per90, 2)
              ),
              div(
                strong("xG/90: "), round(player$xG_per90, 2), br(),
                strong("xAG/90: "), round(player$xAG_per90, 2)
              )
          )
      )
    })
    
    return(div(
      h4(paste("Players most similar to", input$reference_player, ":")),
      do.call(div, player_cards)
    ))
  })
  
  # Similarity comparison chart
  output$similarity_comparison_chart <- renderPlotly({
    similar_data <- similar_players_data()
    
    if(is.null(similar_data) || !is.null(similar_data$error)) {
      p <- plot_ly() %>%
        add_annotations(
          text = "Find similar players to see comparison chart",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16, color = "gray")
        ) %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
      return(p)
    }
    
    # Prepare data for comparison
    comparison_metrics <- c("Gls_per90", "Ast_per90", "xG_per90", "xAG_per90", "PrgC_per90", "PrgP_per90")
    
    ref_player <- similar_data$reference_player
    similar_players <- head(similar_data$similar_players, 3)  # Show top 3 for clarity
    
    all_players <- rbind(ref_player, similar_players)
    
    # Create radar chart comparison
    colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4")
    
    p <- plot_ly(type = 'scatterpolar', mode = 'lines+markers')
    
    for(i in 1:nrow(all_players)) {
      player_name <- all_players$Player[i]
      values <- as.numeric(all_players[i, comparison_metrics])
      
      # Normalize to percentiles for fair comparison
      percentile_values <- sapply(1:length(comparison_metrics), function(j) {
        metric <- comparison_metrics[j]
        all_values <- bundesliga_data()[[metric]]
        percent_rank(c(all_values, values[j])) * 100
      })
      percentile_values <- percentile_values[length(percentile_values)]
      
      line_width <- if(i == 1) 4 else 2  # Thicker line for reference player
      
      p <- p %>%
        add_trace(
          r = values,
          theta = c("Goals/90", "Assists/90", "xG/90", "xAG/90", "Prog Carries/90", "Prog Passes/90"),
          name = if(i == 1) paste(player_name, "(Reference)") else player_name,
          line = list(color = colors[i], width = line_width),
          marker = list(color = colors[i], size = if(i == 1) 10 else 6)
        )
    }
    
    p <- p %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, max(all_players[, comparison_metrics], na.rm = TRUE) * 1.1)
          )
        ),
        title = list(
          text = "Similar Players Comparison<br><sub>Per 90 minute statistics</sub>",
          font = list(size = 16)
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          y = -0.1,
          xanchor = 'center'
        ),
        margin = list(t = 80, b = 100, l = 80, r = 80)
      )
    
    return(p)
  })
  
  # Player summary table
  output$player_summary <- DT::renderDataTable({
    req(input$player1)
    
    data <- bundesliga_data()
    selected_players <- c(input$player1)
    
    if(input$player2 != "") selected_players <- c(selected_players, input$player2)
    if(input$player3 != "") selected_players <- c(selected_players, input$player3)
    
    if(input$stats_mode == "per90") {
      summary_data <- data[data$Player %in% selected_players, 
                          c("Player", "Squad", "Pos", "Age", "MP", "Min90s",
                            "Gls_per90", "Ast_per90", "xG_per90", "xAG_per90")]
      
      colnames(summary_data) <- c("Player", "Squad", "Position", "Age", "Matches", "Minutes/90",
                                 "Goals/90", "Assists/90", "xG/90", "xAG/90")
    } else {
      summary_data <- data[data$Player %in% selected_players, 
                          c("Player", "Squad", "Pos", "Age", "MP", "Min90s", 
                            "Gls", "Ast", "xG", "xAG")]
      
      colnames(summary_data) <- c("Player", "Squad", "Position", "Age", "Matches", "Minutes/90",
                                 "Goals", "Assists", "xG", "xAG")
    }
    
    DT::datatable(
      summary_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't'
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("Minutes/90", "Goals/90", "Assists/90", "xG/90", "xAG/90", 
                                 "Goals", "Assists", "xG", "xAG"), digits = 2)
  })
  
  # Full data table
  output$full_data_table <- DT::renderDataTable({
    data <- bundesliga_data()
    
    # Select relevant columns for display
    display_cols <- c("Player", "Squad", "Pos", "Age", "MP", "Min90s", 
                     "Gls", "Ast", "xG", "xAG", "CrdY", "PrgC", "PrgP", "PrgR",
                     "Gls_per90", "Ast_per90", "xG_per90", "xAG_per90")
    
    display_data <- data[, display_cols]
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        searchHighlight = TRUE,
        columnDefs = list(
          list(targets = c(4:17), className = "dt-right")  # Right-align numeric columns
        )
      ),
      filter = 'top',
      rownames = FALSE,
      colnames = c("Player", "Squad", "Pos", "Age", "MP", "Min/90", 
                  "Goals", "Assists", "xG", "xAG", "Yellow Cards", 
                  "Prog Carries", "Prog Passes", "Prog Receptions",
                  "Goals/90", "Assists/90", "xG/90", "xAG/90")
    ) %>%
      DT::formatRound(columns = c("Min/90", "xG", "xAG", "Goals/90", "Assists/90", 
                                 "xG/90", "xAG/90"), digits = 2) %>%
      DT::formatStyle(
        columns = c("Goals/90", "Assists/90", "xG/90", "xAG/90"),
        backgroundColor = "#f0f8ff",
        fontWeight = "bold"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# Instructions for use:
# 1. Install required packages: 
#    install.packages(c("shiny", "shinydashboard", "DT", "plotly", "dplyr", "fmsb", "scales"))
# 
# 2. Replace the sample data creation with your actual data loading:
#    In the bundesliga_data() reactive function, replace the sample data with:
#    data <- read.csv("your_bundesliga_data.csv")
# 
# 3. Run the app with: shiny::runApp()
# 
# 4. The app will open in your default browser

# NEW FEATURES IMPLEMENTED:

# 🎯 1. PER-90 NORMALIZATION:
# - Toggle between per-90 and total season statistics
# - Automatically calculates per-90 versions of all relevant metrics
# - Fair comparison between players with different playing time
# - Per-90 mode is recommended and set as default

# 📊 2. INTERACTIVE PLOTLY CHARTS:
# - Hover functionality shows exact values and percentiles
# - Smooth animations and zooming capabilities
# - Professional styling with customizable transparency
# - Toggle between interactive and static chart types
# - Values displayed as league percentiles for better context

# 🔍 3. SIMILAR PLAYER FINDER:
# - Advanced cosine similarity algorithm
# - Filters: same position, exclude teammates, minimum minutes
# - Customizable similarity metrics (attacking, creative, advanced)
# - Professional cards showing similarity scores and key stats
# - Comparison chart showing reference player vs top similar players
# - Results show percentage similarity scores

# ADDITIONAL ENHANCEMENTS:
# - Percentile-based normalization instead of min-max scaling
# - Enhanced UI with better organization and visual hierarchy
# - Comprehensive help text and user guidance
# - Professional styling with custom CSS
# - Improved data table with per-90 statistics highlighted
# - Better error handling and user feedback

# The app now provides much more sophisticated analysis capabilities while
# maintaining ease of use. The similar player finder is particularly powerful
# for scouting and tactical analysis.
