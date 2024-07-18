<<<<<<< HEAD
# Load necessary libraries
library(shiny)
library(tidyverse)
library(plotly)
library(readr)
library(here)
library(gganimate)
library(gifski)

# Load the datasets using relative paths
matches <- read_csv(here("data", "ewf_matches.csv"))
appearances <- read_csv(here("data", "ewf_appearances.csv"))
standings <- read_csv(here("data", "ewf_standings.csv"))

# Preprocess data for analysis
match_outcomes <- matches %>%
  group_by(result) %>%
  summarise(count = n())

attendance_over_time <- matches %>%
  filter(!is.na(attendance)) %>%
  group_by(season) %>%
  summarise(avg_attendance = mean(attendance))

# Prepare data for team ranking evolution
team_wins <- standings %>%
  filter(tier == 1) %>%
  group_by(season, team_name) %>%
  summarise(total_wins = sum(wins)) %>%
  ungroup() %>%
  arrange(season, desc(total_wins))

# Create the UI
ui <- fluidPage(
  titlePanel("English Women's Football Analysis"),
  sidebarLayout(
    sidebarPanel(
      h3("Analysis Controls"),
      selectInput("team", "Select Team", choices = unique(standings$team_name)),
      actionButton("refresh", "Refresh Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Match Outcomes", plotlyOutput("match_outcomes_plot")),
        tabPanel("Attendance Over Time", plotlyOutput("attendance_plot")),
        tabPanel("Team Performance", plotlyOutput("team_performance_plot")),
        tabPanel("Promotion and Relegation", plotlyOutput("promotion_relegation_plot")),
        tabPanel("Team Wins Over Time", imageOutput("team_wins_plot"))
      )
    )
  )
)

# Create the server
server <- function(input, output, session) {
  
  # Reactive expression for filtered team performance
  team_performance <- reactive({
    standings %>%
      filter(team_name == input$team) %>%
      select(season, played, wins, draws, losses, goals_for, goals_against, points)
  })
  
  # Interactive match outcomes
  output$match_outcomes_plot <- renderPlotly({
    match_outcomes_plot <- ggplot(match_outcomes, aes(x = result, y = count, fill = result)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Distribution of Match Outcomes", x = "Outcome", y = "Count")
    
    ggplotly(match_outcomes_plot)
  })
  
  # Interactive attendance over time
  output$attendance_plot <- renderPlotly({
    attendance_plot <- ggplot(attendance_over_time, aes(x = season, y = avg_attendance)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = "Average Attendance Over Seasons", x = "Season", y = "Average Attendance")
    
    ggplotly(attendance_plot)
  })
  
  # Interactive team performance over seasons
  output$team_performance_plot <- renderPlotly({
    team_perf_plot <- ggplot(team_performance(), aes(x = season)) +
      geom_line(aes(y = points, color = "Points")) +
      geom_line(aes(y = goals_for, color = "Goals For")) +
      geom_line(aes(y = goals_against, color = "Goals Against")) +
      theme_minimal() +
      labs(title = paste(input$team, "Performance Over Seasons"), x = "Season", y = "Count") +
      scale_color_manual(name = "Metrics", values = c("Points" = "blue", "Goals For" = "green", "Goals Against" = "red"))
    
    ggplotly(team_perf_plot)
  })
  
  # Interactive promotion and relegation trends
  output$promotion_relegation_plot <- renderPlotly({
    promotion_relegation_plot <- ggplot(promotion_relegation, aes(x = season, y = count, fill = season_outcome)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Promotion and Relegation Trends", x = "Season", y = "Count", fill = "Outcome")
    
    ggplotly(promotion_relegation_plot)
  })
  
  # Animated racing bar plot for team wins over time
  output$team_wins_plot <- renderImage({
    # Generate the animated plot
    team_wins_plot <- ggplot(team_wins, aes(x = reorder(team_name, -total_wins), y = total_wins, fill = team_name)) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      transition_states(season, transition_length = 2, state_length = 1) +
      labs(title = 'Total Wins by Season: {closest_state}', x = 'Team', y = 'Total Wins') +
      ease_aes('linear')
    
    anim <- animate(team_wins_plot, nframes = 100, fps = 10, renderer = gifski_renderer())
    
    # Save the animation
    anim_save("www/team_wins_animation.gif", animation = anim)
    
    # Return the animation file
    list(src = "www/team_wins_animation.gif", contentType = 'image/gif')
  }, deleteFile = FALSE)
}

# Run the app
shinyApp(ui, server)
=======

>>>>>>> ef5ee31670bc7f027a932e0edd2564a59d868322
