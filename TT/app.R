# Load necessary libraries
library(shiny)
library(tidyverse)
library(plotly)
library(readr)
library(here)
library(gganimate)
library(gifski)

<<<<<<< HEAD
# Create necessary directories
if (!dir.exists("www")) {
  dir.create("www")
}
if (!dir.exists("images")) {
  dir.create("images")
}
=======
# Create the 'www' directory if it doesn't exist
if (!dir.exists("www")) {
  dir.create("www")
}
>>>>>>> ef5ee31670bc7f027a932e0edd2564a59d868322

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

# Prepare data for promotion and relegation trends
promotion_relegation <- standings %>%
  group_by(season, season_outcome) %>%
  summarise(count = n())

<<<<<<< HEAD
# Function to save plots
save_plot <- function(plot, filename) {
  ggsave(filename, plot, width = 10, height = 6)
}

# Function to create and save plots for each team
save_team_plots <- function(team_name) {
  team_dir <- file.path("images", team_name)
  if (!dir.exists(team_dir)) {
    dir.create(team_dir)
  }
  
  # Filter data for the team
  filtered_matches <- matches %>% filter(home_team_name == team_name | away_team_name == team_name)
  filtered_attendance <- attendance_over_time %>% filter(season %in% unique(filtered_matches$season))
  filtered_prom_rel <- promotion_relegation %>% filter(season %in% unique(filtered_matches$season))
  team_performance <- standings %>% filter(team_name == team_name) %>% select(season, played, wins, draws, losses, goals_for, goals_against, points)
  team_wins_reactive <- standings %>% filter(team_name == team_name) %>% filter(tier == 1) %>% group_by(season, team_name) %>% summarise(total_wins = sum(wins)) %>% ungroup() %>% arrange(season, desc(total_wins))
  
  # Create and save plots
  match_outcomes_plot <- ggplot(filtered_matches %>% group_by(result) %>% summarise(count = n()), aes(x = result, y = count, fill = result)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Distribution of Match Outcomes for", team_name), x = "Outcome", y = "Count")
  save_plot(match_outcomes_plot, file.path(team_dir, "match_outcomes.png"))
  
  attendance_plot <- ggplot(filtered_attendance, aes(x = season, y = avg_attendance)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = paste("Average Attendance for", team_name, "Over Seasons"), x = "Season", y = "Average Attendance")
  save_plot(attendance_plot, file.path(team_dir, "attendance_over_time.png"))
  
  team_perf_plot <- ggplot(team_performance, aes(x = season)) +
    geom_line(aes(y = points, color = "Points")) +
    geom_line(aes(y = goals_for, color = "Goals For")) +
    geom_line(aes(y = goals_against, color = "Goals Against")) +
    theme_minimal() +
    labs(title = paste(team_name, "Performance Over Seasons"), x = "Season", y = "Count") +
    scale_color_manual(name = "Metrics", values = c("Points" = "blue", "Goals For" = "green", "Goals Against" = "red"))
  save_plot(team_perf_plot, file.path(team_dir, "team_performance.png"))
  
  promotion_relegation_plot <- ggplot(filtered_prom_rel, aes(x = season, y = count, fill = season_outcome)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    labs(title = paste("Promotion and Relegation Trends for", team_name), x = "Season", y = "Count", fill = "Outcome")
  save_plot(promotion_relegation_plot, file.path(team_dir, "promotion_relegation.png"))
  
  team_wins_plot <- ggplot(team_wins_reactive, aes(x = reorder(team_name, -total_wins), y = total_wins, fill = team_name)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    transition_states(season, transition_length = 2, state_length = 1) +
    labs(title = 'Total Wins by Season: {closest_state}', x = 'Team', y = 'Total Wins') +
    ease_aes('linear')
  anim <- animate(team_wins_plot, nframes = 100, fps = 10, renderer = gifski_renderer())
  anim_save(file.path(team_dir, "team_wins_over_time.gif"), animation = anim)
}

# Save plots for all teams
unique_teams <- unique(standings$team_name)
lapply(unique_teams, save_team_plots)

=======
>>>>>>> ef5ee31670bc7f027a932e0edd2564a59d868322
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
  
  # Reactive expressions for filtered data based on selected team
  filtered_matches <- reactive({
    matches %>%
      filter(home_team_name == input$team | away_team_name == input$team)
  })
  
  filtered_attendance <- reactive({
    attendance_over_time %>%
      filter(season %in% unique(filtered_matches()$season))
  })
  
  filtered_prom_rel <- reactive({
    promotion_relegation %>%
      filter(season %in% unique(filtered_matches()$season))
  })
  
  team_performance <- reactive({
    standings %>%
      filter(team_name == input$team) %>%
      select(season, played, wins, draws, losses, goals_for, goals_against, points)
  })
  
  team_wins_reactive <- reactive({
    standings %>%
      filter(team_name == input$team) %>%
      filter(tier == 1) %>%
      group_by(season, team_name) %>%
      summarise(total_wins = sum(wins)) %>%
      ungroup() %>%
      arrange(season, desc(total_wins))
  })
  
  # Interactive match outcomes
  output$match_outcomes_plot <- renderPlotly({
    match_outcomes <- filtered_matches() %>%
      group_by(result) %>%
      summarise(count = n())
    
    match_outcomes_plot <- ggplot(match_outcomes, aes(x = result, y = count, fill = result)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = paste("Distribution of Match Outcomes for", input$team), x = "Outcome", y = "Count")
    
    ggplotly(match_outcomes_plot)
  })
  
  # Interactive attendance over time
  output$attendance_plot <- renderPlotly({
    attendance_plot <- ggplot(filtered_attendance(), aes(x = season, y = avg_attendance)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Average Attendance for", input$team, "Over Seasons"), x = "Season", y = "Average Attendance")
    
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
    promotion_relegation_plot <- ggplot(filtered_prom_rel(), aes(x = season, y = count, fill = season_outcome)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = paste("Promotion and Relegation Trends for", input$team), x = "Season", y = "Count", fill = "Outcome")
    
    ggplotly(promotion_relegation_plot)
  })
  
  # Animated racing bar plot for team wins over time
  output$team_wins_plot <- renderImage({
    # Generate the animated plot using reactive team_wins()
    team_wins_plot <- ggplot(team_wins_reactive(), aes(x = reorder(team_name, -total_wins), y = total_wins, fill = team_name)) +
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
