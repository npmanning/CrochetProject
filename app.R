library(shiny)
library(tidyverse)
library(rsconnect)

# Load the dataset
crochet_final <- read_csv("crochet_final.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Crochet Pitch Expected Run Values"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "count",
        "Select Count:",
        choices = c("All", unique(crochet_final$count)),
        selected = "All"
      ),
      selectInput(
        "bat_side",
        "Select Batter Side:",
        choices = c("All", unique(crochet_final$`matchup.batSide.description`)),
        selected = "All"
      )
    ),
    
    mainPanel(
      plotOutput("run_dist_plot", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    data <- crochet_final
    if (input$count != "All") {
      data <- data %>% filter(count == input$count)
    }
    if (input$bat_side != "All") {
      data <- data %>% filter(`matchup.batSide.description` == input$bat_side)
    }
    data
  })
  
  # Render plot
  output$run_dist_plot <- renderPlot({
    data <- filtered_data()
    
    # Calculate mean and confidence interval
    summary_stats <- data %>%
      group_by(details.type.description) %>%
      summarize(
        mean_er = mean(expected_run_value, na.rm = TRUE),
        se = sd(expected_run_value, na.rm = TRUE) / sqrt(n()),
        lower = mean_er - qt(0.975, df = n() - 1) * se,
        upper = mean_er + qt(0.975, df = n() - 1) * se
      ) %>%
      mutate(label_y = upper + 0.25)  # push labels higher
    
    ggplot(data, aes(x = details.type.description, y = expected_run_value)) +
      geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
      stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
      geom_text(
        data = summary_stats,
        aes(
          x = details.type.description,
          y = label_y,
          label = sprintf("Mean: %.3f\n[%.3f, %.3f]", mean_er, lower, upper)
        ),
        color = "black",
        size = 3,
        vjust = 0
      ) +
      labs(
        x = "Pitch Type",
        y = "Expected Run Value",
        title = "Pitch-by-Pitch Expected Run Value with 95% Confidence Intervals"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)