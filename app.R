library(shiny)
library(tidyverse)

crochet_final <- read_csv("crochet_final.csv")

# --- zone coordinate factory --------------------------------------------------
# We'll define coordinates for the 14 Statcast zones.
# The strike zone core is a 3x3 box with x in [-0.83, 0.83], y in [1.5, 3.5].
# We split that into 3 columns and 3 rows and place 11/12/13/14 outside.
make_zone_coords <- function() {
  # core zone boundaries
  x_min_zone <- -0.83
  x_max_zone <-  0.83
  y_min_zone <-  1.5
  y_max_zone <-  3.5
  
  # compute column breakpoints (3 columns)
  x_breaks <- seq(x_min_zone, x_max_zone, length.out = 4) # 4 points -> 3 intervals
  # compute row breakpoints (3 rows)
  y_breaks <- seq(y_min_zone, y_max_zone, length.out = 4)
  
  # convenience to build rectangles
  rect <- function(xl, xr, yb, yt) tibble(xmin = xl, xmax = xr, ymin = yb, ymax = yt)
  

  zones_core <- tibble(
    zone = c(7,8,9,4,5,6,1,2,3),
    xmin = c(x_breaks[1], x_breaks[2], x_breaks[3],
             x_breaks[1], x_breaks[2], x_breaks[3],
             x_breaks[1], x_breaks[2], x_breaks[3]),
    xmax = c(x_breaks[2], x_breaks[3], x_breaks[4],
             x_breaks[2], x_breaks[3], x_breaks[4],
             x_breaks[2], x_breaks[3], x_breaks[4]),
    ymin = c(y_breaks[1], y_breaks[1], y_breaks[1],
             y_breaks[2], y_breaks[2], y_breaks[2],
             y_breaks[3], y_breaks[3], y_breaks[3]),
    ymax = c(y_breaks[2], y_breaks[2], y_breaks[2],
             y_breaks[3], y_breaks[3], y_breaks[3],
             y_breaks[4], y_breaks[4], y_breaks[4])
  )
  
  # outside zones:
  # 11 = left top outside (adjacent to zone 7)
  # 12 = right top outside (adjacent to zone 9)
  # 13 = left bottom outside (adjacent to zone 1)
  # 14 = right bottom outside (adjacent to zone 3)
  # choose reasonable widths for outside gutters
  left_gutter_xmin  <- x_min_zone - 1.0    # extend left
  left_gutter_xmax  <- x_min_zone
  right_gutter_xmin <- x_max_zone
  right_gutter_xmax <- x_max_zone + 1.0   # extend right
  
  # top half and bottom half heights based on splits
  top_ymin    <- y_breaks[3] # start of top row
  top_ymax    <- y_breaks[4]
  bottom_ymin <- y_breaks[1]
  bottom_ymax <- y_breaks[2]
  
  outside <- tibble(
    zone = c(11, 12, 13, 14),
    xmin = c(left_gutter_xmin, right_gutter_xmin, left_gutter_xmin, right_gutter_xmin),
    xmax = c(left_gutter_xmax, right_gutter_xmax, left_gutter_xmax, right_gutter_xmax),
    ymin = c(top_ymin,      top_ymin,       bottom_ymin,   bottom_ymin),
    ymax = c(top_ymax,      top_ymax,       bottom_ymax,   bottom_ymax)
  )
  
  bind_rows(zones_core, outside)
}

zone_coords <- make_zone_coords()

# UI
ui <- fluidPage(
  titlePanel("Strike Zone Heatmap"),
  sidebarLayout(
    sidebarPanel(
      selectInput("pitch_type",
                  "Pitch Type:",
                  choices = c("All", sort(unique(crochet_final$details.type.description))),
                  selected = "All"),
      selectInput("bat_side",
                  "Batter Side:",
                  choices = c("All", sort(unique(crochet_final$`matchup.batSide.description`))),
                  selected = "All"),
      radioButtons("metric", "Metric to display:",
                   choices = c("Mean expected run value" = "mean",
                               "Frequency (n)" = "count"),
                   selected = "mean")
    ),
    mainPanel(
      plotOutput("zone_heatmap", height = "650px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered <- reactive({
    df <- crochet_final
    if (input$pitch_type != "All") df <- df %>% filter(details.type.description == input$pitch_type)
    if (input$bat_side  != "All") df <- df %>% filter(`matchup.batSide.description` == input$bat_side)
    df
  })
  
  zone_summary <- reactive({
    # summarize by pitchData.zone
    filtered() %>%
      filter(!is.na(pitchData.zone)) %>%
      group_by(pitchData.zone) %>%
      summarise(
        mean_value = mean(expected_run_value, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      rename(zone = pitchData.zone) %>%
      right_join(zone_coords, by = "zone") %>%
      # ensure zones with no data get NA mean and 0 n
      mutate(mean_value = if_else(is.na(mean_value), NA_real_, mean_value),
             n = if_else(is.na(n), 0L, n))
  })
  
  output$zone_heatmap <- renderPlot({
    z <- zone_summary()
    
    # define fill mapping
    if (input$metric == "mean") {
      fill_values <- z$mean_value
      fill_label  <- "Avg expected run value"
      fill_scale  <- scale_fill_gradient2(low = "green3", mid = "white", high = "red2", midpoint = 0, na.value = "grey90")
    } else {
      fill_values <- z$n
      fill_label  <- "Count (n)"
      fill_scale  <- scale_fill_gradient(low = "white", high = "lightblue1", na.value = "grey90")
    }
    
    # base plot with rectangles
    p <- ggplot(z) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = if (input$metric == "mean") mean_value else n),
                color = "black", size = 0.4) +
      fill_scale +
      # labels: zone ID in upper-left of each rect
      geom_text(aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = ifelse(n==0, "", paste0("n=", n))),
                color = "black", size = 4) +
      # optional: annotate zone numbers small
      geom_text(aes(x = xmin + 0.05*(xmax-xmin), y = ymax - 0.05*(ymax-ymin), label = zone),
                color = "black", size = 3, hjust = 0) +
      coord_fixed() +
      labs(title = paste("Strike Zone -", ifelse(input$pitch_type=="All","All Pitches", input$pitch_type),
                         "| Batter:", input$bat_side),
           fill = fill_label) +
      theme_minimal(base_size = 14) +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
    
    print(p)
  })
}

# Run
shinyApp(ui, server)