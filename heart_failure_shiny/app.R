#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)

# Load the data
df <- read.csv("heart.csv") %>%
  mutate(HeartDisease = ifelse(HeartDisease == 1, "Heart Disease", "No Heart Disease"))

# Determine variable types
num_vars <- names(df)[sapply(df, is.numeric)]
cat_vars <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]

# Exclude HeartDisease from selectable variables
selectable_vars <- setdiff(names(df), "HeartDisease")

ui <- fluidPage(
  titlePanel("Heart Dataset Variable Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var_select", "Select a variable:", choices = selectable_vars),
      uiOutput("second_var_ui"),
      radioButtons("hd_filter", "Heart Disease Filter:",
                   choices = c("Both", "Heart Disease", "No Heart Disease"),
                   selected = "Both")
    ),
    mainPanel(
      plotlyOutput("var_plot"),
      plotOutput("second_plot")
    )
  )
)

server <- function(input, output, session) {
  # Reactive: update second variable choices dynamically
  output$second_var_ui <- renderUI({
    second_choices <- setdiff(selectable_vars, input$var_select)
    selectInput("var_second", "Select second variable:", choices = second_choices)
  })
  
  # Reactive: full data filtered by heart disease
  base_filtered <- reactive({
    if (input$hd_filter == "Both") df else df %>% filter(HeartDisease == input$hd_filter)
  })
  
  selected_level <- reactive({
    click <- event_data("plotly_click", source = "main")
    if (!is.null(click)) {
      level_clicked <- click$x
      as.character(level_clicked)
    } else {
      NULL
    }
  })
  
  # Reactive: dynamically filtered by relayout and legend selection
  filtered_data <- reactive({
    df_filtered <- base_filtered()
    var <- input$var_select
    relayout <- event_data("plotly_relayout", source = "main")
    restyle <- event_data("plotly_restyle", source = "main")
    
    if (var %in% num_vars && !is.null(relayout[["xaxis.range[0]"]]) && !is.null(relayout[["xaxis.range[1]"]])) {
      df_filtered <- df_filtered %>%
        filter(get(var) >= relayout[["xaxis.range[0]"]], get(var) <= relayout[["xaxis.range[1]"]])
    }
    
    if (var %in% cat_vars && !is.null(selected_level())) {
      df_filtered <- df_filtered %>% filter(get(var) == selected_level())
    }
    
    
    df_filtered
  })
  
  # Render first plot
  output$var_plot <- renderPlotly({
    df_filtered <- df
    var <- input$var_select
    
    if (var %in% num_vars) {
      p <- plot_ly(df_filtered, x = ~get(var), source = "main") %>%
        add_histogram() %>%
        layout(title = paste("Histogram of", var), xaxis = list(title = var))
      event_register(p, "plotly_relayout")
      # event_register(p, "plotly_restyle")
      p
    } else if (var %in% cat_vars) {
      df_counts <- df_filtered %>% count(!!sym(var))
      p <- plot_ly(
        data = df_counts,
        x = ~get(var),
        y = ~n,
        type = "bar",
        color = ~get(var),
        colors = "Set2",
        text = ~n,
        hoverinfo = "text",
        source = "main"
      ) %>%
        layout(
          title = paste("Barplot of", var),
          xaxis = list(title = var),
          yaxis = list(title = "Count"),
          legend = list(title = list(text = var))
        )
      event_register(p, "plotly_relayout")
      # event_register(p, "plotly_restyle")
      p
    }
  })
  
  # Render second plot as ggplot based on filtered data
  output$second_plot <- renderPlot({
    req(input$var_second)
    df_plot <- filtered_data()
    var2 <- input$var_second
    
    if (var2 %in% num_vars) {
      ggplot(df_plot, aes(x = .data[[var2]])) +
        geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
        theme_minimal() +
        labs(title = paste("Histogram of", var2), x = var2, y = "Count")
    } else {
      ggplot(df_plot, aes(x = .data[[var2]], fill = .data[[var2]])) +
        geom_bar() +
        theme_minimal() +
        scale_fill_brewer(palette = "Set2") +
        labs(title = paste("Barplot of", var2), x = var2, y = "Count")
    }
  })
}

shinyApp(ui, server)