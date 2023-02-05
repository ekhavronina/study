library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(babynames)     # babynames dataset

  
# App 1: Most popular baby names by year and sex

ui <- fluidPage(
  titlePanel("Most Popular Names"),
  theme = shinythemes::shinytheme("journal"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sex", "Select Sex", choices = c("M", "F")),
      sliderInput("year", "Select Year", min=1880, max=2017, value=1880),
      actionButton("show_names", "Show Names"),
      actionButton("show_help", "Help")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # observe({
  #   showNotification("Welcome!", type = "message")
  # })
  observeEvent(input$show_help, {
    showModal(modalDialog("Choose sex and year, then click to \"Show Names\"
                          to see Top-10 baby names"))
  })
  
  # reactive expression is cached, so it will be executed only once
  get_top10_names <- eventReactive(input$show_names, {
    babynames %>%
      filter(sex == input$sex) %>%
      filter(year == input$year) %>%
      slice_max(prop, n=10)
  })
  
  output$plot <- renderPlot({
    ggplot(get_top10_names(), aes(x = name, y = prop, label = n)) +
      geom_col() +
      geom_text(vjust = 1.2, color = "white")
  })
  
  output$table <- renderTable({get_top10_names()})
}

shinyApp(ui = ui, server = server)
