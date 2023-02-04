library(shiny)
library(shinythemes)
library(ggplot2)
library(babynames)
library(dplyr)

# App 1
ui <- fluidPage(
  titlePanel("Baby Names Popularity"),
  shinythemes::themeSelector(),
  
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Enter Name:", "David")
      ),
  
    mainPanel(
      plotOutput("trend")
    )
  )
)

server <- function(input, output) {
  output$trend <- renderPlot({
    data_name <- subset(babynames, 
                        name == input$name
                        )
    ggplot(data_name, aes(x=year, y=prop, color=sex)) +
             geom_line()
      
  })
}

shinyApp(ui = ui, server = server)


# App 2

ui <- fluidPage(
  titlePanel("Most Popular Names"),
  theme = shinythemes::shinytheme("journal"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sex", "Select Sex", choices = c("M", "F")),
      sliderInput("year", "Select Year", min=1880, max=2017, value=1880)
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
  get_top10_names <- function() {
    babynames %>%
      filter(sex == input$sex) %>%
      filter(year == input$year) %>%
      slice_max(prop, n=10)
  }
  
  output$plot <- renderPlot({
    ggplot(get_top10_names(), aes(x = name, y = prop, label = n)) +
      geom_col() +
      geom_text(vjust = 1.2, color = "white")
  })
  
  output$table <- renderTable({get_top10_names()})
}

shinyApp(ui = ui, server = server)
