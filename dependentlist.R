library(shinythemes)
library(shiny)
library(ggplot2)

ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel("Welcome to Responisve Shiny"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("data", "Dataset:",
                                choices = c("mtcars", "iris", "diamonds")
                    ),
                    uiOutput("x_axis"),
                    uiOutput("y_axis"),
                    uiOutput("color")
                  ),
                  mainPanel(
                    plotOutput("distPlot")
                  )
                )
)
server <- function(input, output) {
  output$x_axis <- renderUI({
    col_opts <- get(input$data)
    selectInput("x_axis2", "X-Axis:",
                choices = names(col_opts))
  })
  output$y_axis <- renderUI({
    cols2 <- reactive({
      col_opts2 <- get(input$data)
      names(col_opts2)[!grepl(input$x_axis2, names(col_opts2))]
    })
    selectInput("y_axis2", "Y-Axis:",
                choices = cols2(),
                selected = "hp")
  })
  output$color <- renderUI({
    col_opts <- get(input$data)
    selectInput("color", "Color:",
                choices = names(col_opts),
                selected = "cyl")
  })
  output$distPlot <- renderPlot({
    if(input$data == "mtcars"){
      p <- ggplot(mtcars, aes_string(input$x_axis2, input$y_axis2, color = input$color)) +
        geom_point()
    }
    if(input$data == "iris"){
      p <- ggplot(iris, aes_string(input$x_axis2, input$y_axis2, color = input$color)) +
        geom_point()
    }
    if(input$data == "diamonds"){
      p <- ggplot(diamonds, aes_string(input$x_axis2, input$y_axis2, color = input$color)) +
        geom_point()
    }
    print(p)
  })
}
shinyApp(ui = ui, server = server)