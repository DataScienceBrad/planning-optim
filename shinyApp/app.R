library(shiny)

names = c(
'week1',
'week2',
'week3',
'week4',
'week5',
'week6',
'week7',
'week8',
'week9',
'week10')

displayNames = c(
'Week 1',
'Week 2',
'Week 3',
'Week 4',
'Week 5',
'Week 6',
'Week 7',
'Week 8',
'Week 9',
'Week 10')

fourchette = t(matrix(rep(c(60, 120), 10), ncol = 10))
valueFourchette = rep(100, 10)



# Define UI for slider demo app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Sliders"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar to demonstrate various slider options ----
    sidebarPanel(

      sliderInput(names[1], displayNames[1],
                  min = fourchette[1, 1],
                  max = fourchette[1, 2],
                  value = valueFourchette[1]),

      sliderInput(names[2], displayNames[2],
                  min = fourchette[2, 1],
                  max = fourchette[2, 2],
                  value = valueFourchette[2]),

      sliderInput(names[3], displayNames[3],
                  min = fourchette[3, 1],
                  max = fourchette[3, 2],
                  value = valueFourchette[3]),

      sliderInput(names[4], displayNames[4],
                  min = fourchette[4, 1],
                  max = fourchette[4, 2],
                  value = valueFourchette[4]),

      sliderInput(names[5], displayNames[5],
                  min = fourchette[5, 1],
                  max = fourchette[5, 2],
                  value = valueFourchette[5]),

      sliderInput(names[6], displayNames[6],
                  min = fourchette[6, 1],
                  max = fourchette[6, 2],
                  value = valueFourchette[6]),

      sliderInput(names[7], displayNames[7],
                  min = fourchette[7, 1],
                  max = fourchette[7, 2],
                  value = valueFourchette[7]),

      sliderInput(names[8], displayNames[8],
                  min = fourchette[8, 1],
                  max = fourchette[8, 2],
                  value = valueFourchette[8]),

      sliderInput(names[9], displayNames[9],
                  min = fourchette[9, 1],
                  max = fourchette[9, 2],
                  value = valueFourchette[9]),

      sliderInput(names[10], displayNames[10],
                  min = fourchette[10, 1],
                  max = fourchette[10, 2],
                  value = valueFourchette[10]),



      # Input: Simple integer interval ----
      sliderInput("integer", "Integer:",
                  min = 0, max = 1000,
                  value = 500),

      # Input: Decimal interval with step value ----
      sliderInput("decimal", "Decimal:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.1),

      # Input: Specification of range within an interval ----
      sliderInput("range", "Range:",
                  min = 1, max = 1000,
                  value = c(200,500)),

      # Input: Custom currency format for with basic animation ----
      sliderInput("format", "Custom Format:",
                  min = 0, max = 10000,
                  value = 0, step = 2500,
                  pre = "$", sep = ",",
                  animate = TRUE),

      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      sliderInput("animation", "Looping Animation:",
                  min = 1, max = 2000,
                  value = 1, step = 10,
                  animate =
                    animationOptions(interval = 300, loop = TRUE))

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Table summarizing the values entered ----
      tableOutput("values")

    )
  )
)


# Define server logic for slider examples ----
server <- function(input, output) {

  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({






    data.frame(
      Name = c("Integer",
               "Decimal",
               "Range",
               "Custom Format",
               "Animation"),
      Value = as.character(c(input$integer,
                             input$decimal,
                             paste(input$range, collapse = " "),
                             input$format,
                             input$animation)),
      stringsAsFactors = FALSE)

  })

  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })

}

# Create Shiny app ----
shinyApp(ui, server)
