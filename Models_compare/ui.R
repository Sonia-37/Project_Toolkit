library(shiny)

ui <- fluidPage(
  titlePanel("Comparison of Dynamic and Minimal Glucose–Insulin Models"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plotMode", "Plot Mode",
                  choices = c("Insulin and Glucose ~ Time" = "time", 
                              "Glucose ~ Insulin" = "gi", 
                              "Parameter Disturbance in Minimal Model" = "disturb")),
      
      conditionalPanel(
        condition = "input.plotMode != 'disturb'",
        sliderInput("glucoseDose", "Initial Glucose Dose (mg/dl)",
                    min = 100, max = 400, value = 209, step = 10),
        sliderInput("baseInsulin", "Basal insulin level (pM)",
                    min = 5, max = 150, value = 68.6, step = 1)
      ),
      
      conditionalPanel(
        condition = "input.plotMode == 'disturb'",
        sliderInput("h", "h Parameter (Minimal Model)",
                    min = 5, max = 150, value = 100, step = 1)
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.plotMode == 'time'",
        fluidRow(
          column(6, plotOutput("dynGlucosePlot")),
          column(6, plotOutput("minGlucosePlot"))
        ),
        fluidRow(
          column(6, plotOutput("dynInsulinPlot")),
          column(6, plotOutput("minInsulinPlot"))
        )
      ),
      conditionalPanel(
        condition = "input.plotMode == 'gi'",
        fluidRow(
          column(6, plotOutput("dynGlucoseInsulinPlot")),
          column(6, plotOutput("minGlucoseInsulinPlot"))
        )
      ),
      conditionalPanel(
        condition = "input.plotMode == 'disturb'",
        fluidRow(
          column(6, plotOutput("dystminGlucosePlot")),
          column(6, plotOutput("dystminInsulinPlot"))
        )
      )
    )
  )
)
