library(shiny)

library(shiny)
library(deSolve)
library(ggplot2)
source("models.R")

server <- function(input, output) {
  
  simulateModels <- reactive({
    # Shared baseline values
    Gb <- 88
    Ib <- input$baseInsulin
    
    # Dynamic model 
    dyn_params <- c(
      b0 = input$glucoseDose,
      b1 = 0.0002,
      b2 = 0.0422,
      b3 = 1.64,
      b4 = 0.000109,
      b5 = 320,
      b6 = 0.033,
      b7 = 0.68,
      alpha = 0.01,
      Gb = Gb,
      Ib = Ib
    )
    
    G0_dyn <- Gb + dyn_params["b0"]
    I0_dyn <- Ib + dyn_params["b3"] * dyn_params["b0"]
    init_dyn <- c(G = G0_dyn, I = I0_dyn)
    time_dyn <- seq(0, 15000, by = 1)
    
    out_dyn <- dede(
      y = init_dyn,
      times = time_dyn,
      func = dynamic_model,
      parms = dyn_params,
      method = "lsoda"
    )
    colnames(out_dyn) <- c("time", "G", "I")
    out_dyn <- as.data.frame(out_dyn)
    
    # Minimal model 
    min_params <- c(
      P1 = 0.03082,
      P2 = 0.02093,
      P3 = 1.062e-5,
      n = 0.1,
      gamma = 1e-4,
      h = input$h,
      Gb = Gb,
      Ib = Ib
    )
    
    init_min <- c(
      G = input$glucoseDose + Gb,  # initial G same as dose
      X = 0,
      I = Ib
    )
    time_min <- seq(0, 180, by = 0.5)
    
    out_min <- ode(
      y = init_min,
      times = time_min,
      func = minimal_model,
      parms = min_params
    )
    colnames(out_min) <- c("time", "G", "X", "I")
    out_min <- as.data.frame(out_min)
    
    list(dynamic = out_dyn, minimal = out_min)
  })
  
  output$dynGlucosePlot <- renderPlot({
    if (input$plotMode == "time") {
      out <- simulateModels()$dynamic
      ggplot(out, aes(x = time, y = G)) +
        geom_line(color = "blue", linewidth = 1) +
        labs(title = "Minimal Model - Glucose",
             x = "Time (min)", 
             y = "Glucose (mg/dL)") +
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })
  
  output$minGlucosePlot <- renderPlot({
    if (input$plotMode == "time") {
      out <- simulateModels()$minimal
      ggplot(out, aes(x = time, y = G)) +
        geom_line(color = "blue", linewidth = 1) +
        labs(title = "Minimal Model - Glucose",
             x = "Time (min)", 
             y = "Glucose (mg/dL)") +
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })
  
  output$dynInsulinPlot <- renderPlot({
    if (input$plotMode == "time") {
      out <- simulateModels()$dynamic
      ggplot(out, aes(x = time, y = I)) +
        geom_line(color = "red", linewidth = 1) +
        labs(title = "Minimal Model - Insulin",
             x = "Time (min)", 
             y = "Insulin (µU/mL)") +
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })
  
  output$minInsulinPlot <- renderPlot({
    if (input$plotMode == "time") {
      out <- simulateModels()$minimal
      ggplot(out, aes(x = time, y = I)) +
        geom_line(color = "red", linewidth = 1) +
        labs(title = "Minimal Model - Insulin",
             x = "Time (min)", 
             y = "Insulin (µU/mL)") +
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
    }
  })
  
  output$dynGlucoseInsulinPlot <- renderPlot({
    if (input$plotMode == "gi") {
      out <- simulateModels()$dynamic
      ggplot(out, aes(x = I, y = G)) +
        geom_path(color = "purple", linewidth = 1) +
        labs(title = "Dynamic Model: Glucose vs Insulin",
             x = "Insulin (pM)", 
             y = "Glucose (mg/dL)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })
  
  output$minGlucoseInsulinPlot <- renderPlot({
    if (input$plotMode == "gi") {
      out <- simulateModels()$minimal
      ggplot(out, aes(x = I, y = G)) +
        geom_path(color = "purple", linewidth = 1) +
        labs(title = "Minimal Model: Glucose vs Insulin",
             x = "Insulin (pM)", 
             y = "Glucose (mg/dL)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })
  
  output$dystminGlucosePlot <- renderPlot({
    if (input$plotMode == "disturb") {
      out <- simulateModels()$minimal
      ggplot(out, aes(x = time, y = G)) +
        geom_line(color = "orange", linewidth = 1) +
        labs(title = "Minimal Model - Glucose",
             x = "Time (min)", 
             y = "Glucose (mg/dL)") +
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })
  output$dystminInsulinPlot <- renderPlot({
    if (input$plotMode == "disturb") {
      out <- simulateModels()$minimal
      ggplot(out, aes(x = time, y = I)) +
        geom_line(color = "orange", linewidth = 1) +
        labs(title = "Minimal Model - Insulin",
             x = "Time (min)", 
             y = "Insulin (µU/mL)") +
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })
}