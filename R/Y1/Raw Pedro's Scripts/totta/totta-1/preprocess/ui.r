library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("COM R Data Visualizer"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput("machine", "Machine:",
                list("bvtolestrup01" = "bvtolestrup01",
                     "bvtolldapp02" = "bvtolldapp02",
                     "bvtolldapp03" = "bvtolldapp03",
                     "bvtolldapp04" = "bvtolldapp04",
                     "bvtolpcbdp01" = "bvtolpcbdp01",
                     "bvtolpocip01" = "bvtolpocip01",
                     "bvtolpocip02" = "bvtolpocip02",
                     "bvtolpocip03" = "bvtolpocip03",
                     "bvtolpocip04" = "bvtolpocip04",
                     "bvtowpocwp01" = "bvtowpocwp01")),
    
    checkboxInput("CPU", "CPU Usage", FALSE),
    checkboxInput("MEM", "Memory Usage", FALSE),
    checkboxInput("HD", "HD Usage", FALSE),
    checkboxInput("Hide","Hide HD legend",FALSE),
    sliderInput("range", "Hours:",
                min = 0, max = 24, value = c(0,24),animate=TRUE,),
    sliderInput("yrange", "%:",
                min = 0, max = 100, value = c(0,100), ticks=TRUE,  step=5)
    
    
  ),
  
  mainPanel(
    plotOutput("Plot",height="550px")
    )
))