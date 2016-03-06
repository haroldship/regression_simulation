
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Regression coefficient with simulated outcome"),
  
  fluidRow(
    column(3, radioButtons("func",
                "Level of Fit",
                c("None"="unif", "Some"="narrow-norm", "Good"="norm"),
                inline=TRUE
    )),
    column(3, sliderInput("h",
                "Bandwidth:",
                min=0.005,
                max=4,
                step=0.005,
                value=1)),
    column(3, sliderInput("n.Y",
                "Number of sample points",
                min=10,
                max=200,
                step=10,
                value=50)),
    column(3, sliderInput("grid.n",
                "Number of Grid points",
                min=50,
                max=500,
                step=50,
                value=50))
  ),
  
  fluidRow(
    column(4, plotOutput("distPlot")),
    column(4, plotOutput("xyPlot")),
    column(4, plotOutput("mcPlot"))
  ),
  
  fluidRow(
    column(6, plotOutput("pvaluePlot1")),
    column(6, plotOutput("pvaluePlot2"))
  )
))
