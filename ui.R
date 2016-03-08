
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
    column(4, tags$h3("Mean RSS:", htmlOutput("rss", inline=TRUE))),
    column(4, tags$h3("Mean R^2:", htmlOutput("rsquared", inline=TRUE))),
    column(4, tags$h3("Mean MSE:", htmlOutput("mse", inline=TRUE)))
  ),
  
  fluidRow(
    column(4, radioButtons("func",
                "Level of Fit",
                c("None"="unif", "Some"="narrow-norm", "Good"="norm"),
                inline=TRUE
    )),
    column(2, sliderInput("h",
                "Bandwidth:",
                min=0.05,
                max=4,
                step=0.05,
                value=1)),
    column(2, sliderInput("h2",
                          "2nd Bandwidth:",
                          min=0.05,
                          max=4,
                          step=0.05,
                          value=1)),
    column(2, sliderInput("n.Y",
                "Sample points",
                min=10,
                max=200,
                step=10,
                value=50)),
    column(2, sliderInput("grid.n",
                "Grid points",
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
