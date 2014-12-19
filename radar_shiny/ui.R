library(shiny)

shinyUI(fluidPage(
  titlePanel("Comparing the many dimensions of development"),
  fluidRow(
    column(12,
      helpText("The radar chart below shows how a country performs along multiple dimensions.
               The maximum value in each axis is the highest value
               among Vietnam and Sub-Saharan African countries.
               It is clear that while Vietnam has a low GDP per capita, it performs very
               well in other dimensions."),
      helpText("* The top and low 5% outlier countries are removed"),
      uiOutput("selecCountryUI"),
      plotOutput("plot")
    )
  )
))