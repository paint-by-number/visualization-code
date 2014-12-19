# server.R

library(fmsb)
source("helper.R")
data <- readRDS("data/vietnam_africa.rds")

data <- data[!identify_outliers(data$gdp.percap), ]
data <- na.omit(data)

data_maxmin <- sapply(data[ , 2:ncol(data)], function(col) c(max(col), min(col)))
data_vietnam <- rbind.data.frame(data_maxmin,
                  data[data$country=="Vietnam", setdiff(names(data), c("country"))])
data_vietnam <- data.frame(sapply(data_vietnam, round, digits=1))

shinyServer(function(input, output) {

  data_other <- reactive({
    tmp <- rbind.data.frame(data_maxmin,
      data[data$country==input$other_country, setdiff(names(data), c("country"))])
    tmp <- data.frame(sapply(tmp, round, digits=1))
    return(tmp)
  })

  output$selecCountryUI <- renderUI({
    selectInput("other_country", label="Select a country to compare with Vietnam",
                choices=data$country)
  })

  output$plot <- renderPlot({
    par(mar=c(1, 1, 1, 1), oma=c(1, 1, 1, 1),  mfrow=c(1, 2))
    radarchart(data_vietnam, axistype=2, pty=32, plwd=1:5, pcol=1, centerzero=TRUE,
               pfcol="yellow", pdensity=10,
               seg=4, vlabels=c("GDP per capita (2005 USD)", "\n\nInternet User\n(per 100)",
                                "Infant Survived (per 1000)", "\nLife Exp (Years)"),
              title="Vietnam")
    radarchart(data_other(), axistype=2, pty=32, plwd=1:5, pcol=1, centerzero=TRUE,
               pfcol="yellow", pdensity=10,
               seg=4, vlabels=c("GDP per capita (2005 USD)", "\n\nInternet User\n(per 100)",
                                "Infant Survived (per 1000)", "\nLife Exp (Years)"),
               title=input$other_country)
  })
})