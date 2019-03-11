#nacitanie kniznic
library(shiny)
library(ggplot2)

#nacitanie dat
setwd("C:/Users/Simi/Documents/GeneralInsurance_Class/Data")
dt_KPI <- read.csv("lesson2_KPI.csv")

#korekcia dat, odstranenie zaznamov s NA hodnotou
data_pracovne <- na.omit(dt_KPI) 

# Define a server for the Shiny app
function(input, output) {
  # Fill in the spot we created for a plot
  output$Plot <- renderPlot({
    # Render a ggplot
    ggplot(data = data_pracovne,
           mapping = aes_string(x = "Premium", y = "Expenses", colour = input$vyber)
    ) +
      geom_point() +  geom_smooth() 
  })
}
