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


########################################################################################################################
########################################################################################################################
#Zdovodnenie, preco sa jednotlive kategorie spravaju odlisne:
#1. Pri zobrazeni zavislosti Premium a Expenses vzhladom na kategoriu region mozeme usudit: region Alandia je pravdepodobne najvacsim regionom nakolko sa v suvislosti s nim dosahuju najvacsie hodnoty Expenses a zaroven Premium. Pri regione Cergo rastie premenna Premium rychlejsie nez Expenses, co je pre poistovnu na prvy pohlad lepsie oproti regionu Belandia, kde naklady rastu rychlejsie nez vyzbierane poistne(zisk).
#2. V suvislosti s kategoriou unit mozeme usudit: Pri unit 8 sa dosahuju najvyssie hodnoty Premium, avsak s tym, ze su na to vynalozene najvyssie naklady. Pri porovnani Unit2 a unit 4 mozeme usudit, ze su si velmi podobne, avsak unit 2 je o trochu viac nakladovejsia a rizikovejsia. Taktiez vidime, ze pri nizsich hodnotach Premium je zavislost medzi Expenses a Premium pri jednotlivych unitach priblizne rovnaka, avsak neskor vidime, ze pravdepodobne najvynosnejsiou je unita7.
#3. Ak sa na to pozrieme podla segmentu, jasne vidime, ze velke segmenty su menej rizikove nez male segmenty, ktore pri tej istej hodnote Premium dosahuju vyssie hodnoty Expenses.
#4. Pri pohlade na Business mozeme usudit, ze najvacsi celkovy obnos Premium sa dosahuje v cestovani a na druhom mieste su domacnosti, avsak domacnosti su ovela viac rizikovejsie oproti cestovnemu biznisu, kedze maju strmsi sklon rastu zavislosti.
