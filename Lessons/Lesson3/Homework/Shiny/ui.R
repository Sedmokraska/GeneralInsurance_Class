#nacitanie dat
setwd("C:/Users/Simi/Documents/GeneralInsurance_Class/Data")
dt_KPI <- read.csv("lesson2_KPI.csv")

#korekcia dat, odstranenie zaznamov s NA hodnotou
data_pracovne <- na.omit(dt_KPI) 

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Zavislost Expenses od Premium"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("vyber", "Colouring Var", 
                  choices=colnames(data_pracovne[,1:5]),
                  selected = 1),
      hr(),
      helpText("Data pochadzaju z GeneralInsurance_Class z priecinka Data")
    ),
    
    # Create a spot for the ggplot
    mainPanel(
      plotOutput("Plot")  
    )
  )
)
