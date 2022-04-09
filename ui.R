

# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Admission Prediction'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Insert your data for prediction :)')),
    numericInput("gre_score", 
                 label = "GRE Score ( Enter in range 0-340 ) ", 
                 value = 150),
    numericInput("toefl_score", 
                 label = "TOEFL Score ( Enter in range 0-120 )", 
                 value = 100),
    numericInput("university_rating", 
                 label = "University Rating ( Enter in range 1-5 )", 
                 value = 3),
    numericInput("sop_rating", 
                 label = "SOP Rating ( Enter in range 1-5 )", 
                 value = 3.5),
    numericInput("lor_rating", 
                 label = "LOR Rating ( Enter in range 1-5 )", 
                 value = 2.3),
    numericInput("cgpa", 
                 label = "CGPA ( Enter in range 0-10 )", 
                 value = 9.89),
    numericInput("research", 
                 label = "Research ( Enter 0 for no - 1 for yes  )", 
                 value = 1),
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  
  # Input Data
  datasetInput <- reactive({  
    PREDICTION = ''
    GRE_Score = c(input$gre_score)
    TOEFL_Score = c(input$toefl_score)
    University_Rating = c(input$university_rating)
    SOP = c(input$sop_rating)
    LOR = c(input$lor_rating)
    CGPA = c(input$cgpa)
    Research = c(input$research)
    
    datas = data.frame(GRE_Score,TOEFL_Score,University_Rating,SOP,LOR,CGPA,Research)
    
    print(datas)
    model <- readRDS("model.rds")
    result = predict(model,datas)*100
    PREDICTION = paste('Your chance of getting addmission is',format(round(result, 2), nsmall = 2),'%')
    
    Output <- data.frame(PREDICTION)
    

  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })

  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)