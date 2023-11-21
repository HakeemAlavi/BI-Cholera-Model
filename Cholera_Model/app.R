library(shiny)
library(caret)

# Load the logistic regression model
model <- readRDS("cholera_model_two.rds")

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;500;600;700&display=swap');
      body {
        font-family: 'Poppins', sans-serif;
        font-size: 16px;
      }
      .input-label {
        font-weight: normal;
        font-size: 14px;
        margin-bottom: 5px;
      }
    "))
  ),
  br(),
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #3deb6c; padding: 20px; border-radius: 5px; box-shadow: 2px 2px 5px #888888;",
      # Add input fields for the independent variables
      
      br(),
      tags$p(style = "font-weight: bold;", "Disclaimer"),
      tags$p("The AI model's diagnosis has an accuracy of 67%."),
      tags$p("Use the diagnosis as an informed guess and visit a doctor for further treatment."),
      tags$p("Please fill in the form to receive a cholera classification based on your respective symptoms."),
      br(),
      numericInput("age", tags$label("Age", class = "input-label"), value = 25, min = 0, max = 120),
      tags$label("Gender", class = "input-label"),
      selectInput("male", NULL, choices = c("Female", "Male"), selected = "Male"),
      tags$label("Vomiting", class = "input-label"),
      selectInput("vomiting", NULL, choices = c("No", "Yes"), selected = "No"),
      tags$label("Muscle Cramps", class = "input-label"),
      selectInput("muscleCramps", NULL, choices = c("No", "Yes"), selected = "No"),
      tags$label("Rapid Heart Rate", class = "input-label"),
      selectInput("rapidHeartRate", NULL, choices = c("No", "Yes"), selected = "No"),
      tags$label("Watery Diarrhoea", class = "input-label"),
      selectInput("wateryDiarrhoea", NULL, choices = c("No", "Yes"), selected = "No"),
      tags$label("Dehydration", class = "input-label"),
      selectInput("dehydration", NULL, choices = c("No", "Yes"), selected = "No"),
      tags$label("Education Level", class = "input-label"),
      selectInput("education", NULL, choices = c("Weak", "Average", "Good", "Exceptional"), selected = "Primary"),
      br(),
      actionButton("submit", 
                   "Submit", 
                   style = "background-color: #ffffff; 
                      color: #3deb6c; 
                      width: 100%; 
                      border-radius: 5px; 
                      padding: 10px; 
                      font-weight: bold;
                      transition: background-color 0.3s, color 0.3s;",
                   class = "submit-button"
      ),
      br(),
    ),
    mainPanel(
      # Display the model's prediction
      div(style = "padding-left: 150px; padding-top: 10px; font-weight: bold;",
          h3("Cholera Diagnosis", style = "color: #3deb6c; font-weight: bold;"),
          textOutput("prediction")
      )
    )
  )
)

server <- function(input, output, session) {
  # Install the glmnet package if it's not already installed
  if (!require("glmnet")) {
    install.packages("glmnet")
  }
  observeEvent(input$submit, {
    # Display "Testing" on button click
    print("Button clicked")
    
    # Check if the model is loaded
    if (!exists("model")) {
      output$prediction <- renderText({
        "Model not loaded"
      })
      return()
    }
    
    # Check if the inputs are captured
    print("Capturing inputs")
    print(input)
    
    # Extract input values
    age <- as.numeric(input$age)
    vomiting <- ifelse(input$vomiting == "Yes", 1, 0)
    muscleCramps <- ifelse(input$muscleCramps == "Yes", 1, 0)
    rapidHeartRate <- ifelse(input$rapidHeartRate == "Yes", 1, 0)
    male <- ifelse(input$male == "Male", 1, 0)
    education <- match(input$education, c("Weak", "Average", "Good", "Exceptional"))
    wateryDiarrhoea <- ifelse(input$wateryDiarrhoea == "Yes", 1, 0)
    dehydration <- ifelse(input$dehydration == "Yes", 1, 0)
    
    # Check the classes of the variables after conversion
    print(class(age))
    print(class(vomiting))
    print(class(muscleCramps))
    print(class(rapidHeartRate))
    print(class(male))
    print(class(education))
    print(class(wateryDiarrhoea))
    print(class(dehydration))
    
    # Make the prediction using the loaded model
    tryCatch({
      print("Making prediction")
      prediction <- predict(model, data.frame(age, vomiting, muscleCramps, rapidHeartRate, male, education, wateryDiarrhoea, dehydration))
      
      # Convert the result to "Yes" or "No"
      result <- ifelse(prediction == 1, "Yes", "No")
      
      # Display the prediction
      output$prediction <- renderText({
        paste("Result: ", result)
      })
    }, error = function(e) {
      output$prediction <- renderText({
        paste("An error occurred: ", conditionMessage(e), ". Please try again.")
      })
    })
  })
}

# Run the application
shinyApp(ui, server)