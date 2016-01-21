shinyUI(fluidPage(
  
  # Copy the line below to make a text input box
  textInput("text", label = h3("Text input"), value = "Enter text..."),
  
  hr(),
  h3("Predicted word"),
  fluidRow(column(3, verbatimTextOutput("value")))
  
))