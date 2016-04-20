library(shiny)

## Define UI for getting input phrase
shinyUI(navbarPage("An App for Next Word Predictions",
        tabPanel("About",
          includeHTML("Documentation.html")
                 ),
        tabPanel("Application",
          sidebarPanel(
            textInput("inphrase", label = "TYPE YOUR PHRASE HERE"),
            submitButton("Submit")
                      ),
          mainPanel(
            textOutput("pred1"),
            textOutput("pred2"),
            textOutput("pred3"),
            textOutput("message")
                    ))
))