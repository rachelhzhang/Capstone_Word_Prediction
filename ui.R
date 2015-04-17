library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Word Prediction Algorithm"),
    sidebarPanel(
        textInput("inputText", "Please type an incomplete phrase, then hit Submit for a word prediction"),
        submitButton("Submit")
        ),
    mainPanel(
        h4("We predict your next word to be:"),
        verbatimTextOutput("wordSuggest"),
        p("For documentation on how to use this app, see: URL")
        )
    ))