library(shiny)
library(stringr)

shinyUI(fluidPage(
        fluidRow(
                column(4,
                       img(src = "swiftkey.jpg", height = 250, width = 400)
                ),
                column(8, 
                       br(),
                       br(),
                       br(),
                       br(),
                       h1(strong("Word Prediction Application")),
                       br(),
                       h4(strong("Instructions:")),
                       h5(em("Enter a text string in the text field on the left, then hit the \"Predict next word\" button."))
                       )
        ),
        fluidRow(column(12,
                        br())
        ),
        fluidRow(column(3, 
                        textInput("text_input", label = h4(strong("Enter text here:"))),
                        actionButton("predict_button", "Predict next word")
                        ),
                 column(6, 
                        h4(strong("Predicted next word:")),
                        wellPanel(style = "background-color: #ffffff;",
                                textOutput("prediction_output"),
                                tags$head(tags$style("#prediction_output{color: black;
                                         font-size: 14px;
                                                     }"
                                                )
                                        )
                                )
                        )
        )
)
)
