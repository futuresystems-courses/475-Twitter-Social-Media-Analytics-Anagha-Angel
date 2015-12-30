#Load Shiny Package
library("shiny")


#Define the UI
shinyUI(
  fluidPage(
    #Application Title
    titlePanel(title="I590 - Final Project by Anagha Vyas and Angel Rangel"),
    
    fluidRow(
      #Sidebar with an input text field and sliders to set word frequency
      column(2,titlePanel="Select the Hashtag",
                   textInput("hashtagIn","Hashtag", value="#Appian"),
                   actionButton("update", "Change"),
                   hr(),
                   sliderInput("freq",
                               "Minimum Frequency:",
                               min = 1,  max = 40, value = 2),
                   sliderInput("max",
                               "Maximum Number of Words:",
                               min = 1,  max = 400,  value = 200)
      ),
      column(10, 
        tabsetPanel(
          position=c("right"),
          tabPanel(
            strong("Plots"),
            br(),
            tags$br(),
            plotOutput("plot",width = "100%", height = "400px"),
            br()
          ),
          tabPanel(
            strong("Tweets"),
            tableOutput("mytable1")
          ),
          
         tabPanel(
            strong("Word Count"),
            tableOutput("mytable2")
          )
          
        )
      )
    )
  )
)

