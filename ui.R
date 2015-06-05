library(shiny)
library(markdown)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  shinyjs::useShinyjs(),

  # TODO: Make pretty

  titlePanel("Pub Quiz - how confident should you be?", "Pub Quiz Confidence"),
  hr(),

  sidebarLayout(
    sidebarPanel(includeMarkdown("instructions.md"),
                 htmlOutput("question")
    ),
    mainPanel(
      plotOutput("score_plot", height="400px"),
      wellPanel(
        sliderInput("answer",
                    "",
                    min = -100,
                    max = 100,
                    step = 1,
                    value = 0),
        actionButton("question_num", "Next question")
      )
    )
  )
))

