library(shiny)
library(markdown)
library(shinyjs)
library(htmltools)

my_slider <- function() {
  slider_vals <- paste(c(paste0(100:51, "% A"), "50% A/B", paste0(51:100, "% B")), collapse=",")

  # construct the input element
  input <- tag("input","")
  input <- tagAppendAttributes(input, class="js-range-slider", id="answer", 
                               `data-from`="50", `data-values`= slider_vals)
  sliderTag <- div(class="form-group shiny-input-container", input)
  dep <- htmlDependency("ionrangeslider", "2.0.6", c(href = "shared/ionrangeslider"), 
            script = "js/ion.rangeSlider.min.js", stylesheet = c("css/normalize.css", 
            "css/ion.rangeSlider.css", "css/ion.rangeSlider.skinShiny.css"))
  attachDependencies(sliderTag, dep)
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  shinyjs::useShinyjs(),

  fluidRow(
    column(width=7,
      wellPanel(
        includeMarkdown("instructions.md"),
        hr(),
        htmlOutput("question")
      )
    ),
    column(width=5,
      plotOutput("score_plot", height="400px"),
      wellPanel(
        my_slider(),
        div(class="span6", style="text-align:center",
            actionButton("question_num", "Next question", style="text-align:right")
        )
      )
    )
  )
))

