library(shiny)
library(markdown)
library(shinyjs)
library(htmltools)

my_slider <- function() {
  slider_vals <- paste(c(paste0(seq(100,55,by=-5), "% A"), "50% A/B", paste0(seq(55,100,by=5), "% B")), collapse=",")

  # construct the input element
  input <- tag("input","")
  input <- tagAppendAttributes(input, class="js-range-slider", id="answer", 
                               `data-from`="10", `data-values`= slider_vals)
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
#      fixedPanel(
        wellPanel(
        plotOutput("score_plot", height="400px", click = clickOpts(id="score_click"),
hover = hoverOpts(
          id = "score_brush",
#          opacity = 0,
          delayType = "throttle"
        )),
        div(class="span6", style="text-align:center",
            my_slider()),
        div(class="span6", style="text-align:center",
            actionButton("question_num", "Next question"))
        )
#        width = "540px"
#      )
    )
  )
))

