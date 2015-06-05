library(shiny)
library(shinyjs)
library(markdown)

saved_scores <- read.csv("scores.csv") # we'll update this as we go...
answers <- read.table("answers.txt", sep=" ") == "B"
num_questions <- min(2, length(answers))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  cat(isolate(names(input)), "\n")

  colorFunc <- function(x, alpha=1) {
    cf <- colorRamp(c("red", "grey80", "blue"), space="Lab")
    rgb(t(col2rgb(ifelse(x < 0.5, "red", "blue"))), alpha=alpha*255, maxColorValue=255)
  }

  colorFunc2 <- function(x, alpha=1) {
    cf <- colorRamp(c("red", "grey80", "blue"), space="Lab")
    rgb(cf(x), alpha=alpha*255, maxColorValue=255)
  }

  v <- reactiveValues(question_num = 1, show_answer = 0, show_summary = FALSE, scores = saved_scores)

  observeEvent(input$question_num, {
    num <- input$question_num %% (num_questions*2 + 1)
    v$show_summary <- (num == num_questions*2)
    v$question_num <- num %/% 2 + 1
    v$show_answer  <- num %% 2
    cat("Observed a question_num event:", isolate(input$question_num), "show_answer:", isolate(v$show_answer), "question_num:", isolate(v$question_num), "\n")
    if (v$show_answer) {
      # save the user answer
      if (v$question_num == 1) {
        v$scores <- rbind(v$scores, rep(NA, num_questions))
      }
      v$scores[nrow(v$scores),v$question_num] <- input$answer
      # save results to disk for posterity
      if (v$question_num == num_questions) {
        write.csv(v$scores, "scores.csv", row.names=F)
      }
    }
    # TODO: Add a summary plot
  })

  # toggle the slider state
  observe({
    shinyjs::toggleState("answer", v$show_answer == 0 && !v$show_summary)
    get_next_text <- function(v) {
      if (v$show_summary) {
        "Next student"
      } else if (v$show_answer) {
        if (v$question_num == num_questions) {
          "Show Summary"
        } else {
          "Next question"
        }
      } else {
        "Submit Answer"
      }
    }
    cat(isolate(get_next_text(v)))
    shinyjs::text("question_num", get_next_text(v))
  })

  # TODO: Save the scores in case we need to re-run it after each game cycle

  score <- function(x, answer) {
    1-(answer-(x+100)/200)^2
  }

  output$score_plot <- renderPlot({
    if (v$show_summary) {

      totals <- apply(v$scores, 1, function(x) { sum(score(x, answers)) })
      hist(totals, xlim=c(0, num_questions), main="Total score compared with others", col="grey70", border=NA, xlab="")
      abline(v=totals[length(totals)], col="red", lwd=2)

    } else if (v$show_answer == 1) {
      # 2 plots, one for your answer, one for histogram of previous answers
      par(mfrow=c(1,2))

      breaks <- seq(-100,100,by=10)
      cols <- colorFunc2((breaks[-1] - 5 + 100)/200, 1)
      if (!answers[v$question_num])
        cols <- rev(cols)
      hist(v$scores[,v$question_num], breaks=breaks, xlim=c(-100,100), main="Your answer compared with others", col=cols, border=NA, xlab="")
      abline(v=input$answer, col="black", lwd=2)

      by <- 0.05
      breaks <- seq(0,1,by=by)
      cols = colorFunc2(1 - sqrt(1-breaks[-1]+by/2),1)
      hist(score(v$scores[,v$question_num], answers[v$question_num]), breaks=breaks, xlim=c(0, 1), main="Your score compared with others", col=cols, border=NA, xlab="")
      abline(v=score(input$answer, answers[v$question_num]), col="black", lwd=2)

    } else {
      par(mai=rep(0.1,4))
      plot(NULL, xlab="", ylab="", xlim=c(0,100), ylim=c(0,100), main="", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
      rect(0, 0, (input$answer+100)/2, 100, col=colorFunc((input$answer+100)/200, 0.5), border=NA)
      rect(0, 0, 100, (input$answer+100)/2, col=colorFunc((input$answer+100)/200, 0.5), border=NA)
      rect(0, (input$answer+100)/2, 100, 100, col=colorFunc(1-(input$answer+100)/200, 0.5), border=NA)
      rect((input$answer+100)/2, 0, 100, 100, col=colorFunc(1-(input$answer+100)/200, 0.5), border=NA)
      legend("topleft", legend=c("Penalty if correct", "Penalty if incorrect"), fill=c("red", "blue"))
    }
  })

  output$question <- renderUI({
    if (v$show_summary) {
      includeMarkdown("summary.md")
    } else if (v$show_answer) {
      includeMarkdown(paste0("answer", v$question_num, ".md"))
    } else {
      includeMarkdown(paste0("question", v$question_num, ".md"))
    }
  })
})
