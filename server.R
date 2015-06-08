library(shiny)
library(shinyjs)
library(markdown)

source("pubquiz_db.R")
source("score_plot.R")

# read in questions
questions <- read.csv("questions.csv", stringsAsFactors=FALSE)
num_questions <- nrow(questions)
num_rounds    <- 2

if (!create_database(num_questions)) {
  cat("Unable to create database\n", file=stderr());
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  v <- reactiveValues(round = 1,
                      question_num = 1,
                      show_answer = 0,
                      show_summary = FALSE,
                      scores = read_scores(),
                      round_order    = sample(1:2,2,replace=FALSE),
                      question_order = c(sample(1:(num_questions/2),replace=FALSE),sample(1:(num_questions/2),replace=FALSE)),
                      answers = sample(0:1,num_questions,replace=TRUE))

  click <- reactiveValues(question_num = 0)

  current_question <- function() {
    v$question_order[v$question_num] + (v$round_order[v$round]-1)*num_questions/2
  }

  transform_slider <- function(x) {
    x / 20
  }

  observeEvent(input$question_num, {
    click$question_num <- click$question_num + 1; #input$question_num
  })

  observeEvent(click$question_num, {
    num <- click$question_num %% (num_questions*2 + 1)
    v$show_summary <- (num == num_questions*2)
    v$round        <- (num >= num_questions) + 1
    v$question_num <- num %/% 2 + 1
    v$show_answer  <- num %% 2
    if (v$show_summary) {
      # re-randomise the questions and answers
      v$round_order    <- sample(1:2,2,replace=FALSE)
      v$question_order <- c(sample(1:(num_questions/2),replace=FALSE),sample(1:(num_questions/2),replace=FALSE))
      v$answers        <- sample(0:1,num_questions,replace=TRUE)
    }
    if (v$show_answer) {
      # save the user answer
      if (v$question_num == 1) {
        v$scores <- rbind(v$scores, rep(NA, num_questions+1))
        v$scores[nrow(v$scores), num_questions+1] <- v$round_order[1]
      }
      score <- transform_slider(input$answer) * 200-100
      if (v$answers[v$question_num] == 0)
        score <- -score
      v$scores[nrow(v$scores),current_question()] <- score
      # save results to disk for posterity
      if (v$question_num == num_questions) {

        # write scores into the database
        if (!write_score(v$scores[nrow(v$scores),])) {
          cat("Unable to write score to database\n", file=stderr())
        }

      }
    } else {
      # reset the slider
#      shinyjs::reset("answer")
    }
  })

  observeEvent(input$score_click, {
    cat("click! x=", isolate(input$score_click$x), "y=", isolate(input$score_click$y), "\n")

# record answer and go to the next one straight away
    updateSliderInput(session, "answer", value=round(input$score_click$x*20))
    click$question_num <- click$question_num + 1

#    click$last <- input$score_click$x
  })

  observeEvent(input$score_brush, {
    cat("brush! xmin=", isolate(names(input$score_brush)), "\n")
#"xmax=", 
#isolate(input$score_brush$xmax), "\n")
#    cat("brush! names=", isolate(names(input$score_brush)), "\n")
#    cat("brush! clicknames=", isolate(names(input$score_click)), "\n")
 #   if (length(input$score_click) > 0) {
#    if (input$score_brush$xmin == click$last) { #input$score_click$x) {
#      x <- input$score_brush$xmax
#    } else {
#      x <- input$score_brush$xmin
#    }
  updateSliderInput(session, "answer", value=round(input$score_brush$x*20))
  #  }
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
    shinyjs::text("question_num", get_next_text(v))
  })

  colorFunc5 <- function(x, alpha=1) {
    cf <- colorRamp(c("red", "grey80", "blue"), space="Lab")
    rgb(cf(x), alpha=alpha*255, maxColorValue=255)
  }

  score <- function(x, round = 2) {
    if (round == 1) {
      (x+100)/200
    } else {
      1-(1-(x+100)/200)^2
    }
  }

  question_header <- function() {
    h2(paste("Question", ifelse(v$round == 1, v$question_num,
                                              v$question_num - num_questions/2)))
  }

  output$score_plot <- renderPlot({
    if (v$show_summary) {

      totals <- apply(v$scores[,1:num_questions, drop=FALSE], 1, function(x) { sum(score(x)) })
      breaks <- seq(0,1,length.out=16)
      cols = colorFunc5(1 - sqrt(1-breaks[-1]),1)

#      outfile <- tempfile(fileext=".png")
#      png(outfile, bg="transparent", width=session$clientInfo$output_score_plot_width, height=session$clientInfo$output_score_plot_height)
      hist(totals, xlim=c(0, num_questions), breaks=breaks*num_questions,
           main="Total score compared with others", col=cols, border=NA, xlab="",xaxt="n")
      abline(v=totals[length(totals)], col="black", lwd=2)
      axis(side=1, at=seq(0,num_questions,length.out=5), labels=seq(-10,10,length.out=5)*num_questions)
#      dev.off()

#      list(src = outfile,
#           alt = "Summary of scores");

    } else if (v$show_answer == 1) {

      breaks <- seq(0,1,length.out=16)
      if (v$round == 1) {
        cols = colorFunc5(breaks[-1],1)
      } else {
        cols = colorFunc5(1 - sqrt(1-breaks[-1]),1)
      }
      sc <- v$scores[,current_question()]

 #     outfile <- tempfile(fileext=".png")
 #     png(outfile, bg="transparent", width=session$clientInfo$output_score_plot_width, height=session$clientInfo$output_score_plot_height)
      hist(score(sc, v$round), breaks=breaks, xlim=c(0, 1),
           main="Your score compared with others", col=cols, border=NA, xlab="", xaxt="n")
      abline(v=score(sc[length(sc)], v$round), col="black", lwd=2)
      axis(side=1, at=seq(0,1,by=0.25), labels=seq(-10,10,by=5))
 #     dev.off()

 #     list(src = outfile,
 #          alt = paste("Score for", question_header()));

    } else {

#      outfile <- sprintf("round%d_%03.0f.png", v$round, transform_slider(input$answer)*100)

 #     outfile <- tempfile(fileext=".png")
 #     cat("names",isolate(names(session$clientInfo)), "\n", file=stderr())
 #     png(outfile, bg="transparent", width=session$clientInfo$output_score_plot_width, height=session$clientInfo$output_score_plot_height)

      score_plot(transform_slider(input$answer), v$round)

  #    dev.off()

  #    list(src = file.path("plots", outfile),
  #         alt = question_header());
    }
  }) #, deleteFile = TRUE) #(v$show_summary || v$show_answer == 1))

  question_header <- function() {
    h2(paste("Question", ifelse(v$round == 1, v$question_num,
                                              v$question_num - num_questions/2)))
  }

  output$question <- renderUI({
    question <- questions[current_question(),]
    if (v$show_summary) {
      includeMarkdown("summary.md")
    } else if (v$show_answer) {
      score <- v$scores[nrow(v$scores), current_question()]
      div(
        includeMarkdown(paste0("round",v$round,".md")),
        question_header(),
        p(strong(ifelse(score > 0, "Right!", ifelse(score == 0, "Hedging your bets, huh?", 
"Wrong!")))),
        p(paste("A:", question[2+v$answers[v$question_num]]),
          strong(question[4+v$answers[v$question_num]])),
        p(paste("B:", question[2+1-v$answers[v$question_num]]),
          strong(question[4+1-v$answers[v$question_num]]))
      )
    } else {
      div(
        includeMarkdown(paste0("round",v$round,".md")),
        question_header(),
        p(question[1]),
        p(paste("A:", question[2+v$answers[v$question_num]])),
        p(paste("B:", question[2+1-v$answers[v$question_num]]))
      )
    }
  })
})
