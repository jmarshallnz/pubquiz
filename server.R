library(shiny)
library(shinyjs)
library(markdown)
library(RMySQL)

# read in questions
questions <- read.csv("questions.csv", stringsAsFactors=FALSE)
num_questions <- nrow(questions)
num_rounds    <- 2

# fetch the database if possible
mysql <- FALSE
saved_scores <- NULL

try({
  conn <- dbConnect(RMySQL::MySQL(), user='pubquiz', password='pubquiz', host='localhost', dbname='pubquiz')

  # create score table if not already there
  fields <- paste(c("score_id INT", sprintf("q%02d INT", 1:num_questions), "first_round INT"), collapse=",")

  dbSendQuery(conn, paste("CREATE TABLE IF NOT EXISTS scores (", fields, ");"))

  # retrieve saved scores
  saved_scores <- fetch(dbSendQuery(conn, "SELECT * FROM scores"))[,-1]

  mysql <- TRUE
}, silent=TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  v <- reactiveValues(round = 1,
                      question_num = 1,
                      show_answer = 0,
                      show_summary = FALSE,
                      scores = saved_scores,
                      round_order    = sample(1:2,2,replace=FALSE),
                      question_order = c(sample(1:(num_questions/2),replace=FALSE),sample(1:(num_questions/2),replace=FALSE)),
                      answers = sample(0:1,num_questions,replace=TRUE))

  current_question <- function() {
    v$question_order[v$question_num] + (v$round_order[v$round]-1)*num_questions/2
  }

  observeEvent(input$question_num, {
    num <- input$question_num %% (num_questions*2 + 1)
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
      score <- input$answer*2-100
      if (v$answers[v$question_num] == 0)
        score <- -score
      v$scores[nrow(v$scores),current_question()] <- score
      # save results to disk for posterity
      if (v$question_num == num_questions) {

        # write scores into the database
          cols <- paste(c(sprintf("q%02d", 1:num_questions),"first_round"), collapse=",")
          vals <- paste(v$scores, collapse=",")
          sql  <- paste("INSERT INTO scores(",cols,") VALUES(",vals,");")
          cat("running: ", sql, "\n")
        if (mysql) {
          dbSendQuery(conn, sql)
        }

      }
    } else {
      # reset the slider
      shinyjs::reset("answer")
    }
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

  colorFunc2 <- function(x, alpha=1) {
    cf <- colorRamp(c("red", "grey80", "blue"), space="Lab")
    rgb(t(col2rgb(ifelse(x <= 0.5, "red", "blue"))), alpha=alpha*255, maxColorValue=255)
#   rgb(cf(x), alpha=alpha*255, maxColorValue=255)
  }

  colorFunc3 <- function(x, alpha=1) {
    cf <- colorRamp(c("red", "blue"), space="Lab")
    rgb(cf(x), alpha=alpha*255, maxColorValue=255)
  }

  colorFunc <- function(x, alpha=1) {
    y <- 1/(1 + exp(-250*(x-0.5)))
    colorFunc3(y, alpha)
  }

  colorFunc4 <- function(x, alpha=1) {
    cf <- colorRamp(c("red", "grey80", "blue"), space="Lab")
    y <- 1/(1 + exp(-250*(x-0.5)))
    rgb(cf(y), alpha=alpha*255, maxColorValue=255)
  }

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

  output$score_plot <- renderPlot({
    if (v$show_summary) {

      totals <- apply(v$scores[,1:num_questions, drop=FALSE], 1, function(x) { sum(score(x)) })
      breaks <- seq(0,1,length.out=16)
      cols = colorFunc5(1 - sqrt(1-breaks[-1]),1)
      hist(totals, xlim=c(0, num_questions), breaks=breaks*num_questions,
           main="Total score compared with others", col=cols, border=NA, xlab="")
      abline(v=totals[length(totals)], col="black", lwd=2)

    } else if (v$show_answer == 1) {

      breaks <- seq(0,1,length.out=16)
      if (v$round == 1) {
        cols = colorFunc5(breaks[-1],1)
      } else {
        cols = colorFunc5(1 - sqrt(1-breaks[-1]),1)
      }
      sc <- v$scores[,current_question()]

      hist(score(sc, v$round), breaks=breaks, xlim=c(0, 1),
           main="Your score compared with others", col=cols, border=NA, xlab="")
      abline(v=score(sc[length(sc)], v$round), col="black", lwd=2)

    } else {

      par(mai=rep(0.1,4))
      plot(NULL, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1), main="", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
      w <- input$answer/100

      # background colouring
      x <- seq(0,1,by=0.01)
      y1 <- 1-x^2
      y2 <- 1-(1-x)^2

      if (v$round == 1) {
        # background
        polygon(c(0,0,0.5,1,1),c(0.5,1,0.5,1,0.5), col=colorFunc(1,0.2), border=NA)
        polygon(c(0,0,0.5,1,1),c(0.5,0,0.5,0,0.5), col=colorFunc(0,0.2), border=NA)

        lines(c(0,0.5,1),c(0,0.5,0),lwd=2, col=colorFunc(0,1))
        lines(c(1,0.5,0),c(1,0.5,1),lwd=2, col=colorFunc(1,1))
        lines(c(0,1),c(0.5,0.5),lwd=1)

        points(w,1-w,bg=colorFunc4(1-w,1),pch=21,cex=4)
        points(w,w,bg=colorFunc4(w,1),pch=21,cex=4)
      } else {
        # background
        y3 <- (y1+y2)/2
        polygon(c(0,x[1:51],x[51:1],0),c(0.5,y1[1:51],y3[51:1],0.5), col=colorFunc(1,0.2), border=NA)
        polygon(c(0,x[1:51],x[51:1],0),c(0.5,y2[1:51],y3[51:1],0.5), col=colorFunc(0,0.2), border=NA)
        polygon(c(1,x[101:51],x[51:101],1),c(0.5,y1[101:51],y3[51:101],0.5), col=colorFunc(0,0.2), border=NA)
        polygon(c(1,x[101:51],x[51:101],1),c(0.5,y2[101:51],y3[51:101],0.5), col=colorFunc(1,0.2), border=NA)

        lines(x, pmax(y1,y2), lwd=2, col=colorFunc(1,1))
        lines(x, pmin(y1,y2), lwd=2, col=colorFunc(0,1))
        lines(x, y3, lwd=1)

        points(w,1-w^2,bg=colorFunc4(1-w,1),pch=21,cex=4)
        points(w,1-(1-w)^2,bg=colorFunc4(w,1),pch=21,cex=4)
      }

      legend("bottom", legend=c("Score if correct", "Score if incorrect"), fill=c("blue", "red"), bty="n")
    }
  })

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

if (mysql) {
  dbDisconnect(conn)
}

