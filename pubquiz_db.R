library(RMySQL)

# fetch from the database if possible
read_scores <- function() {
  scores <- NULL

  try({
    conn <- dbConnect(RMySQL::MySQL(), user='pubquiz', password='pubquiz', host='localhost', dbname='pubquiz')

    scores <- dbGetQuery(conn, "SELECT * FROM scores")[,-1]

    dbDisconnect(conn)
  }, silent=TRUE)

  scores
}

# create the database if possible
create_database <- function(num_questions) {
  success <- FALSE

  try({
    conn <- dbConnect(RMySQL::MySQL(), user='pubquiz', password='pubquiz', host='localhost', dbname='pubquiz')

    # create score table if not already there
    fields <- paste(c("score_id INT PRIMARY KEY AUTO_INCREMENT", sprintf("q%02d INT", 1:num_questions), "first_round INT"), collapse=",")

    res <- dbSendQuery(conn, paste("CREATE TABLE IF NOT EXISTS scores (", fields, ");"))
    dbClearResult(res)

    dbDisconnect(conn)

    success <- TRUE
  }, silent=TRUE)

  success
}

# write score to the database if possible
write_score <- function(score) {
  success <- FALSE

  try({
    conn <- dbConnect(RMySQL::MySQL(), user='pubquiz', password='pubquiz', host='localhost', dbname='pubquiz')
 
    cols <- paste(c(sprintf("q%02d", 1:(length(score)-1)),"first_round"), collapse=",")
    vals <- paste(score, collapse=",")
    sql  <- paste("INSERT INTO scores(",cols,") VALUES(",vals,");")

    res <- dbSendQuery(conn, sql)
    dbClearResult(res)

    dbDisconnect(conn)
    success <- TRUE
  }, silent=TRUE)

  success
}
