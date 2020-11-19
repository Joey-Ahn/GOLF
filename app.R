library(shiny)
library(DTedit)
############################################################################################################################################
conn <- dbConnect(RSQLite::SQLite(), "score_db.sqlite")
#...........................................................................................................................................
get.score <- function() {
  res <- dbSendQuery(conn, "SELECT * FROM score_db")
  score <- dbFetch(res)
  dbClearResult(res)
  score$Date <- as.Date(score$Date)
  score$Course[score$Golfzon == "Yes"] <- "Golfzon"
  score$Course <- as.character(score$Course)
  score$Golfzon <- as.factor(score$Golfzon)
  score$Time <- as.factor(score$Time)
  score$Joey <- as.integer(score$Joey)
  score$Lucy <- as.integer(score$Lucy)
  score$David <- as.integer(score$David)
  score$Steve <- as.integer(score$Steve)
  score$Jun <- as.integer(score$Jun)
  score[is.na(score)] <- 0
  return(score)
}
############################################################################################################################################
server <- function(input, output) {
  score <- get.score() %>%
    arrange(desc(Date, Time))
  dtedit(input, output,
         name = 'score',
         thedata = score,
         edit.cols = c("Date", "Time", "Course", "Golfzon", "Joey", "Lucy", "David", "Steve", "Jun"),
         edit.label.cols = c("Date", "AM/PM", "Course", "Golfzon", "Joey", "Lucy", "David", "Steve", "Jun"),
         input.types = c(Course='textInput'),
         view.cols = names(score)[c(1, 2, 3, 5, 6, 7, 8, 9)],
         callback.update = score.update.callback,
         callback.insert = score.insert.callback,
         callback.delete = score.delete.callback)
}
############################################################################################################################################
ui <- fluidPage(
  uiOutput('score')
)
############################################################################################################################################
shinyApp(ui = ui, server = server)
