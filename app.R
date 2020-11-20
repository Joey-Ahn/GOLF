library(shiny)
library(shinythemes)
library(RSQLite)
library(DTedit)
library(dplyr)
library(zoo) # rollmean
library(ggplot2)
############################################################################################################################################
conn <- dbConnect(RSQLite::SQLite(), "score_db.sqlite")
#...........................................................................................................................................
if(!'score_db' %in% dbListTables(conn)) {
  score <- read.csv('score.csv', stringsAsFactors = FALSE) %>%
    select(Date, Time, Course, Golfzon, Joey, Lucy, David, Steve, Jun)
  score$id <- 1:nrow(score)
  score$Date <- as.character(score$Date)
  score$Course[score$Golfzon == "Yes"] <- "Golfzon"
  score$Course <- as.character(score$Course)
  score$Joey <- as.integer(score$Joey)
  score$Lucy <- as.integer(score$Lucy)
  score$David <- as.integer(score$David)
  score$Steve <- as.integer(score$Steve)
  score$Jun <- as.integer(score$Jun)
  score[is.na(score)] <- 0
  dbWriteTable(conn, "score_db", score, overwrite = TRUE)
}
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
#...........................................................................................................................................
get.handicap <- function() {
  res <- dbSendQuery(conn, "SELECT * FROM score_db")
  handicap <- dbFetch(res)
  dbClearResult(res)
  handicap[handicap == 0] <- NA
  handicap <- arrange(handicap, desc(Date, Time))
  c = sapply(handicap[handicap$Golfzon == "No", 5:9], function(x) {length(x[!is.na(x)])})
  g = sapply(handicap[handicap$Golfzon == "Yes", 5:9], function(x) {length(x[!is.na(x)])})
  handicap <- rbind.data.frame(
    data.frame(
      Type = "Course",
      Joey = rollmean(handicap$Joey[!is.na(handicap$Joey) & handicap$Golfzon == "No"], ifelse(c[1] >= 5, 5, c[1]))[1],
      Lucy = rollmean(handicap$Lucy[!is.na(handicap$Lucy) & handicap$Golfzon == "No"], ifelse(c[2] >= 5, 5, c[2]))[1],
      David = rollmean(handicap$David[!is.na(handicap$David) & handicap$Golfzon == "No"], ifelse(c[3] >= 5, 5, c[3]))[1],
      Steve = rollmean(handicap$Steve[!is.na(handicap$Steve) & handicap$Golfzon == "No"], ifelse(c[4] >= 5, 5, c[4]))[1],
      Jun = rollmean(handicap$Jun[!is.na(handicap$Jun) & handicap$Golfzon == "No"], ifelse(c[5] >= 5, 5, c[5]))[1]
    ),
    data.frame(
      Type = "Golfzon",
      Joey = rollmean(handicap$Joey[!is.na(handicap$Joey) & handicap$Golfzon == "Yes"], ifelse(g[1] >= 5, 5, g[1]))[1],
      Lucy = rollmean(handicap$Lucy[!is.na(handicap$Lucy) & handicap$Golfzon == "Yes"], ifelse(g[2] >= 5, 5, g[2]))[1],
      David = rollmean(handicap$David[!is.na(handicap$David) & handicap$Golfzon == "Yes"], ifelse(g[3] >= 5, 5, g[3]))[1],
      Steve = rollmean(handicap$Steve[!is.na(handicap$Steve) & handicap$Golfzon == "Yes"], ifelse(g[4] >= 5, 5, g[4]))[1],
      Jun = rollmean(handicap$Jun[!is.na(handicap$Jun) & handicap$Golfzon == "Yes"], ifelse(g[5] >= 5, 5, g[5]))[1]
    )
  ) %>%
    mutate_if(is.numeric, round, 0)
  return(handicap)
}
#...........................................................................................................................................
score.insert.callback <- function(data, row) {
  query <- paste0("INSERT INTO score_db (Date, Time, Course, Golfzon, Joey, Lucy, David, Steve, Jun, id) VALUES (",
                  "'", as.character(data[row,]$Date), "', ",
                  "'", as.character(data[row,]$Time), "', ",
                  "'", data[row,]$Course, "', ",
                  "'", as.character(data[row,]$Golfzon), "', ",
                  "", data[row,]$Joey, ", ",
                  "", data[row,]$Lucy, ", ",
                  "", data[row,]$David, ", ",
                  "", data[row,]$Steve, ", ",
                  "", data[row,]$Jun, ", ",
                  "", max(get.score()$id) + 1, "",
                  ")")
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(get.score())
}
#...........................................................................................................................................
score.update.callback <- function(data, olddata, row) {
  query <- paste0("UPDATE score_db SET ",
                  "Date = '", as.character(data[row,]$Date), "', ",
                  "Time = '", as.character(data[row,]$Time), "', ",
                  "Course = '", data[row,]$Course, "', ",
                  "Golfzon = '", as.character(data[row,]$Golfzon), "', ",
                  "Joey = ", data[row,]$Joey, ", ",
                  "Lucy = ", data[row,]$Lucy, ", ",
                  "David = ", data[row,]$David, ", ",
                  "Steve = ", data[row,]$Steve, ", ",
                  "Jun = ", data[row,]$Jun, " ",
                  "WHERE id = ", data[row,]$id)
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(get.score())
}
#...........................................................................................................................................
score.delete.callback <- function(data, row) {
  query <- paste0('DELETE FROM score_db WHERE id = ', data[row,]$id)
  dbSendQuery(conn, query)
  return(get.score())
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
  handicap <- get.handicap()
  output$handicap <- renderTable(handicap, digits = 0)
  output$scorechart <- renderPlot({
    rbind.data.frame(
      cbind.data.frame(ID = c(10:1), Name = "Joey", Score = arrange(score[score$Joey > 0 & score$Golfzon == "No", ], desc(Date, Time))[c(1:10), 5]),
      cbind.data.frame(ID = c(10:1), Name = "Lucy", Score = arrange(score[score$Lucy > 0 & score$Golfzon == "No", ], desc(Date, Time))[c(1:10), 6]),
      cbind.data.frame(ID = c(10:1), Name = "David", Score = arrange(score[score$David > 0 & score$Golfzon == "No", ], desc(Date, Time))[c(1:10), 7]),
      cbind.data.frame(ID = c(10:1), Name = "Steve", Score = arrange(score[score$Steve > 0 & score$Golfzon == "No", ], desc(Date, Time))[c(1:10), 8]),
      cbind.data.frame(ID = c(10:1), Name = "Jun", Score = arrange(score[score$Jun > 0 & score$Golfzon == "No", ], desc(Date, Time))[c(1:10), 9])
    ) %>%
      arrange(ID) %>%
      ggplot(aes(x = ID, y = Score, group = Name, colour = Name)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values=c("#60bd68", "#faa43a", "#f15854", "#5da5da", "#b276b2")) +
      ylim(70, 130) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.background = element_rect(fill = "white", colour = "grey"),
            panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
            panel.grid.minor.y = element_line(colour = "grey", linetype = "dotted"),
            panel.grid.major.x = element_blank(),
            legend.title = element_blank(),
            legend.key = element_rect(fill = NA))
  })
}
############################################################################################################################################
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  h2('Handicap'),
  uiOutput('handicap'),
  hr(),
  h2('Score'),
  plotOutput('scorechart', height = 200, hover = hoverOpts(id ="plot_hover")),
  uiOutput('score')
)
############################################################################################################################################
shinyApp(ui = ui, server = server)
