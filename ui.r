shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  h2('Handicap'),
  uiOutput('handicap'),
  hr(), 
  h2('Score'),
  plotOutput('scorechart', height = 200, hover = hoverOpts(id ="plot_hover")),
  uiOutput('score')
))