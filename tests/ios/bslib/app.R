# iOS smoke test 2: minimal bslib page_sidebar (same page scaffold as the real app)
library(shiny); library(bslib)
ui <- page_sidebar(title = "Test 2: bslib sidebar", fillable = FALSE,
  sidebar = sidebar(selectInput("a", "Pick", letters[1:5])),
  card(card_header("Result"), textOutput("out")))
server <- function(input, output, session) output$out <- renderText(paste("You picked", input$a))
shinyApp(ui, server)
