selectTypeInput <- function(id, data) {
  types <- unique(data$ship_type)
  selectInput(NS(id, "type"), 
              "Pick a vassel type", 
              choices = c("All", sort(types)),
              selected = "All",
              multiple = FALSE)
}

selectTypeServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    reactive({input$type})
  })
}