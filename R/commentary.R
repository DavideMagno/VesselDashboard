CommentaryUI <- function(id) {
  div(class = "ui raised segment",
      div(a(class = "ui green ribbon label", "Info"),
          textOutput(NS(id, "commentary"))))
}

CommentaryServer <- function(id, input_from_calculation, type) {
  moduleServer(id, function(input, output, session) {
    
    comment <- reactive({
      if (nrow(input_from_calculation()) <= 2) {
        
        type <- type()
        vessel <- unique(input_from_calculation()$SHIPNAME)
        time_start <- input_from_calculation()$DATETIME[1]
        port <- unique(input_from_calculation()$port)
        
        if (nrow(input_from_calculation()) == 2) {
          
          time_end <- input_from_calculation()$DATETIME[2]
          meters <- unique(input_from_calculation()$DIST)
          glue::glue('The {type} vessel {vessel} has navigated \\
                     {round(meters,0)} meters between \\
                     {format(time_start, "%I:%M %p")} and \\
                     {format(time_end, "%I:%M %p")} on \\
                     {format(time_start, "%A, %B %e, %Y")} \\
                     in the maritime space of the port of {port}')
        } else {
          
          glue::glue('The {type} vessel {vessel} has always been parked in \\
                     the port of {port}. The last observation available is at \\
                     {format(time_start, "%I:%M %p")} on \\
                     {format(time_start, "%A, %B %e, %Y")}')
        }
        
      } else {
        
        type <- ifelse(grepl("All", type()),"",type())
        glue::glue('The maps below represent the latest observation \\
                   available for all the {type} vessels.  Their positions have been \\
                   split by the maritime space in which they have been \\
                   reported last')
      }
    })
    
    output$commentary <- renderText(comment())
  })
}