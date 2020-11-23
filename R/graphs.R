GraphsUI <- function(id){
  plotly::plotlyOutput(NS(id, "graph"))
}

GraphsServer <- function(id, data, type, title, y_axis_title) {
  
  moduleServer(id, function(input, output, session) {
    
    # Checks if only one vessel is selected or multiple ones
    check <- reactive(ifelse(length(unique(data()$SHIPNAME)) > 1, TRUE, FALSE))
      
    # Prepares the input for the graph only if multiple vassels are selected
    table <- reactive({
      if (check()){
        if (grepl("Count",type)) {
          data() %>% 
            dplyr::count(port, ship_type)
        } else {
          data() %>% 
            dplyr::group_by(port, ship_type) %>% 
            dplyr::summarise(!!rlang::sym(type) := sum(!!rlang::sym(type), 
                                                       na.rm = TRUE),
                             .groups = 'drop')
        }
      }})
    
    # Generalised variable for the plotting
    var_y <- ifelse(grepl("Count",type), "n", type)
    
    # Number of colors of the palette
    n_colors <- reactive(length(unique(table()$ship_type)))
    
    # Plotly graph. The specs are different depending on the number of vessel types.
    # The graph is empty if only one vessel is selected
    output$graph <- plotly::renderPlotly({
      if (check()) {
        if (n_colors() > 1) {
          p <- table()  %>% 
            plotly::plot_ly(x = ~port, y = ~get(var_y), type = 'bar',
                            color = ~ship_type,  
                            colors = RColorBrewer::brewer.pal(n_colors(), "RdYlBu"))
        } else {
          p <- table()  %>% 
            plotly::plot_ly(x = ~port, y = ~get(var_y), type = 'bar',
                            color = ~ship_type, colors = "orange")
        }
        
        p <- p %>% 
          plotly::layout(title = paste("Total", title, "of vessels in the port"), 
                         yaxis = list(title = y_axis_title), 
                         barmode = 'stack')
      } else {
        p <- plotly::plotly_empty(type = "scatter", mode = "markers")
      }
      p
    })
  })
}
