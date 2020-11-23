GraphsUI <- function(id){
  plotly::plotlyOutput(NS(id, "graph"))
}

GraphsServer <- function(id, data, type, title, y_axis_title) {
  
  moduleServer(id, function(input, output, session) {
    table <- reactive({
      if (grepl("Count",type)) {
      data() %>% 
        dplyr::count(port, ship_type)
    } else {
      data() %>% 
        dplyr::group_by(port, ship_type) %>% 
        dplyr::summarise(!!rlang::sym(type) := sum(!!rlang::sym(type), 
                                                   na.rm = TRUE),
                         .groups = 'drop')
    }})
    
    var_y <- ifelse(grepl("Count",type), "n", type)
    
    n_colors <- reactive(length(unique(table()$ship_type)))
    
    output$graph <- plotly::renderPlotly({
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
      
      p %>% 
        plotly::layout(title = paste("Total", title, "of vessels in the port"), 
                       yaxis = list(title = y_axis_title), 
                       barmode = 'stack')})
  })
}