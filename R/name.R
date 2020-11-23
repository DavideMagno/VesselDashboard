selectVesselInput <- function(id, type) {
  selectInput(NS(id, "name"), label = "Pick a vassel name", 
              choices = character(0)) 
}

selectVesselServer <- function(id, type, data) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(type(), {
      if (grepl("All", type())) {
        choices = "All"
      } else {
        choices = data %>% 
          dplyr::filter(ship_type %in% type()) %>%
          dplyr::pull("SHIPNAME") %>% 
          unique %>% 
          sort %>% 
          {c("All", .)}
      }
      updateSelectInput(session, "name", 
                        label = "Pick a vassel name",
                        choices = choices, 
                        selected = head(choices, 1))
    })
    
    reactive({
      if (grepl("All", type())) {
        res <- data
      } else {
        res <- data %>%
          dplyr::filter(ship_type %in% type())
        if (!grepl("All", input$name)) {
          res <- res %>%
            dplyr::filter(SHIPNAME %in% input$name)
        }
      }
      res
    })
  })
}