library(shiny)
library(shiny.semantic)
library(magrittr)
library(leaflet)


VesselDashboard <- function() {
  
  # Download the data from Google Doc
  temp <- tempfile(fileext = ".zip")
  download.file(glue::glue("https://drive.google.com/uc?authuser=0&id=160JnqoQ\\
                           ysqzvR1GBBnKJFKAew_v6TYli&export=download"),
                temp)
  ships <- unzip(temp, exdir = tempdir()) %>% 
    vroom::vroom(.)

  # Specifics of the plot grid
  grid_graphs <- grid_template(
    default = list(areas = rbind(c("graph1", "graph2")),
                   rows_height = c("auto"),
                   cols_width = c("50%", "50%"))
  )
  
  ui <- semanticPage(
    title = "Vessel Dashboard",
    h1("Vessel Dashboard"),
    sidebar_layout(
      sidebar_panel(
        selectTypeInput("vassel_type", ships),
        selectVesselInput("vassel_name", vassel_type)
      ),
      main_panel(
        segment(class = "placeholder segment",
                CommentaryUI("comment"),
                plotMapUI("map")),
        conditionalPanel(
          condition = "output.multi_vessel_plot",
          segment(class = "placeholder segment",
                  grid(grid_graphs,
                       graph1 = GraphsUI("graph_count"),
                       graph2 = GraphsUI("graph_DWT"))
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    # Module 1: manage type input
    vessel_type <- selectTypeServer("vassel_type")
    
    #Module 2: manage name input and filter the dataset
    filtered_ships <- selectVesselServer("vassel_name", vessel_type, ships)
    
    #Module 3: render the map in different cases and output the distance analysis
    output_analysis <- plotMapServer("map", filtered_ships)
    
    # Module 4: render the comments depending on the result of the distance analysis
    CommentaryServer("comment", output_analysis, vessel_type)
    
    # Manages the conditional panel of the graphs
    output$multi_vessel_plot <- reactive({
      return(ifelse(length(unique(filtered_ships()$SHIPNAME)) > 1, TRUE, FALSE))
    })
    outputOptions(output, 'multi_vessel_plot', suspendWhenHidden = FALSE)
    
    # Module 5: render the plots only when more than 1 vessel is selected
    GraphsServer("graph_count", filtered_ships, "Count", "number", 
                 "Number of vessels in the port")
    GraphsServer("graph_DWT", filtered_ships, "DWT", "capacity",
                 "Sum capacity [DWT]")
  }
  shinyApp(ui, server)
}

VesselDashboard()
