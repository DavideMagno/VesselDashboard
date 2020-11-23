library(shiny)
library(shiny.semantic)
library(magrittr)
library(leaflet)


VesselDashboard <- function() {
  
  temp <- tempfile(fileext = ".zip")
  download.file(glue::glue("https://drive.google.com/uc?authuser=0&id=160JnqoQ\\
                           ysqzvR1GBBnKJFKAew_v6TYli&export=download"),
                temp)
  ships <- unzip(temp, exdir = tempdir()) %>% 
    vroom::vroom(.)

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
    vessel_type <- selectTypeServer("vassel_type")
    filtered_ships <- selectVesselServer("vassel_name", vessel_type, ships)
    output_analysis <- plotMapServer("map", filtered_ships)
    
    output$multi_vessel_plot <- reactive({
      return(ifelse(nrow(output_analysis()) > 2, TRUE, FALSE))
    })
    outputOptions(output, 'multi_vessel_plot', suspendWhenHidden = FALSE)
    
    CommentaryServer("comment", output_analysis, vessel_type)
    GraphsServer("graph_count", output_analysis, "Count", "number", 
                 "Number of vessels in the port")
    GraphsServer("graph_DWT", output_analysis, "DWT", "capacity",
                 "Sum capacity [DWT]")
  }
  shinyApp(ui, server)
}

VesselDashboard()
