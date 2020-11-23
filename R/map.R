
# Distance Algorithm + Functions For One Vessel --------------------------------

# We use the geosphere::distHaversine function to calculate the distance between
# two observations. We then extract start and ending point.
CalculateDistance <- function(input.data) {
  n <- nrow(input.data)
  
  data <- input.data %>% 
    dplyr::arrange(DATETIME) %>% 
    dplyr::select(LON, LAT, DATETIME, SHIPNAME, ship_type) %>% 
    dplyr::mutate(DIST = c(0,
                           geosphere::distHaversine(.[1:n-1,1:2], .[2:n,1:2])))
  
  index_max_distance <- which(data$DIST == max(data$DIST))
  
  if (length(index_max_distance) > 0) 
    index_max_distance <- index_max_distance[length(index_max_distance)]
  
  data <- data %>% 
    dplyr::slice((index_max_distance - 1):index_max_distance) %>% 
    dplyr::mutate(DIST = max(DIST))
  
  return(data)
}

# If one specific vessel is selected, we run the distance algorithm, which consists
# of the following steps:
# 1. If the vessel has always been parked, we just record its latest available position
# 2. Otherwise, we group the observations by port and date. It is possible in fact
# that a vessel has been reported by two different ports at the same time
# 3. If there are at least 2 consecutive observations per group, we run the 
# distance calculation algorithm
# 4. We then plot the results in the map depending on steps 1. and 3. 
MapOneVessel <- function(data) {
  test_parked <- nrow(dplyr::filter(data, !is_parked))
  
  if (test_parked > 1) {
    data_for_map <- data %>% 
      dplyr::filter(!is_parked) %>% 
      dplyr::group_nest(port, date)
    
    test_one_observation <- purrr::map_dbl(data_for_map$data, nrow) %>% 
      {which(. == 1)}
    
    if (length(test_one_observation) > 0) {
      data_for_map <- data_for_map[-test_one_observation,]
    }
    
    if (nrow(data_for_map) > 0) {
      data_for_map <- data_for_map %>% 
        dplyr::mutate(longest_distance = purrr::map(data, CalculateDistance)) %>% 
        dplyr::select(-data) %>% 
        tidyr::unnest(longest_distance) %>% 
        dplyr::slice_max(DIST)
    } else {
      data_for_map <- data %>% 
        dplyr::slice_max(DATETIME)
    }
  } else {
    data_for_map <- data %>% 
      dplyr::slice_max(DATETIME)
  }
  
  map_popup <- paste0("<strong> Latitude: </strong>",
                      data_for_map$LAT, "<br>",
                      "<strong> Longitude: </strong>",
                      data_for_map$LON, "<br>",
                      "<strong> Observation: </strong>",
                      data_for_map$DATETIME)
  
  map <- leaflet(data_for_map) %>%
    addTiles() %>%
    addCircleMarkers(~LON, ~LAT, stroke = FALSE, fillOpacity = 1,
                     popup = map_popup) 
  
  if (nrow(data_for_map) > 1) {
    map <- map %>%
      leaflet.minicharts::addFlows(data_for_map$LON[1], data_for_map$LAT[1],
                                   data_for_map$LON[2], data_for_map$LAT[2],
                                   maxThickness = 3) 
  }
  
  return(list(data = data_for_map,
              map = list(map),
              ncol = 1))
}

# Functions For More Vessels------------- --------------------------------

# Specifics of the leaflet map for each of the port
MapMoreVessels <- function(port, data) {
  
  ship_types <- unique(data$ship_type)
  
  map_popup <- paste0("<strong> Name: </strong>",
                      data$SHIPNAME, "<br>",
                      "<strong> Observation: </strong>",
                      data$DATETIME)
  
  pal <- colorFactor("RdYlBu", domain = unique(data$ship_type))
  
  map <- leaflet(data, height = 250) %>%
    addTiles() %>%
    addCircleMarkers(~LON, ~LAT, 
                     radius = 6, 
                     color = ~pal(ship_type), stroke = FALSE, 
                     fillOpacity = 1, popup = map_popup)  %>% 
    addLegend(title = stringr::str_to_sentence(port),
              pal = pal, values = ~ship_type, opacity = 1)
  
  return(map)
  
}


# If more vessels are selected, we extract the latest known observation per port and
# map it. As there are 6 different ports, we repeat this sequentially with the
# purrr::map2 function
MultipleMapsMoreVessels <- function(data) {
  data_for_map <- data %>% 
    dplyr::group_by(SHIPNAME) %>% 
    dplyr::slice_max(DATETIME) %>% 
    dplyr::ungroup() 
  
  multiple_maps <- data_for_map %>% 
    dplyr::nest_by(port) %>% 
    {purrr::map2(.$port, .$data, MapMoreVessels)}
  
  ncol <- ifelse(length(multiple_maps) >= 3, 3, length(multiple_maps)) 
  
  return(list(data = data_for_map,
              map = multiple_maps,
              ncol = ncol))
  
}

# Shiny Module ------------------------------------------------------------

plotMapUI <- function(id) {
  uiOutput(NS(id, "map"))
}

plotMapServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Two different analysis are run whether one specific vessel is selected or not
    map <- reactive({
      if (length(unique(data()$SHIPNAME)) == 1) {
        MapOneVessel(data())
      } else {
        MultipleMapsMoreVessels(data())
      }
    })
    
    # The leafsync package allows to create a lattice of leaflet maps in one
    # predefined frame
    output$map <- renderUI(leafsync::latticeview(map()$map, ncol = map()$ncol))
    
    return(reactive(map()$data))
    
  })
}