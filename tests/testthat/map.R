source(here::here("R/map.R"))


testthat::test_that("Vessel Selection Returns Data", {
  temp <- tempfile(fileext = ".zip")
  download.file(glue::glue("https://drive.google.com/uc?authuser=0&id=160JnqoQ\\
                           ysqzvR1GBBnKJFKAew_v6TYli&export=download"),
                temp)
  ships <- unzip(temp, exdir = tempdir()) %>% 
    vroom::vroom(.)
  
  test <- ships %>% 
    dplyr::mutate(name = SHIPNAME) %>% 
    dplyr::nest_by(name) %>% 
    {purrr::map(.$data, MapOneVessel)} %>% 
    purrr::map(purrr::pluck("data")) %>% 
    purrr::map_lgl(tibble::is_tibble)
  
  testthat::expect_true(all(test))
})