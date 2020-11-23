### Vessel Dashboard

This the main repository for the Vessel Dashboard available at 
[https://dmagno.shinyapps.io/VesselDashboard/](https://dmagno.shinyapps.io/VesselDashboard/).

You can clone the repository and run the dashboard without any additional code change.

I will detail here some implementation choices I have taken.

**1. Loading of external data**

The dataset is significantly big (500 MB). It can't hence be uploaded on Github
since the maximum size for uploads is 100Mb. 

From a design perspective, I have preferred to write a code that could be run 
without additional modifications after the repository is cloned. 

I therefore download the data from the Google Drive location and load it in the 
dashboard every time the app is run.

In a "real world" exercise I would have taken a different coding direction, 
preferring a quick solution (like pre-loading) to a clean one. 

**2. Calculation of the distance**

This is the logic I have implemented for the calculation of the distance between
tow observations:

  * The snapping time between two successive observations is typically 2' but this
  is not always the case. I have decided not to infer any data in between two
  different observations if greater than 30'' and stick to the data available.
  
  * I calculate the distance between two successive observations only when the 
  vessel was not reported to be parked
  
  * The distance is calculated using the ["Haversine method"](https://en.wikipedia.org/wiki/Great-circle_distance)
  as provided in the ["geosphere"](https://cran.r-project.org/web/packages/geosphere/)
  package. This function takes the geographical coordinates (longitude and latitude)
  of two points and returns the distance between the two in meters. 
  
  * Sometimes observations are reported by two different ports at the same time.
  Also, observations span over a number of different days. I hence calculate the
  distance between two successive observations by grouping them by port and by date.
  I than take the maximum over the maximum distances in each sub-group.
  
**3. Best Practices**

  * **[Shiny Modules]**: the main shiny application is pretty compact as I made 
  extensive use of the shiny modules. In particular 5 modules have been designed, each one with a very specific purpose:
    
    1. Selection of the vessel type input
    
    2. Selection of the vessel name given the type as input and subsequent filtering of the dataset
    
    3. Management of the filtered dataset and rendering of the map. There are two cases:
      
        a. A specific vessel is selected => We show the longest route over two successive observations
        or the latest available position if it has always been parked
      
        b. All the vessels of a specific type are selected => we get the latest available position
        for each vessel
      
    4. Rendering of the message in the "info" section for the different cases treated in the module 4
    
    5. Rendering of the graphs. These are show and calculated only when more than one vessel is selected. 
    I leveraged the reusability of modules to use the same code twice in the main shiny app.
    
  * **[Testing]**: since there are 1,187 different vessels with dishomogenous observations, I run a test to
  make sure that the calculation algorithm always return a result in form of a tibble for each and every vessel.
  This test has helped me spot specific cases in which the algorithm was crashing.
  
  * **[File organisation]**: naming conventions and file locations have been defined in line with what recommended 
  in the ["Mastering Shine"](https://mastering-shiny.org/index.html) book.
  
**4. Optimization for mobile **

The multi-faceted leaflet map is not optimised for mobile. The dashboard is best visualized on computer/tablet in horizontal.

