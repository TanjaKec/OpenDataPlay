# upload the packages
#library(ggplot2)
#library(shinyBS)
#library(ggmap)
#library(RColorBrewer)
#library(ggmap)
#library(readODS)
#library(magrittr)

# install the packages
#install.packages("shiny", repos = "http://cran.us.r-project.org")
#install.packages("leaflet", repos = "http://cran.us.r-project.org")
#install.packages("tidyr", repos = "http://cran.us.r-project.org")
#install.packages("dplyr", repos = "http://cran.us.r-project.org")

library(shiny)
library(leaflet)
library(tidyr)
library(dplyr)


# read data 
#md <- read_ods("NEZ_OPENDATA_20182_20180225.ODS")
md <- read.csv("data/nez-opendata_02_19.csv")
dim(md)
head(md)
# name variables
names(md) <- letters[1:7]
head(md)
str(md)
glimpse(md)
# separate date and time and save as two variables ('date' and 'time')
md <- separate(md, b, c("date", "time"), sep = ",")
# format and organise data
cols <- c("e", "f", "g")
md[cols] <- lapply(md[cols], as.factor)
glimpse(md)
md$date <- format(as.POSIXct(md$date,format="%d.%m.%Y"),"%d/%m/%Y")
md$time <- format(as.POSIXct(md$time, format="%H:%M"),"%H:%M")
md$date <- as.Date(md$date, "%d/%m/%Y")
str(md)
levels(md$e) <- c("mat.demage", "deaths", "injured")
#levels(md$f) <- c("one_vehicle", "two_vehicle_no_turn", "two_vehicle", "parked_vehicle", "pedestrian")
md <- rename(md, id = a, long = c, lat = d, accident = e, type_acc = f, descrip = g)
names(md)
glimpse(md)

md1 <- md %>% 
  separate(time, c("hour", "minutes"), sep = ":") %>% 
  separate(date, c("year", "month", "day"), sep = "-")

cols <- c("hour", "minutes", "year", "month", "day")
md1[cols] <- lapply(md1[cols], as.integer)  

# check data
glimpse(md1)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(code(a("Traffic Accidents in Belgrade (Feb 2019)", href="https://data.gov.rs/sr/datasets/podatsi-o-saobratshajnim-nezgodama-za-teritoriju-grada-beograda/" ), style = "color:purple")),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(

         h6("'Otvoreni podaci – otvorene mogućnosti'"),
         p(h6(code(a("R-Ladies Belgrade", href= "https://rladies.org/")))),
         h6(code(a("instructor: Dr Tatjana Kecojevic", href="https://tanjakec.github.io"))),

         br(),
         h6("hover & click above the points to see description!"),
         selectInput("acc", "Type of Accident",
                     c(
                       "Material Demage"= "mat.demage",
                       "Deaths" ="deaths",
                       "Injured" = "injured",
                       "All Accidents" = "all"
                       ))
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("mymap", width = "100%", height = 600)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  minlat <- min(md1$lat)
  maxlat <- max(md1$lat)
  minlong <- min(md1$long)
  maxlong <- max(md1$long)

  output$mymap <- renderLeaflet({
    
    if (input$acc %in% levels(md1$accident)){
      md2 <- md1 %>% 
        filter(accident == input$acc)
    } else {
      md2 <- md1}
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data = md2) %>% 
      addTiles() %>%
      fitBounds(~minlong, ~minlat, ~maxlong, ~maxlat) %>% 
      addCircles(lng = ~long, lat = ~lat,
                 radius = 150, weight = 5, color = "black",
                 fillColor = "red", fillOpacity = 0.7, popup = ~paste(
                   "<b>", type_acc, "<br>", descrip, "<br>"
                 ))
  })

   
}

# Run the application 
shinyApp(ui = ui, server = server)

