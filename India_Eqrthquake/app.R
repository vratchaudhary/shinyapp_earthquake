#importdata
#load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
#import data

h2<-read.csv("https://earthquake.usgs.gov/fdsnws/event/1/query.csv?starttime=2010-05-11%2000:00:00&endtime=2019-05-18%2023:59:59&maxlatitude=38.376&minlatitude=-14.477&maxlongitude=137.461&minlongitude=63.633&minmagnitude=2.5&maxmagnitude=8&orderby=time&limit=20000")
head(h2)
str(h2$time)
#install.packages("lubridate")
library(lubridate)
h2$time<- ymd_hms(h2$time)
h1<-h2 %>% 
    tidyr::separate(time,c("year","month"),sep="-") %>% 
    filter(mag >=6)
######
#categorize earthquake depth
h1$depth_type <- ifelse(h1$depth <= 70, "shallow", ifelse(h1$depth <= 300 | h1$depth >70, "intermediate", ifelse(h1$depth > 300, "deep", "other")))
#######3
ui <- fluidPage(
    # Application title
    titlePanel("Earthquake data of South East Asia"),
    sidebarLayout(
        sidebarPanel( sliderInput("year","Dates:",
                                             min = (2010),
                                             max = (2019),
                                             value = 1)
                  
    ),
    
    mainPanel( 
        #this will create a space for us to display our map
        leafletOutput(outputId = "mymap"),
        checkboxInput("heat", "Heatmap", FALSE),
        checkboxInput("markers", "Depth", FALSE)
        #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
        #absolutePanel(top = 60, left = 20, 
                    #  checkboxInput("markers", "Depth", FALSE),
                      
        )
    )
)
#######server

server <- function(input, output, session) {
    #define the color pallate for the magnitidue of the earthquake
    pal <- leaflet::colorNumeric(
        palette = c('pink', 'orchid', 'dark orange', 'orange red', 'red', 'dark red',
                    'magenta', 'blue', 'light green', 'dark blue', 'light blue', 'dark green',
                    'orange', 'yellow', 'grey', 'purple'),
        domain = h1$mag)


    #define the color of for the depth of the earquakes
    pal2 <- leaflet::colorFactor(
        palette = c('blue', 'orange', 'pink'),
        domain = h1$depth_type
    )
head(h1)
    
    #create the map
    output$mymap <- renderLeaflet({
        leaflet(h1) %>% 
            setView(lng = 100, lat = 20, zoom = 4)  %>% #setting the view over ~ center of east asia
            addTiles() %>% 
            addCircles(data = h1, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~sqrt(mag)*25000, popup = ~as.character(mag), label = ~as.character(paste0("Magnitude: ", sep = " ", mag)), color = ~pal(mag), fillOpacity = 0.5)
    })
    
    #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
    observe({
        proxy <- leafletProxy("mymap", data = h1)
        proxy %>% clearMarkers()
        if (input$markers) {
            proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(depth_type), fillOpacity = 0.2,label = ~as.character(paste0("Magnitude: ", sep = " ", mag))) %>%
                addLegend("bottomright", pal = pal2, values = h1$depth_type,
                          title = "Depth Type",
                          opacity = 1)}
        else {
            proxy %>% clearMarkers() 
        }
    })
    
    observe({
        proxy <- leafletProxy("mymap", data = h1)
        proxy %>% clearMarkers()
        if (input$heat) {
            proxy %>%  addHeatmap(lng=~longitude, lat=~latitude, intensity = ~mag, blur =  10, max = 0.05, radius = 15) 
        }
        else{
            proxy %>% clearHeatmap()
        }
        
        
    })
    
}

shinyApp(ui, server)