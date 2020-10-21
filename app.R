#############################################
# Attach packages
library(shiny)
library(tidyverse)
library(shinythemes) 
library(dplyr)
library(ggplot2)
library(slickR)
library(here)
library(leaflet)

#############################################
# Import location data

setwd("~/Documents/GitHub/Intertidal-Temp-Shiny-App/")

all_sites <- read.csv("~/Documents/GitHub/Intertidal-Temp-Shiny-App/InfoMicrosite_2016_mussel.csv")

# Subset West Coast sites
site_data = filter(all_sites, 
                   state_province == "California" 
                   | state_province == "Oregon" 
                   | state_province == "Washington")

# Subset unique West Coast sites
unique_site_data <- distinct(site_data, location, .keep_all = TRUE)

# All temp data from "data_wrangling.R"
temp_data<-read_csv(file="temp_data.csv")

#############################################
ui<-navbarPage("Intertidal temperature data along the Pacific Coast",
               theme = shinytheme("flatly"),
               
               #############################################
               # Welcome page
               tabPanel("Home",
                        
                        h1("Welcome!", 
                           align = "left",
                           style = "margin: 15px 40px;"),
                        
                        tags$div(
                          slickROutput("welcome_slideshow", 
                                     height = "350px", 
                                     width = "500px"),
                          style = "float:left; 
                            margin: 0px 40px 15px 40px;
                            border: 1px solid gray;"),
                        
                        tags$div(
                          h4("This app allows you to visualize intertidal
                            temperature data collected along the northeastern Pacific Coast.
                            Explore the app using the navigation bar above."),
                          h1(),
                          h3("Check out our collaborators:"),
                          a("Helmuth Lab at Northeastern University",
                            href="http://www.northeastern.edu/helmuthlab/about.html"),
                          p(),
                          a("PISCO (Partnership for Interdisciplinary Studies of 
                          Coastal Oceans)",
                            href="http://www.piscoweb.org/"),
                          p(),
                          a("MARINe (Multi-Agency Rocky Intertidal Network)",
                            href="https://marine.ucsc.edu/index.html"),
                          style = "margin: 0px 40px;"
                          ),
                       
                        tags$footer(
                          h6("Photos by Jannine Chamorro"),
                            style = "position:absolute;
                              float:center;
                              bottom:40px;
                              width:100%;
                              height:10px;
                              color:gray;
                              font-style:italic;
                              padding:40px 40px;
                              z-index:1;")
                        ),
               

               #############################################
               # Methods page
               tabPanel("Methods", 
                      tags$div(
                        h2("How is this data collected?"),
                        p("Temperature data is collected using biomimetic temperature
                          loggers called Robomussels. Robomussels are different from other 
                          temperature loggers because they thermally match living mussels, 
                          meaning they are similar in size, color, shape and thermal inertia. 
                          Using Robomussels allows us to more accurately measure 
                          temperature experienced by mussels. Robomussels specifically used in 
                          this dataset were designed to mimic once of the most abundant mussel
                          species found along the northeastern Pacific coast, California mussels",
                        em("(Mytilus californianus)."),
                          "Multiple Robomussels are installed at each site, 
                           they are placed at different heights in the mussel bed 
                          to measure the wide range of temperatures 
                          experienced by mussels in the intertidal zone.")),
                      tags$img(src='robo_mussel_comp.jpg', 
                                 height=350, width="70%",
                                 style="display: block; 
                                    margin-left: auto; 
                                    margin-right: auto;")), #this part of this centers the image

               #############################################
               # Single site page
               tabPanel("Explore a Site",
                        sidebarPanel(
                                      #Select which site to show on map
                                      selectInput("SiteSelect",
                                          label = "Choose a site!",
                                          choices = unique(unique_site_data$location))
                                      ),
                                      
                        mainPanel(
                                      "",
                                      leafletOutput(outputId = "map1",
                                                    width = "60%",
                                                    height = "400")
                        )),
               
               
               #############################################
               # I made replicate single site page to try an figure out how to plot the temp data. 
               # Once I figure it out we can combine it with the original page.
               tabPanel("Explore a Site",
                        sidebarPanel(
                                      # Select which Site to plot
                                      selectInput(
                                            inputId = "SiteFinder",
                                            label="Choose a Site", 
                                            choices=c("Lompoc Landing, CA"="LL", "Alegria, CA"="AG", "Coal Oil Point, CA"="CP")),
                                     
                                      # Select which Zone(s) to plot
                                      checkboxGroupInput(
                                            inputId = "ZoneFinder",
                                            label= "Zone",
                                            choices= c("High", "Upper-Mid", "Mid", "Lower-Mid", "Low")),
                                     
                                      # Select which Date Range to Plot
                                      sliderInput(
                                            inputId = "dateFinder",
                                            label = "Date",
                                            value= c(as.Date(min(temp_data$local_datetime), format="%Y-%m-%d %h:%m:%s"), as.Date(max(temp_data$local_datetime), format="%Y-%m-%d %h:%m:%s")),
                                            min= as.Date(min(temp_data$local_datetime), format="%Y-%m-%d %h:%m:%s"),
                                            max= as.Date(max(temp_data$local_datetime), format="%Y-%m-%d %h:%m:%s"),
                                            timeFormat="%b %Y")), 
                       #Here is some text to determine if my stuff is getting pushed
                        
                              mainPanel("", 
                                    plotOutput((outputId="scatterplotFinder")))))
                       
                         
        


#############################################
server<-function(input, output, session) {
  
  # Create welcome page photo slideshow
  output$welcome_slideshow <- renderSlickR({
    images <- c( "IMG_2275.jpg", "IMG_5476.jpg", "Panorama12.jpg", 
                "IMG_6126.jpg")
    slickR(
      images,
      slideId = "slideshow_images",
      height = 350, width = "70%"
    )
  })
  
  # Create reactive site dataframe
  #locations <- reactive({
  #  unique_site_data %>% 
  #    filter(location %in% input$locations) 
  #})
  
  #location_reactive <- reactive({
  #  if(is.null(input$SiteSelect)){
  #    unique_site_data
  #  }else{
  #    unique_site_data[unique_site_data$location %in% input$SiteSelect, ]
  #  }
  #})

  # Create map for sites
  #leaflet(options = leafletOptions(minZoom = 4))
  
  output$map1 <- renderLeaflet({
    leaflet( data=unique_site_data ) %>% 
    addProviderTiles(providers$Esri.WorldPhysical,
                     providerTileOptions(detectRetina = T)) %>%
#) %>% 
    addMarkers(
      lng = unique_site_data$field_lon,
      lat = unique_site_data$field_lat,
      label = unique_site_data$location,
      group = "myMarkers",
      labelOptions = labelOptions(direction = "bottom",
                                  offset = c(2,2), sticky = T,
                                  style = list(
                                    "color" = "black",
                                    "font-family" = "helvetica",
                                    "font-style" = "italic",
                                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                    "font-size" = "12px",
                                    "border-color" = "rgba(0,0,0,0.5)"
                                  )),
      options = markerOptions(interactive = F, clickable = T, riseOnHover = F),
    #  #clusterOptions = markerClusterOptions(),
      popup = paste(unique_site_data$location, ", ", 
                    unique_site_data$state_province, "<br>",
                    "(", unique_site_data$field_lat, "ºN, ", 
                    unique_site_data$field_lon, " ºW)", sep = ""),
      popupOptions = popupOptions(maxWidth = 300, 
                                  minWidth = 50, maxHeight = NULL,
                                  autoPan = T, keepInView = F, closeButton = T)
    ) %>% 
    setView(lat = 42,	lng = -130, zoom = 4) %>% 
    #addMiniMap(
    #  tiles = providers$Esri.OceanBasemap,
    #  toggleDisplay = T
    #) %>% 
    addEasyButton(easyButton(
      icon="fa-globe", title="Zoom out to level 1",
      onClick=JS("function(btn, map){ map.setZoom(1); }")))
  })
  
  observeEvent(input$SiteSelect, {
    leafletProxy("map1") %>% 
      clearGroup("myMarkers") %>% 
      addMarkers(
        data = unique_site_data[unique_site_data$location == input$SiteSelect, ],
        lng = ~field_lon,
        lat = ~field_lat,
        label = ~location,
        group = "myMarkers",
        labelOptions = labelOptions(direction = "left",
                                    permanent = T,
                                    style = list(
                                      "color" = "black",
                                      "font-family" = "helvetica",
                                      "font-style" = "italic",
                                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                      "font-size" = "12px",
                                      "border-color" = "rgba(0,0,0,0.5)"
                                    ))) #%>% 
      #flyTo(#unique_site_data[unique_site_data$location == input$SiteSelect, ],
      #      lng = ~field_lon, lat = ~field_lat, 
        #lng = ~unique_site_data$field_lon[unique_site_data$location == input$SiteSelect, ],
        #lat = ~unique_site_data$field_lat[unique_site_data$location == input$SiteSelect, ],
      #  zoom = 10)
    
      
        #popup = paste(
        #  unique_site_data$location[unique_site_data$location == input$SiteSelect, ]
        #),
        #popupOptions = popupOptions(maxWidth = 300, 
        #                            minWidth = 50, maxHeight = NULL,
        #                            autoPan = T, keepInView = F, closeButton = T))
      
      
    
    #if (is.null(input$SiteSelect)){
    #  unique_site_data = unique_site_data
    #}else{
    #  unique_site_data = unique_site_data[unique_site_data$location %in% input$SiteSelect,]
    #}
    #leafletProxy("map1") %>% 
    #  clearMarkers() %>% 
    #  addMarkers(
    #    data = location_rea,
    #    lng = ~field_lon,
    #    lat = ~field_lat,
    #    label = ~location,
    #  )
  }
)
    
    
    
    
    #input$map1_marker_click, {
    #leafletProxy("map1", session) %>% 
    #  removeMarker(input$map1_marker_click$id)
#  })

### Explore a Site Page
      
    #Filtering Data to Plot
            Intertidal_Finder<-reactive({
                   temp_data %>% 
                        filter(site==input$SiteFinder) %>% #Filter out site of interest
                         filter(zone==input$ZoneFinder) %>% #Filter out zone (s) of interest
                          subset(date >= input$dateFinder[1] & date <= input$dateFinder[2])
})

  
      #Plotting Temperature Data 
              output$scatterplotFinder<- renderPlot({
              ggplot(data=Intertidal_Finder(), aes(x=local_datetime, y=Temp_C, color=zone))+
              geom_point(size=0.2, alpha=0.5)+
                  xlab("Date")+
                  ylab("Temperature (C)")+
                  theme_minimal(base_size=20)
})
  

  
}
  
shinyApp(ui = ui, server = server)