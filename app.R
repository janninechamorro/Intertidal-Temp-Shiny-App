#############################################
# Attach packages
library(shiny)
library(tidyverse)
library(shinythemes) 
library(dplyr)
library(ggplot2)
library(slickR)
library(here)

#############################################

ui<-navbarPage("Intertidal temperature data along the Pacific Coast",
               theme = shinytheme("flatly"),
               
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
                            temperature data collected along the Pacific Coast.
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
               

               
               # Methods page
               tabPanel("Methods", 
                      tags$div(
                        h2("How is this data collected?"),
                        p("Temperature data is collected using biomimetic temperature
                          loggers called Robomussels. Robomussels are different from other 
                          temperature loggers because they thermally match living mussels, 
                          meaning they are similar in size, color, shape and thermal inertia. 
                          Using Robomussels allows us to more accurately measure 
                          temperature experienced by mussels.")),
                      tags$img(src='robomussel.jpg', 
                                 height=350, width="70%",
                                 style="display: block; 
                                    margin-left: auto; 
                                    margin-right: auto;"), #this part of this centers the image
                      tags$div(   
                         p("Multiple Robomussels are installed at each site, 
                           they are placed at different heights in the mussel bed 
                           to measure the wide range of temperatures 
                           experienced by mussels in the intertidal zone."))),
               
               # Single site page
               tabPanel("Explore a Site"),
               
               # Site comparisons page
               tabPanel("Compare Sites"))

server<-function(input, output) {
  
  # Create welcome page photo slideshow
  output$welcome_slideshow <- renderSlickR({
    images <- c("IMG_2186.jpg", "IMG_2275.jpg", "IMG_5476.jpg", "Panorama12.jpg", 
                "IMG_6126.jpg")
    slickR(
      images,
      slideId = "slideshow_images",
      height = 350, width = "70%"
    )
  })
  
}
  
shinyApp(ui = ui, server = server)