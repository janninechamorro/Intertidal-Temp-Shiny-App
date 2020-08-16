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
               tabPanel("Home",
                        h1("Welcome!", align = "left"), 
                        tags$div(
                          slickROutput("slideshow", 
                                     height = "350", 
                                     width = "400"),
                          style = "float:left; 
                            margin: 0px 40px;
                            border: 1px solid gray;"),
                        h4("This app allows you to visualize intertidal
                           temperature data collected along the Pacific Coast.
                           Explore the app using the navigation bar above."),
                        h1(),
                        h4("Check out our collaborators:"),
                        a("Helmuth lab", 
                          href="http://www.northeastern.edu/helmuthlab/about.html"),
                        p(),
                        a("PISCO (Partnership for Interdisciplinary Studies of 
                          Coastal Oceans)", 
                          href="http://www.piscoweb.org/"),
                        p(),
                        a("MARINe (Multi-Agency Rocky Intertidal Network)", 
                          href="https://marine.ucsc.edu/index.html"),
                        h6("Photos by Jannine Chamorro.", align = "center")),
               tabPanel("Methods", 
                        h2("How is this data collected?"),
                        p("Temperature data is collected using biomimetic temperature loggers called Robomussels. Robomussels are different from other temperature loggers because they thermally match living mussel, meaning they are similar in size, color, shape and thermal inertia. Using Robomussels allows us to more accurately measure temperature experienced by mussels." ),
                        p("Multiple Robomussels are installed at each site, they are placed at different heights in the mussel bed to measure the wide range of temperatures experienced by mussels.")),
               tabPanel("Explore a Site"),
               tabPanel("Compare Sites"))

server<-function(input, output) {
  
  output$slideshow <- renderSlickR({
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