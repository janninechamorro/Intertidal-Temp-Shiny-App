

ui<-navbarPage("Our Navigation Bar!", 
               tabPanel("First tab!"),
               tabPanel("Second tab!"))

server<-function(input, output) {}
  
shinyApp(ui = ui, server = server)