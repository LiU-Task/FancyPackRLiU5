require(shiny)
library(shiny)

ui <- fluidPage(
  
  titlePanel("Show Map!"),
  sidebarLayout(
    sidebarPanel(
      # Choose dataset
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("world-2","se-7","se-4","fi-8", "ch-8", "no-7","no-4","dk-7","us-4","gl-7")),
      
      # Type in objective 
      textInput(inputId="aim", label = h4("Entity to show"), value = "Enter entity name..."),
      # Type in date
      textInput(inputId = "date", label = h4("Searching date"), value = "Enter in form of YYYY-MM...")
      
    ),
    
    mainPanel(
      plotOutput(outputId = "visa"),
      verbatimTextOutput(outputId = "id"),
      verbatimTextOutput(outputId = "name")
      
    )))

server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "world-2"=world-2,"se-7"=se-7,"se-4"=se-4,"fi-8"=fi-8, "ch-8"=ch-8, "no-7"=no-7,"no-4"=no-4,"dk-7"=dk-7,"us-4"=us-4,"gl-7"=gl-7)
  })
  
  
  # Show map 
  output$visa <- renderPlot({
    x<-ICU$new(land=input$dataset,date=input$date)   
    x$visa(input$aim)
  })
  # Show id + Formal name
  output$id <- renderPrint({
    #x<-ICU$new(land=input$dataset,date=input$date) 
    z<-x$guo$Countries$coordinates
    enti<-x$guo$Entities
    gj<-x$guo$Countries
    id<-enti[grep(input$aim,enti$name,ignore.case=TRUE),]$id
    cat(paste("This entity has id number of ",id,sep=""))
  }) 
  output$name <- renderPrint({
    #x<-ICU$new(land=input$dataset,date=input$date) 
    z<-x$guo$Countries$coordinates
    enti<-x$guo$Entities
    gj<-x$guo$Countries
    nam<-enti[grep(input$aim,enti$name,ignore.case=TRUE),]$name
    cat(paste("This entity has formal name of ",nam,sep=""))
  }) 
  
}

shinyApp(ui = ui, server = server)