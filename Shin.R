# 
library(shiny)
library(leaflet)
library(foreign)
library(ggmap)
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(yaml)

load("Data.Rdata")

palette_rev <- rev(brewer.pal(10, "RdBu"))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 20, right = 20, left = "auto",  bottom = "auto",
                fixed =FALSE,class = "panel panel-default",width = "15%",style = "background-color: #ffffff; color: #000000;",draggable = TRUE,
                selectInput("Code", "Code",unique(Data$Code)),
                htmlOutput("desc"),
                plotOutput("hist", height = 200)
                )
)

server <- function(input, output, session) {
  
  rData <- reactive({
    Data_sel <- Data[Data$Code == input$Code,]
    Data_sel <- Data_sel[!is.na(Data_sel$Long),]
    Data_sel
  })
  
 # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(palette_rev, rData()$Price)
  })
  
  output$mymap <- renderLeaflet({
    pal <- colorpal()
    leaflet(rData()) %>% 
      addProviderTiles("Esri.WorldTopoMap") %>% 
      fitBounds(~min(Long), ~min(Lat), ~max(Long), ~max(Lat)) %>% 
      addCircleMarkers(fillColor = ~pal(Price),stroke = TRUE,fillOpacity = .5,color="black",opacity=1,weight = 2,
                       popup = ~paste(Inst," (",as.character(Price),")",sep="")) %>% 
      addLegend("bottomright", pal = pal, values = ~Price,labels=c("Bag"),
                title = "Prijs (Euro)"  ) 
  })
  
  output$desc <- renderText({
    unique(rData()[,"Desc"])
  })
  
  output$hist <- renderPlot({
    pal <- colorpal()
    dataPlot <- rData()
    dataPlot$Inst <- factor(dataPlot$Inst,levels = dataPlot$Inst[order(-dataPlot$Price)],ordered = TRUE)
    ggplot(dataPlot,aes(Price)) + 
      geom_density(fill="gray") +
      scale_y_continuous("",breaks=NULL) +
      scale_x_continuous("Prijs (EURO)") +
      theme(panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA)
            )
  }, bg="transparent")
  

}

shinyApp(ui, server)
