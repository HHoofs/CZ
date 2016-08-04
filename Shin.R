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
library(RColorBrewer)

load("Data/Data.Comp2.Rdata")
Data.Comp2$Code <- str_trim(as.character(Data.Comp2$Code))
Data.Comp2$Product <- str_trim(as.character(Data.Comp2$Product))
Data.Comp2$Beschrijving <- str_trim(as.character(Data.Comp2$Beschrijving))


palette_rev <- rev(brewer.pal(10, "RdBu")) 

rev(brewer.pal(4, "RdBu"))


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 20, right = 20, left = "auto",  bottom = "auto",
                fixed =FALSE,class = "panel panel-default",width = "15%",style = "background-color: #ffffff; color: #000000;",draggable = TRUE,
                wellPanel(style = "background-color: #ffffff;border:0px;",
                selectInput("Code", "Code",unique(Data.Comp2$Code)),
                htmlOutput("desc"),
                plotOutput("hist", height = 100)
                )
                )
)

server <- function(input, output, session) {
  
  rData <- reactive({
    Data_sel <- Data.Comp2[Data.Comp2$Code == input$Code,]
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
      fitBounds(3.373385, 50.750862, 7.201162, 53.552211) %>% 
      addCircleMarkers(fillColor = ~pal(Prijs),stroke = TRUE,fillOpacity = .5,color="black",opacity=1,weight = 2,
                       popup = ~paste(Instelling," (",as.character(Prijs),")",sep="")) %>% 
      addLegend("bottomright", pal = pal, values = ~Prijs,labels=c("Bag"),
                title = "Prijs (Euro)"  ) 
  })
  
  output$desc <- renderText({
    unique(rData()[,"Beschrijving"])
  })
  
  output$hist <- renderPlot({
    pal <- colorpal()
    dataPlot <- rData()
    ggplot(dataPlot,aes(Prijs)) + 
      geom_density(fill="gray") +
      # geom_point(position=position_jitter(width=0.1),size=1) +
      geom_vline(xintercept=mean(dataPlot$Prijs)) +
      scale_y_continuous(breaks=NULL) +
      scale_x_continuous() +
      labs(x=NULL, y=NULL) +
      # coord_flip() 
      theme_gray(15) +
      theme(panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA)
            )
    
  }, bg="transparent")
  

}

shinyApp(ui, server)
