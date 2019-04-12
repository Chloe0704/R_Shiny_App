
library(shiny)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(plotly)


zillow <- read.csv("zillow_irvine.csv")

colo <- c("taxvaluedollarcnt","yearbuilt")
ui <- navbarPage("House selector",# id="nav",
      tabPanel("Interactive map",  
               leafletOutput("map"), 
               div(class="outer",
               tags$head(includeCSS("design.css")),
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                                            
                h3("Irvine Housing Selector"),
                                            
                h4("Filter"),
                selectInput("colorchoose", "Color By", colo),
                numericInput("lotsizesquarefeet1", "maximum house size(sqft)", 400000),
                numericInput("lotsizesquarefeet2", "minimum house size(sqft)", 0),
                sliderInput("valuepersquarefoot", "Assesed value per squarefoot",0, 600, c(0,600), step = 1),
                sliderInput("bedroomcnt", "number of bedrooms",0, 10, c(0,10), step = 1),
                sliderInput("bathroomcnt", "number of bathrooms", 0, 10, c(0,10), step = 0.5),
                sliderInput("garagecarcnt", "number of garage",0, 15, c(0,15), step = 1)
                              )
                              
                          ),
                          
                 plotlyOutput("valuepersquare",height = 200, width = 800),
                 plotlyOutput("taxvaluedollarcnt",height = 200,width = 800),
            wellPanel(
                span("Number of houses selected(by filter):",textOutput("zillow_f")),width = 200),
            wellPanel(
                span("Number of houses in bound:",textOutput("inbound")),width = 200),
                tags$div(id="DATA",
                         'Data source ', tags$em('Zillow: https://www.kaggle.com/c/zillow-prize-1/data'), ' 2017.')
                 )
)


server <- function(input, output) {
  
  #make map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -117.800, lat = 33.6846, zoom = 12) 
    
  })
  
  
  #react to the filter
  house <- reactive({
    
    minlotsizesquarefeet <- input$lotsizesquarefeet2
    minvaluepersquarefoot <- input$valuepersquarefoot[1]
    minbedroomcnt <- input$bedroomcnt[1]
    minbathroomcnt <- input$bathroomcnt[1]
    mingaragecarcnt <- input$garagecarcnt[1]
    
    
    maxlotsizesquarefeet <- input$lotsizesquarefeet1
    maxvaluepersquarefoot <- input$valuepersquarefoot[2]
    maxbedroomcnt <- input$bedroomcnt[2]
    maxbathroomcnt <- input$bathroomcnt[2]
    maxgaragecarcnt <- input$garagecarcnt[2]
    
    
    
    # Apply filters
    zillow_f <- zillow %>%
      filter(
        # regionidcounty == regionidcounty,
        
        lotsizesquarefeet >= minlotsizesquarefeet,
        lotsizesquarefeet <=maxlotsizesquarefeet,
        
        valuepersquarefoot >= minvaluepersquarefoot,
        valuepersquarefoot <=maxvaluepersquarefoot,
        
        bedroomcnt >= minbedroomcnt,
        bedroomcnt <=maxbedroomcnt,
        
        bathroomcnt >= minbathroomcnt,
        bathroomcnt <=maxbathroomcnt,
        
        garagecarcnt >= mingaragecarcnt,
        garagecarcnt <=maxgaragecarcnt
        
        
      ) %>%
      arrange(bedroomcnt)
    
    zillow_f <- as.data.frame(zillow_f)
    zillow_f
    
  })
  
  
  #react to selected area on map
  hsInBounds <- reactive({
    if (is.null(req(input$map_bounds)))
      return(zillow[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(house(),
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  
  
  #hist for total house value in bound
  output$taxvaluedollarcnt <- renderPlotly({
    if (nrow(hsInBounds()) == 0)
      return(NULL)
    
    #hist(zipsInBounds()$taxvaluedollarcnt , breaks=100 ,border = FALSE, col=rgb(0.1,0.8,0.3,0.5) ,xlab="House accessed value (In bound house)" ,main="")
    p <- plot_ly(x = hsInBounds()$taxvaluedollarcnt, type = "histogram",color =I("skyblue"))
    layout(p, xaxis = list(title = "House accessed value (In bound house)"), yaxis = list(title = 'Frequency'))
  })
  
  
  #hist for house size in bound
  output$valuepersquare <- renderPlotly({
    if (nrow(hsInBounds()) == 0)
      return(NULL)
    
    #hist(zipsInBounds()$valuepersquarefoot , breaks=100 ,border = FALSE, col=rgb(0.1,0.8,0.3,0.5) ,xlab="Accessed value/sqarefoot (In bound house)" ,main="")
    t<-plot_ly(x = hsInBounds()$valuepersquarefoot, type = "histogram",color =I("skyblue"))
    layout(t, xaxis = list(title = "Accessed value/sqarefoot (In bound house)"),yaxis = list(title = 'Frequency'))
  })
  
  
  
  #react to fiter
  observe({
    colorBy <- input$colorchoose
    colorData <-  hsInBounds()[[colorBy]]
    pal <- colorBin("viridis", colorData, reverse = TRUE,7, pretty = TRUE)
    
    leafletProxy("map", data =  hsInBounds() ) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude,
                 stroke=FALSE, fillOpacity=1, radius=10, fillColor=pal(colorData) )%>%
      addLegend("bottomleft", pal=pal, values=colorData,title=colorBy,layerId="colorLegend"
      )
  })
  
  # Show a popup at the given location
  showhsPopup <- function(lat, lng) {
    selectedh <- zillow[zillow$latitude == lat & zillow$longitude == lng,]
    content <- as.character(tagList(
      
      tags$strong(HTML(sprintf("%s, County %s, %s",
                               selectedh$regionidcity, selectedh$regionidcounty, selectedh$regionidzip
      ))), tags$br(),
      sprintf("Total area(squarefeet): %s", as.integer(selectedh$lotsizesquarefeet)), tags$br(),
      sprintf("Total accessed value: %s", dollar(selectedh$taxvaluedollarcnt)), tags$br(),
      sprintf("Value per squarefoot: %s", dollar(as.integer(selectedh$valuepersquarefoot))), tags$br(),
      sprintf("Number of bedroom: %s", selectedh$bedroomcnt), tags$br(),
      sprintf("Number of bathroom: %s", selectedh$bathroomcnt), tags$br(),
      sprintf("Number of garage: %s", selectedh$garagecarcnt)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showhsPopup(event$lat, event$lng)    
    })
  })
  output$zillow_f <- renderText({nrow(house())})
  output$inbound <- renderText({nrow(hsInBounds())})
  
}

shinyApp(ui = ui, server = server)




