# http://www.paulamoraga.com/book-geospatial/sec-shinyexample.html
# https://rstudio.github.io/leaflet/shiny.html
# https://github.com/r-spatial/mapview/issues/198
#
# Author:   Cristian E. Nuno
# Purpose:  Export leaflet map in Shiny with user drawn shapes
# Date:     December 13, 2018
#

# load necessary packages -----
library(leaflet)
library(leaflet.extras)
library(shiny)
library(mapview)

# create UI -----
ui <- fluidPage(
    
    sidebarLayout(position = "right",
                  sidebarPanel(numericInput("num", 
                                 h6("Nivel de confiança"), 
                                  min = 0, max=10,step=1,value=0),
                               width=2),
                  mainPanel("",width=0.1))
    ,
    
    
    fluidRow(
    column(3, 
           h4("Onde você viu",span(em("Rhea americana")),"?")),
           
    leafletOutput(outputId = "map")
    , br()
    , downloadButton(outputId = "download_table"
                     , label = "Download a .csv file")
    , verbatimTextOutput(outputId = "text1")
    , verbatimTextOutput(outputId = "text2")
    , verbatimTextOutput(outputId = "text3")
    , tableOutput(outputId = "my_table")
    , actionButton("click", "Colar a coordenada")
        #, ## nivel de confiabilidade no registro
       )
    
)
    



#server <- function(input, output){ }## simplest example (only UI)
# run app ----
#shinyApp(ui = ui, server = server)


# create server -----
#server <- function(input, output, session) {
#  
#  values <- reactiveValues()
#  values$df <- data.frame(lat = NA, #numeric(),
#                          long =NA, #numeric(), 
#                          conf =NA) #numeric())
#    
#    # build foundational map
#    foundational_map <- reactive({
#        
#        leaflet() %>% 
#        
#          addTiles () %>%
#            
#            #addProviderTiles("ESRI.WorldImagery") %>%
#            
#            addDrawToolbar(targetGroup = 'draw'
#                           , editOptions = editToolbarOptions(selectedPathOptions = 
#                                                                  selectedPathOptions())
#                           
#                           , polylineOptions = FALSE
#                           , polygonOptions = FALSE
#                           , circleOptions = FALSE
#                           , rectangleOptions = FALSE
#                           , circleMarkerOptions = FALSE
#                           , markerOptions=TRUE) %>%
#            
#            setView(lat = -30, lng = -50, zoom = 5) %>%
#            
#            addStyleEditor(position = "bottomleft", 
#                           openOnLeafletDraw = TRUE)
#    })
#    
#    # render foundational map
#    ## fazer o mapa somente uma vez
#    output$map <- renderLeaflet({
#        foundational_map()
#    })
#    
#    output$text2 <- renderPrint({
#      print(paste0("lat equals "
#                   , length(input$map_draw_all_features$features)))#[[1]]$geometry$coordinates[[2]]))
#      
#      values$data <- sapply(input$map_draw_all_features$features, function(x){
#        lat <- x$geometry$coordinates[[2]]
#        long <- x$geometry$coordinates[[1]]
#        list(values$data,
#        cbind(as.numeric(lat), as.numeric (long)))
#        
#        
#      })
#      
#      values$data
#    }
#    )
#    
#    ## conf
#    output$text3 <- renderPrint({
#      print(paste0("conf equals "
#                   , input$num))#[[1]]$geometry$coordinates[[2]]))
#      
#      values$conf <- sapply(seq (1,length(input$map_draw_all_features$features), function(x){
#        input$num[[x]]
#      }))
#      
#      values$conf
#    }
#    )
#    
#    # after the user places the first marker on the map
#    # display the lng
#    observeEvent(values$data, {
#      
#      output$text1 <- renderPrint({
#        mtx <- t(values$data)[ncol(values$data),]
#        add.row <- as.vector(mtx)
#        add.conf <- list(add.row, input$num)
#        so_conf <- add.conf [2]
#        #gambiarra <- cbind(do.call(rbind, add.conf[1]), so_conf)
#        #values$lista <- list(values$df, add.row)
#        #values$df <- do.call(rbind,values$lista)
#        #df_coord <- do.call(rbind, values$lista)
#        #values$df <- list(df_coord, add.row)
#      
#        list(values$data, mtx,add.row,add.conf,so_conf,add.conf[1])
#        #     do.call(rbind,values$lista))
#        
#      })
#
#    })
#    
#  
#    
#    # create download logic
#    # Execute selections on data upon button-press
#    # saving CSV
#    output$download_table  <- downloadHandler(
#        
#       filename <- paste0("file_EMA_", Sys.time(), ".csv"),
#        
#        content <- function(file) {
#        
#              write.csv(user_markers(), file, row.names = FALSE)
#        }
#    )
#    
#}
#
#
#

library (rgdal)
require(sp)
require(raster)
require(here)
mymun <- readOGR (dsn=here(), layer= "43MUE250GC_SIR")


# create UI -----
ui <- fluidPage(
  
 
    leafletOutput(outputId = "mymap")
    
  
)


## create server
server <- function (input,output,server) {
  
    output$mymap<- renderLeaflet({
      leaflet(data=mymun) %>%
        addTiles() %>%
        setView(lat=-31,lng=-52,zoom=6) %>% 
         addPolygons(fillColor="white"
                     , color= "white"
                     , stroke= FALSE
                     , highlight=highlightOptions(weight=0.5
                                                , color="gray"
                                                , fillOpacity= 0.5
                                                , fillColor="gray"
                                                , bringToFront = FALSE)
                     , label = ~ NM_MUNICIP)
    })
  
    observe ({
      click <- input$mymap_shape_click 
      if (is.null (click))
        return ()
      else 
        leafletProxy ("mymap") %>%
        setView (lng=click$lng, lat= click$lat, zoom = 6)
    })
}



# run app ----
shinyApp(ui = ui, server = server)

# end of script #












