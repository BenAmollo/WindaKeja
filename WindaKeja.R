library(geoloc)
library(stringr)    
library(DT)
library(leaflet)
library(googlesheets4)





# 
#  write_sheet(MyDataFrame, SHEET_ID, "Mtaaste")
#  dt = read_sheet(SHEET_ID, "Mtaaste") 
# 
# datatable(dt)


BedroomNum <- c("Bed Sitter", "Studio", "Single Room", "One Bedroom", "Two Bedroom", "Three Bedroom",
                "Four Bedroom", "Five Bedroom", "Six Bedroom")
HouseType <- c("Flat/Apartment", "Bungalow", "Maisonette", "Town House",
               "Duplex")


runApp(
  list(
    ui = fluidPage(
      headerPanel('GeoKamp'),
      sidebarPanel(
        textInput("mes", label="Enter the Neighbourhood:", value="Langata"),
        selectInput("concepto", label="House Typology",
                    selected = "Bungalow", HouseType),
        selectInput("partida", label="Number of bedrooms",
                    selected="One Bedroom", BedroomNum),
        numericInput("actividad", label="Number of bathrooms", value=""),
        numericInput("monto", label="Rent per month", value=""),
        radioButtons(inputId = "montizoo", label = "Are pets allowed?",
                     choices = c("TRUE", "FALSE")),
        radioButtons(inputId = "parkizo", label = "Is parking available?",
                     choices = c("TRUE", "FALSE")),
        textInput("amenitia", label="Any other amenitis", value="e.g. Swiming pool"),
        geoloc::button_geoloc("myBtn", "Get my Location"),
        actionButton("addButton", "SUBMIT")
      ),
      mainPanel(
        leafletOutput("kejaste"),
        leafletOutput("makeja"),
        dataTableOutput("table"))
    ),
    
    server = function(input, output, session) {    
      
      output$makeja <- renderLeaflet({
        req(input$myBtn_lon)
        req(input$myBtn_lat)
        leaflet() %>%
          addTiles() %>%
          setView(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), zoom = 17) %>%
          addMarkers(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), label = "You're here!")
      })
      
      output$kejaste <- renderLeaflet({
        leaflet() %>% 
          addTiles() %>% 
          addMarkers(data = dto, lng = as.numeric(dto$Longitude), lat = as.numeric(dto$Latitude))
        
      })
      
      # just a small part of iris for display
      iris_sample <- MyDataFrame
      row.names(iris_sample) <- NULL
      
      # The important part of reactiveValues()
      values <- reactiveValues()
      values$df <- iris_sample
      addData <- observe({
        
        # your action button condition
        if(input$addButton > 0) {
          
          click <- input$makeja_click
          clat <- click$lat
          clng <- click$lng
          content <- paste(clat, ",", clng)
          
          
          my_lat <- as.numeric(input$myBtn_lat)
          my_long <- as.numeric(input$myBtn_lon)
          My_LatLong <- paste(my_lat,"," ,my_long)
          # create the new line to be added from your inputs
          newLine <- isolate(c(input$mes, input$concepto, input$partida, input$monto,
                               input$actividad, input$montizoo, input$parkizo, input$amenitia, as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat)))
          # update your data
          # note the unlist of newLine, this prevents a bothersome
          # warning message that the rbind will return regarding rownames
          # because of using isolate.
          isolate(values$df <- rbind(as.matrix(values$df), unlist(newLine)))
          
          MyDataFrame <- as.data.frame(values$df)
          sheet_append(SHEET_ID, MyDataFrame, "Mtaaste")
        }
      })
      
      
      
      gs4_auth(email = "geokamp001@gmail.com")
      
      
      
      
      
      SHEET_ID <- gs4_create("Dataste", sheets = c("Mtaaste"))
      
      
      MyDataFrame <- data.frame(Neighbourhood = character(0),
                                House_Typology = character(0),
                                Number_of_Bedrooms = integer(0),
                                Rent_Per_Month = integer(0),
                                Number_of_bathrooms = integer(0),
                                Pets  = logical(0),
                                Parking = logical(0),
                                Other_amenities = character(0),
                                Longitude = double(0),
                                Latitude = double(0))
      
      
      output$table<- renderDataTable({
        dto <- read_sheet(SHEET_ID, "Mtaaste")
        drops <- c("Longitude","Latitude")
        dtoDrops <- dto[ , !(names(dto) %in% drops)]
        dtoDrops
      })
      
      
      # output$table <- renderTable({
      #   values$df}, include.rownames=T)
      
      
    }
  )
)
