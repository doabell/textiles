# @Middlebury College - 2021
# Interactive textile explorer
# Originally authored by Ev Berger-Wolf, Camryn Kluetmeier, Jason Rickenbacker, and Nicholas Sliter
# Under the instruction of Prof. Carrie Anderson at Middlebury College
# Code maintained and extended by Nicholas Sliter




#Project initial
library(shiny)
library(shinythemes)
library(readxl)
library(rgdal)
library(tidyverse)
library(stringr)
library(debkeepr)
library(leaflet)
library(viridis)
library(jsonlite)
library(plotly)
library(shinyWidgets)

#source to function file
source('functions.R')


#define constants:
#reference with CONSTANTS["NAME"]
CONSTANTS <- c(
  
  
  "SHINY_THEME" = "sandstone",
  "COLOR_THEME" = "magma",
  "COLORS" = toString(c("white","yellow","red","blue","purple","green","black", "brown", "grey", "silver", "gold"))
  
  
  
)


#> strsplit((CONSTANTS["COLORS"]), ",")[[1]]


# Create zoom locations
latLongZoom.original <- data.frame("Area" = c("World", "Europe", "Africa",
                                              "Middle East", "Pacfic Islands", "Asia"),
                                   "Lat" = c(30, 49.8, -6, 27, 0, 32),
                                   "Long" = c(53, 15.47, 30, 72.5, 116, 115),
                                   "Magnify" = c(2, 4.25, 2.5, 4, 4, 3.25))

latLongZoom <- latLongZoom.original

#Read in the data
joined.data.original <- read_csv("joined.csv")
map.data.original <- readOGR("filteredCountries.GeoJSON")

#make copies of original data
joined.data <- joined.data.original

#convert JSON col to nonJSON
#joined.data <- joined.data.original %>% mutate(colorList = vec_unflatten(colorList))


#Fix Facet Wrapping Issue (deal with this after presentation)
#joined.data$textile_quality_inferred <- factor(joined.data$textile_quality_inferred,
 #                                              levels = c("Inexpensive", "Mid-Range", "Expensive"))
map.data <- map.data.original

#Creating a modifier choice vector
modVec <- c("Textile Name" = "textile_name",
            #"Color" = "colorGroup",
            "Color" = "colorList",
            "Pattern" = "textile_pattern_arch",
            "Process" = "textile_process_arch",
            "Fiber Type" = "textile_fiber_arch",
            "Value Range" = "textile_quality_inferred",
            "Geography" = "textile_geography_arch",
            "Quality" = "textile_quality_arch")


#Creating the UI
ui <- fluidPage(theme = shinytheme("sandstone"),
                titlePanel("Interactive Textile Explorer"),
                sidebarPanel(#All inputs will go in this sidebarPanel
                  h4("Explore different facets of the data by selecting inputs below:"),
                  radioButtons(inputId = "dataSet",
                               label = "Choose company of interest",
                               choices = c("WIC", "VOC", "Both"),
                               selected = "Both"),
                  radioButtons(inputId = "dataType",
                               label = "Choose data type of interest",
                               choices = c("Quantity", "Value"),
                               selected = "Quantity"),
                  radioButtons(inputId = "regionChoice",
                               label = "Select one",
                               choices = c("Origin", "Destination"),
                               selected = "Origin"),
                  
                  selectizeInput(inputId = "zoomTo",
                                 label = "Zoom to:",
                                 choices = levels(factor(latLongZoom$Area)),
                                 selected = "World"),
                  
                  # selectizeInput(inputId = "textileName",
                  #                label = "Choose textile(s) of interest",
                  #                choices = levels(factor(joined.data$textile_name)),
                  #                multiple = TRUE),
                  # # uiOutput(outputId = 'filtered_inputs'),
                  # selectizeInput(inputId = "colors",
                  #                label = "Choose color(s) of interest",
                  #                #choices = levels(factor(joined.data$colorGroup)),
                  #                #choices = levels(factor(joined.data$colorList)),
                  #                
                  #                choices = {
                  #                  strsplit(CONSTANTS['COLORS'], ", ")[[1]]
                  #                  
                  #                  },
                                 
                                 
                                 
                  #               multiple = TRUE),
                  uiOutput(outputId = "TextileName"),
                  uiOutput(outputId = "Colors"),
                  uiOutput(outputId = "Pattern"),
                  uiOutput(outputId = "Process"),
                  uiOutput(outputId = "Fibers"),
                  uiOutput(outputId = "InferredQualities"),
                  uiOutput(outputId = "Geography"),
                  uiOutput(outputId = "Qualities"),
                  uiOutput(outputId = "Year"),

                  # selectizeInput(inputId = "patterns",
                  #                label = "Choose pattern(s) of interest",
                  #                choices = levels(factor(joined.data$textile_pattern_arch)),
                  #                multiple = TRUE),
                  # selectizeInput(inputId = "process",
                  #                label = "Choose process(es) of interest",
                  #                choices = levels(factor(joined.data$textile_process_arch)),
                  #                multiple = TRUE),
                  # selectizeInput(inputId = "fibers",
                  #                label = "Choose fiber(s) of interest",
                  #                choices = levels(factor(joined.data$textile_fiber_arch)),
                  #                multiple = TRUE),
                  # selectizeInput(inputId = "inferredQualities",
                  #                label = "Choose value range(s) of interest",
                  #                choices = levels(factor(joined.data$textile_quality_inferred)),
                  #                multiple = TRUE),
                  # selectizeInput(inputId = "geography",
                  #                label = "Choose geography of interest",
                  #                choices = levels(factor(joined.data$textile_geography_arch)),
                  #                multiple = TRUE),
                  # selectizeInput(inputId = "qualities",
                  #                label = "Choose quality(s) of interest",
                  #                choices = levels(factor(joined.data$textile_quality_arch)),
                  #                multiple = TRUE),
                  # selectizeInput(inputId = "year",
                  #                label = "Year:",
                  #                choices = levels(factor(c(joined.data$orig_yr,joined.data$dest_yr))),
                  #                multiple = TRUE),
                  actionButton(inputId = "updateBtn",
                               label = "Click to update map!"),
                  br(), br(),
                  actionButton(inputId = 'table_updateBtn',
                               label = 'Click to update table!'),
                  br(), br(), #The inputs for the pie chart and bar chart
                  selectInput(inputId = "pieChart",
                              label = "Choose a modifier for the pie chart:",
                              choices = modVec,
                              selected = "textile_name"),
                  checkboxInput(inputId = "omitNAs",
                                label = "Omit NAs in charts"),
                  selectInput(inputId = "barChart",
                              label = "Choose a modifier for the bar chart:",
                              choices = modVec,
                              selected = "textile_name"),
                  checkboxInput(inputId = "facet",
                                label = "Facet by modifier"),
                  actionButton(inputId = 'graph_updateBtn',
                               label = 'Click to update graphs!')
                ),
                mainPanel(
                  tabsetPanel(#All of the outputs go here (map/graphs, data tables)
                    tabPanel(title = "Map Explorer",
                             leafletOutput(outputId = "countriesMap"),
                             plotOutput(outputId = "pieChart"),
                             plotOutput(outputId = "barChart") #outputId = 
                    ),
                    tabPanel(title = "Table Explorer",
                             dataTableOutput('update_inputs'),
                             downloadButton("downloadData", "Download Table") #download button
                    )
                  )
                )
)

server <- function(input, output, session) {

  
  
  
  # 
  # res_mod <- callModule(
  #   module = selectizeGroupServer,
  #   id = "my-filters",
  #   data = joined.data.original,
  #   vars = c("var_one", "var_two", "var_three", "var_four", "var_five")
  # )  
  # 
  
  
  
  
  
  
  reactive_data <- reactive({
    input$updateBtn
    input$graph_updateBtn
    input$table_updateBtn
    
    #reading in all of the inputs, isolating them
    dataSet <- isolate(input$dataSet)
    dataType <- isolate(input$dataType)
    regionChoice <- isolate(input$regionChoice)
    textileName <- isolate(input$textileName)
    colors <- isolate(input$colors)
    patterns <- isolate(input$patterns)
    process <- isolate(input$process)
    fibers <- isolate(input$fibers)
    geography <- isolate(input$geography)
    qualities <- isolate(input$qualities)
    inferredQualities <- isolate(input$inferredQualities)
    area <- isolate(input$zoomTo)
    isolate(input)
    # table_update <- isolate(input$table_updateBtn)
    # graph_update <- isolate(input$graph_updateBtn)
    
    
    data <- joined.data
      
      
    private_filter_by <- function(d, col, data_col){
        if(length(col) != 0 && !is.null(col)){
          d <- d %>%
            filter(data_col %in% col)
        }
        return(d)
        
      }
    
    
    if(isolate(input$dataSet) != "Both"){
      data <- private_filter_by(data,isolate(input$dataSet),data$company)
    }
    data <- private_filter_by(data,isolate(input$textileName),data$textile_name)
    data <- filter_colors(data,isolate(input$colors))
    data <- private_filter_by(data,isolate(input$patterns),data$textile_pattern_arch)
    data <- private_filter_by(data,isolate(input$process),data$textile_process_arch)
    data <- private_filter_by(data,isolate(input$fibers),data$textile_fiber_arch)
    data <- private_filter_by(data,isolate(input$geography),data$textile_geography_arch)
    data <- private_filter_by(data,isolate(input$qualities),data$textile_quality_arch)
    data <- private_filter_by(data,isolate(input$inferredQualities),data$textile_quality_inferred)
    data <- private_filter_by(data,isolate(input$year),data[[return_yrColname(isolate(input$regionChoice))]])

    #browser()
    
    return (data)
    
    
    })
  
  
  output$TextileName <- renderUI({
    selectizeInput(inputId = "textileName",
                   label = "Choose textile(s) of interest",
                   choices = levels(factor(reactive_data()$textile_name)),
                   selected = input$textileName,
                   multiple = TRUE)
    
  })
  

  output$Colors <- renderUI({

    
    pre_unique <- str_split(unique(reactive_data()$colorList), ", ")
    
    list <- c()
    for (i in 1:length(pre_unique)){
      
      list <- append(list,pre_unique[[i]])
      
    }
    
    color_choices <- unique(as.vector(list))
    


    selectizeInput(inputId = "colors",
                   label = "Choose color(s) of interest",
                   choices = color_choices,
                   selected = isolate(input$colors),
                   multiple = TRUE
                   )

  })
  
  
  
  
  output$Pattern <- renderUI({
    patterns <-
      unique(as.vector(reactive_data()$textile_pattern_arch))

    selectizeInput(
      inputId = "Pattern",
      label = "Choose pattern(s) of interest",
      choices = patterns,
      selected = input$patterns,
      multiple = TRUE
    )
    
  })
  
  
  output$Process <- renderUI({
    

    selectizeInput(
                   inputId = "process",
                   label = "Choose process(es) of interest",
                   choices = levels(factor(reactive_data()$textile_process_arch)),
                   selected = input$process,
                   multiple = TRUE)
    
    
  })
  
  
  output$Fibers <- renderUI({
    
    selectizeInput(inputId = "fibers",
                   label = "Choose fiber(s) of interest",
                   choices = levels(factor(reactive_data()$textile_fiber_arch)),
                   selected = input$fibers,
                   multiple = TRUE)
    
    
  })
  
  output$InferredQualities <- renderUI({
    
    selectizeInput(inputId = "inferredQualities",
                   label = "Choose value range(s) of interest",
                   choices = levels(factor(reactive_data()$textile_quality_inferred)),
                   selected = input$inferredQualities,
                   multiple = TRUE)
    
    
    
  })
  
  output$Geography <- renderUI({
    
    selectizeInput(inputId = "geography",
                   label = "Choose geography of interest",
                   choices = levels(factor(reactive_data()$textile_geography_arch)),
                   selected = input$geography,
                   multiple = TRUE)
    
    
  })
  
  output$Qualities <- renderUI({
    
    
    user_choices <- levels(factor(joined.data$textile_quality_arch))
    
    
    selectizeInput(inputId = "qualities",
                   label = "Choose quality(s) of interest",
                   choices = user_choices,
                   selected = input$qualities,
                   multiple = TRUE)

    
    
    
  })
  
  output$Year <- renderUI({
    
    user_choices <-levels(factor(c(reactive_data()$orig_yr,reactive_data()$dest_yr)))
    
    selectizeInput(inputId = "year",
                   label = "Year:",
                   choices = user_choices,
                   selected = input$year,
                   multiple = TRUE)
    
    #user_choices in input year
    
  })


  observe
  
  
  
  
  #creates table
  output$update_inputs <- renderDataTable(searchDelay = 1000,{
    input$table_updateBtn
    #isolate(filter_by_inputs(joined.data.original,isolate(input)))}) #filters the data for what has been searched
    reactive_data()})
  
  # Downloadable .xls of table dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$table_updateBtn, ".xls", sep = "")
    },
    content = function(file) {
      write_excel_csv(
        
        #isolate(filter_by_inputs(joined.data.original,isolate(input)))
        
        reactive_data(), file)
    }
  )

  #The map of countries to be rendered
  output$countriesMap <- renderLeaflet({
    #We only want it to update when the updateBtn is pushed
    input$updateBtn
    
    #reading in all of the inputs, isolating them
    dataSet <- isolate(input$dataSet)
    dataType <- isolate(input$dataType)
    regionChoice <- isolate(input$regionChoice)
    textileName <- isolate(input$textileName)
    colors <- isolate(input$colors)
    patterns <- isolate(input$patterns)
    process <- isolate(input$process)
    fibers <- isolate(input$fibers)
    geography <- isolate(input$geography)
    qualities <- isolate(input$qualities)
    inferredQualities <- isolate(input$inferredQualities)
    area <- isolate(input$zoomTo)
    table_update <- isolate(input$table_updateBtn)
    graph_update <- isolate(input$graph_updateBtn)
    
    #Every time, we want to start with all of the data to filter through
    #joined.data <- joined.data.original
    
    #Use the function to filter the inputs
    #joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))
    
    joined.data <- reactive_data()
    
    
    choice <- get_regionChoice(regionChoice)
    totalValues <- filter_totalValue(joined.data,regionChoice,dataSet)
    
    map.data@data <- left_join(map.data.original@data, #Join with the map data, using the original map data each time
                               totalValues,
                               by = c("ADMIN" = choice))
    
    #This will be used to zoom to a specific region on the map
    latLongZoom <- latLongZoom.original %>%
      filter(Area == area)
    
    viewLat <- latLongZoom[,"Lat"]
    viewLong <- latLongZoom[,"Long"]
    viewZoom <- latLongZoom[,"Magnify"]

    #create the actual map
    create_leaflet_map(map.data,totalValues,dataType,c(viewLat,viewLong,viewZoom))
    
  })
  
  
  
  #want to integrate ggploty to have interactie charts
  
  
  #Used to render the plot for pie chart
  output$pieChart <- renderPlot({
    input$updateBtn
    input$graph_updateBtn
    name <- input$countriesMap_shape_click$id
    
    #only want to do this if they clicked on a country
    if(length(name) != 0){
      #Read in all of the inputs, but isolated
      modifier <- isolate(input$pieChart)
      dataSet <- isolate(input$dataSet)
      regionChoice <- isolate(input$regionChoice)
      textileName <- isolate(input$textileName)
      colors <- isolate(input$colors)
      patterns <- isolate(input$patterns)
      process <- isolate(input$process)
      fibers <- isolate(input$fibers)
      geography <- isolate(input$geography)
      qualities <- isolate(input$qualities)
      inferredQualities <- isolate(input$inferredQualities)
      
      #Again, reusing the original data
      #joined.data <- joined.data.original
      
      #Filter all the inputs
      #joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))
      
      
      joined.data <- reactive_data()
      
      choice <- get_regionChoice(regionChoice) #get dest or orig
      
      #We care specifically about the destination here
      pie.data <- joined.data %>%
        filter(joined.data[choice] == name) %>%
        select(textile_quantity,
               deb_dec,
               all_of(modifier),
               company)
      
      
      #   if(regionChoice == "Destination"){ #Only dest_country
      #   pie.data <- joined.data %>%
      #     filter(dest_country == name) %>%
      #     select(textile_quantity,
      #            deb_dec,
      #            all_of(modifier),
      #            company)
      # }
      # else { #Only orig_country
      #   pie.data <- joined.data %>%
      #     filter(orig_country == name) %>%
      #     select(textile_quantity,
      #            deb_dec,
      #            all_of(modifier),
      #            company)
      # }
      
      #Omit na of the selected columns to avoid errors
      if(input$omitNAs){
        if (modifier == "colorList"){
        pie.data <- pie.data %>%
          mutate(colorList = ifelse(colorList == "No color indicated",NA ,colorList))
        }
        pie.data <- pie.data %>%
          na.omit()
      }
      else{ #Fix a problem for if NA is the only data point
        pie.data[3][is.na(pie.data[3])] <- "None indicated"
      }
      
      if(dataSet != "Both"){ #Controlling for company selection
        pie.data <- pie.data %>%
          filter(company == dataSet)
      }
      
      if(isolate(input$dataType) == "Quantity"){ #If they're interested in quantity
        if(nrow(pie.data) != 0){ #check to see if there are values left to publish
          pie.data %>% 
            ggplot(aes(x="",
                       y = textile_quantity)) +
            geom_bar(stat="identity",
                     width=1,
                     aes_string(fill=modifier))+
            coord_polar("y", start=0) + #This line in particular changes the bar chart to a pie chart
            labs(x = NULL,
                 y = NULL,
                 fill = NULL) +
            scale_fill_viridis(discrete = TRUE,
                               name = paste(names(modVec)[modVec == modifier]),
                              option = "magma") +
            theme_void() +
            ggtitle(label = paste(names(modVec)[modVec == modifier], "distribution for", name, "with these filters."))
        }
        else{ #No rows were found
          ggplot() +
            ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
        }
      }
      else{ #This will do total value the same way, except graphing deb_dec
        if(nrow(pie.data) != 0){
          pie.data %>%
            ggplot(aes(x="",
                       y = deb_dec)) +
            geom_bar(stat="identity",
                     width=1,
                     aes_string(fill=modifier))+
            coord_polar("y", start=0) +
            labs(x = NULL,
                 y = NULL,
                 fill = NULL) +
            scale_fill_viridis(discrete = TRUE,
                               name = paste(names(modVec)[modVec == modifier]),
                               option = "magma") +
            theme_void() +
            ggtitle(label = paste(names(modVec)[modVec == modifier], "monetary distribution for", name, "with these filters."))
        }
        else{
          ggplot() +
            ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
        }
      }
    }
    else{ #This comes up if they have not clicked any countries
      ggplot() +
        ggtitle(label = "Select a country with data for these textiles in order to display a pie chart here.")
    }
  })
  
  #Rendering the bar chart - this works nearly the exact same way as the pie chart
  #except when it is graphing the outputs, it is doing so with a bar chart instead of a pie chart
  output$barChart <- renderPlot({
    input$updateBtn
    input$graph_updateBtn
    name <- input$countriesMap_shape_click$id
    
    values <- c()
    
    
    joined.data <- reactive_data()
    
    
    if(!is.null(name) && length(name) != 0){
      modifier <- isolate(input$barChart)
      modifierObj <- paste("`", names(modVec)[modVec == modifier], "`", sep = "")
      dataSet <- isolate(input$dataSet)
      dataType <- isolate(input$dataType)
      regionChoice <- isolate(input$regionChoice)
      textileName <- isolate(input$textileName)
      colors <- isolate(input$colors)
      patterns <- isolate(input$patterns)
      process <- isolate(input$process)
      fibers <- isolate(input$fibers)
      geography <- isolate(input$geography)
      qualities <- isolate(input$qualities)
      inferredQualities <- isolate(input$inferredQualities)
      #orig_yr <- isolate(input$orig_yr)
      year <- isolate(input$year)
      facet <- isolate(input$facet)
      #dest_yr <- isolate(input$dest_yr)
      
      
      
      values <- c(
        'name' = name,
        'modifier' = modifier,
        'modifierObj' = modifierObj,
        'dataSet' = dataSet,
        'dataType'= dataType,
        'regionChoice' =  regionChoice ,
        'textileName' = textileName,
        'colors' = colors,
        'patterns' = patterns,
        'process' = process,
        'fibers' = fibers,
        'geography' = geography,
        'qualities' = qualities,
        'inferredQualities' = inferredQualities,
        #'orig_yr' = orig_yr,
        'year' = year,
        'facet' = facet
        
      )
      
      
      
      #joined.data <- joined.data.original
      
      #joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))
      
      
      if(regionChoice == "Destination"){
        
        bar.data <- joined.data %>%
          
          filter(dest_country == name) %>%
          select(textile_quantity,
                 deb_dec,
                 orig_yr,
                 dest_yr,
                 all_of(modifier),
                 company)
      }
      else{
        bar.data <- joined.data %>%
          filter(orig_country == name) %>%
          select(textile_quantity,
                 deb_dec,
                 orig_yr,
                 dest_yr,
                 all_of(modifier),
                 company)
      }
      
      if(input$omitNAs){
        if (modifier == "colorList"){
        bar.data <- bar.data %>%
          mutate(colorList = ifelse(colorList == "No color indicated",NA, colorList))
        }
        
        bar.data <- bar.data %>%
          na.omit()
        
        
      }
      else{
        bar.data[4][is.na(bar.data[4])] <- "None indicated"
      }
      
      if(dataSet != "Both"){
        bar.data <- bar.data %>%
          filter(company == dataSet)
      }
      
      #ggplotly
      createBarChart(bar.data,values)
      

    }
    else{
      ggplot() +
        ggtitle(label = paste("No data for these filters."))
      
      
    }
    
  })
}

shinyApp(ui, server)
