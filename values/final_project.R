##Importing Libraries
library(shiny)
library(readxl)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(geojsonio)
library(writexl)
library(ggthemes)
library(dplyr)
library(forcats)


wicvoc <- read_csv("final_data.csv")
wicvoc$orig_yr <- factor(wicvoc$orig_yr) #This line makes year a discrete variable, such that values such as "1720.5" don't show up
wicvoc%>%mutate(pps= total_value_dutch_gulders/as.numeric(textile_quantity))


descript <- read_csv("TextileTermsList1.csv")

##Front End Code
ui <- fluidPage(
  titlePanel("Textiles, Modifiers, and Value"),
  
  
  ##Our selection inputs, such as textile 1, modifers, etc.
  sidebarPanel(
      ##Textile 1 Selection
      selectInput(inputId="textileName1",
                  label="Choose the first textile",
                  choices=sort(unique(wicvoc$textile_name))), ##unique() makes sure there are no duplicate options, sort() orders options alphabetically 
      
      ##This allows the user to choose a modifier for the first textile. The actual selection is created on server-side, since the relevant modifiers depend upon the selected textile
      uiOutput("modifier1Choice"), 
      
      ##Textile 2 selection
      selectInput(inputId="textileName2",
                  label="Choose the second textile",
                  choices=sort(unique(wicvoc$textile_name))), ##unique() makes sure there are no duplicate options, sort() orders options alphabetically
      
      ##This allows the user to choose a modifier for the second textile. The actual selection is created on server-side, since the relevant modifiers depend upon the selected textile
      uiOutput("modifier2Choice"),
      
      ##Selection for what variable should be on each axis
      selectInput(inputId="xAxisChoice",
                  label="Choose a category for your X axis",
                  choices=c("Year"="orig_yr","Destination Port"="dest_loc_region_arch","Origin Port"="orig_loc_region_arch")),##This allows the user to choose a modifier for the first textile
      selectInput(inputId="yAxisChoice",
                  label="Choose a variable for your Y axis",
                  choices =c("Total Value (Dutch Guldens)"="total_value_dutch_gulders","Price per Piece"="pps")),
  
      downloadButton("downloadData", "Download Current Data"),
      downloadButton("downloadDatafull", "Download Complete Data"),
  ),
  mainPanel(
    tabsetPanel( ##Placing our actual graph, as well as the other tabs
      tabPanel("Plot",plotOutput("mainGraph")),
      tabPanel("Description",textOutput("description"),tags$head(tags$style("#description{ font-size:12px; font-style:italic; 
 height: 500px; max-width: 600px; background: ghostwhite;overflow-wrap: word-wrap;}")))##Sets the css for the description page
    )
  ),
  
)


##Back End Code
server <- function(input, output, session) {
    options(scipen = 1000000)#Makes it so it doesn't use scientific notation
  
    output$summary <- renderText("Text")##Renders the text in summary
    
    output$description <- renderText({##Renders the text in the description panel
      de <- descript%>%filter(Term==input$textileName1)
      paste("Textile 1: ",toString(de$Description))
    }
    )

  output$modifier1Choice <- renderUI({ #Here, I create the modifier choice ui
    wicvoc.filtered <- filter(wicvoc, textile_name == input$textileName1) #I filter so that we're only looking at modifiers for the selected textile. This line is why this is written in the server; I need to know what was selected for the textile
    selectInput(inputId="textile1mods",
                multiple=TRUE,
                label="Choose a modifier for your first textile",
                choices=setdiff(unique(tolower(c(wicvoc.filtered$textile_color_arch, wicvoc.filtered$textile_quality_arch))), c(NA))
                )#we make the drop down options only what applies to the selected textile
  })
  
  output$modifier2Choice <- renderUI({ #Here, I create the modifier choice ui
    wicvoc.filtered <- filter(wicvoc, textile_name == input$textileName2) #I filter so that we're only looking at the selected textile. This line is why this is written in the server; I need to know what was selected for the textile
    selectInput(inputId="textile2mods",
                multiple=TRUE,
                label="Choose a modifier for your second textile",
                choices=setdiff(unique(tolower(c(wicvoc.filtered$textile_color_arch, wicvoc.filtered$textile_quality_arch))), c(NA))
                )#we make the drop down options only what applies to the selected textile
  })
    
  output$mainGraph <- renderPlot({
    textile1.data <- filter(wicvoc, textile_name == input$textileName1) #First, I split up the data into two parts,
    textile2.data <- filter(wicvoc, textile_name == input$textileName2) #so that I can filter each textile individually
    if(!is.null(input$textile1mods)) { #if no modifiers are selected for textile 1, I don't want to filter it at all
      if(length(intersect(unique(wicvoc$textile_color_arch), input$textile1mods)) != 0){ #If modifiers are selected, but none are color modifiers, we don't want to filter for colors
        textile1.data <- filter(textile1.data, textile_color_arch %in% input$textile1mods) #If we do select a color(s) as a modifier, we want to filter for that color(s)
      }
      if(length(intersect(unique(wicvoc$textile_quality_arch), input$textile1mods)) != 0){ #If modifiers are selected, but none are quality modifiers, we don't want to filter for quality
        textile1.data <- filter(textile1.data, textile_quality_arch %in% input$textile1mods) #If we do select a quality modifier, we want to filter for that
      }
     textile1.data <- mutate(textile1.data, textile_name=paste(textile_name, paste(input$textile1mods, collapse =', '), sep = ": "))#I use string manipulation such that textile_name reflects what we've filtered for when textile_name appears on the legend
    }###########################################################################################################################################################################################################################################
    if(!is.null(input$textile2mods)) {#if no modifiers are selected for textile 2, I don't want to filter it at all
      if(length(intersect(unique(wicvoc$textile_color_arch), input$textile2mods)) != 0){ #If modifiers are selected, but none are color modifiers, we don't want to filter for colors
        textile2.data <- filter(textile2.data, textile_color_arch %in% input$textile2mods) #If we do select a color(s) as a modifier, we want to filter for that color(s)
      }
      if(length(intersect(unique(wicvoc$textile_quality_arch), input$textile2mods)) != 0){ #If modifiers are selected, but none are quality modifiers, we don't want to filter for quality
        textile2.data <- filter(textile2.data, textile_quality_arch %in% input$textile2mods) #If we do select a quality modifier, we want to filter for that
      }
      textile2.data <- mutate(textile2.data, textile_name=paste(textile_name, paste(input$textile2mods, collapse =', '), sep = ": "))#I use string manipulation such that textile_name reflects what we've filtered for when textile_name appears on the legend
    }###########################################################################################################################################################################################################################################
    rbind(textile1.data, textile2.data)%>% #Concatenates the data for textile 1 and 2, which has already been filtered for modifiers
      filter(!is.na(total_value_guldens)) %>% #if we don't have value information, we can't include the textile on our graph
      mutate(total_value_guldens=as.numeric(total_value_guldens))%>%#Converts guldens to numeric
      mutate(pps= total_value_dutch_gulders/as.numeric(textile_quantity))%>%#makes the total value for price per ps
      ggplot()+
      geom_col(aes_string(x=input$xAxisChoice,
                          y=input$yAxisChoice,
                          fill="textile_name"),position = "dodge")+
      labs(x=switch(input$xAxisChoice,"orig_yr"="Year","dest_loc_region_arch"="Destination","orig_loc_region_arch"="Origin"),
           y=switch(input$yAxisChoice,"total_value_dutch_gulders"="Total Value (Dutch Gulders)","pps"="Price Per Piece (Dutch Gulders)"),
           fill="Textile")+ #Note the use of switch in the two lines above. This is so that we have nice label titles for each selection of our axises.
      theme_bw()+
      theme(axis.text.x=element_text(angle=90))
    
  })
  
  output$downloadData <- downloadHandler(filename = "Modified_Textile_Data.csv",
                                         content = function(file){
                                           write.csv(rbind(textile1.data, textile2.data), file)
                                         })
  
  output$downloadDatafull <- downloadHandler(filename = "Textile_Data.csv",
                                             content = function(file){
                                               write.csv(wicvoc, file)
                                             })
  
}

shinyApp(ui, server)