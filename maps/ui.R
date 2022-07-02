#Creating the UI
fluidPage(theme = shinytheme("sandstone"),
                titlePanel("Interactive Textile Explorer"),
                sidebarPanel(#All inputs will go in this sidebarPanel
                  h4("Explore different facets of the data by selecting inputs below:"),
                  radioButtons(inputId = "dataSet",
                               label = "Choose company of interest",
                               # choices = c("WIC", "VOC", "Both"),
                               choiceNames = c("All Companies", "East India Company (VOC)", "West India Company (WIC)"),
                               choiceValues = c("Both", "VOC", "WIC"),
                               selected = "Both"),
                  radioButtons(inputId = "regionChoice",
                               label = "Select one",
                               choices = c("Origin", "Destination"),
                               selected = "Origin"),
                  radioButtons(inputId = "dataType",
                               label = "Choose data type of interest",
                               choices = c("Quantity", "Value"),
                               selected = "Quantity"),
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
                  # uiOutput(outputId = "InferredQualities"),
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
                  #The inputs for the pie chart and bar chart
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
                                label = "Facet by modifier")
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