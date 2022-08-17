# Creating the UI
fluidPage(
  # Fonts
  tags$head(tags$style(
    HTML("
      @import url('https://fonts.googleapis.com/css2?family=Lato&display=swap');
      * {font-family: 'Lato'} !important;
    ")
  )),
  
  sidebarPanel( # All inputs will go in this sidebarPanel
    pickerInput(
      inputId = "dataSet",
      label = "Choose company of interest",
      choices = c("Both", "VOC", "WIC"),
      # choiceNames = c("All Companies", "East India Company (VOC)", "West India Company (WIC)"),
      # choiceValues = c("Both", "VOC", "WIC"),
      selected = "Both",
      # justified = TRUE
      choicesOpt = list(
        subtext = c("VOC & WIC", "East India Company", "West India Company")
      )
    ),
    uiOutput("twoCountries"),
    radioGroupButtons(
      inputId = "regionChoice",
      label = "Select these countries as...",
      # choices = c("Origin", "Destination"),
      choices = c(`<i class='fa fa-plane-departure'></i> Origin` = "Origin", `<i class='fa fa-plane-arrival'></i> Destination` = "Destination"),
      selected = "Origin",
      justified = TRUE
    ),
    radioGroupButtons(
      inputId = "dataType",
      label = "Choose data of interest",
      # choices = c("Quantity", "Value"),
      choices = c(`<i class='fa fa-bar-chart'></i> Quantity` = "Quantity", `<i class='fa fa-money-bill-alt'></i> Value` = "Value"),
      selected = "Quantity",
      justified = TRUE
    ),

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
    # The inputs for the pie chart and bar chart
    selectInput(
      inputId = "pieChart",
      label = "Choose a modifier for the pie chart:",
      choices = modVec,
      selected = "textile_name"
    ),
    selectInput(
      inputId = "barChart",
      label = "Choose a modifier for the bar chart:",
      choices = modVec,
      selected = "textile_name"
    )
  ),
  mainPanel(
    tabsetPanel( # All of the outputs go here (map/graphs, data tables)
      tabPanel(
        title = "Map Explorer",
        tabsetPanel(
          tabPanel(
            title = "Country 1",
            leafletOutput(outputId = "countriesMap"),
            plotOutput(outputId = "pieChart"),
          ),
          tabPanel(
            title = "Country 2",
            leafletOutput(outputId = "countriesMap2"),
            plotOutput(outputId = "pieChart2"),
          )
        ),
        plotOutput(outputId = "barChart")
      ),
      tabPanel(
        title = "Table Explorer",
        dataTableOutput("update_inputs"),
        downloadButton("downloadData", "Download Table") # download button
      )
    )
  )
)
