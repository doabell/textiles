# UI
fluidPage(
  # titlePanel("Textiles, Modifiers, and Values"),

  # Fonts
  tags$head(tags$style(
    HTML("
      @import url('https://fonts.googleapis.com/css2?family=Lato&display=swap');
      * {font-family: 'Lato'} !important;
    ")
  )),

  # Inputs ####
  sidebarPanel(

    # Which textile
    selectInput(
      inputId = "textileName",
      label = "Choose a textile",
      choices = sort(unique(wicvoc$textile_name))
    ),

    # First modifier
    uiOutput("modifier1Choice"),

    # And or or?
    # FALSE: or
    # TRUE: and
    tags$label("Connected with..."),
    switchInput(
      inputId = "modifier1And",
      onLabel = "And",
      onStatus = "success",
      offLabel = "Or",
      offStatus = "info"
    ),
    
    uiOutput("mod1Warn"),


    # Second modifier
    uiOutput("modifier2Choice"),

    # And or or?
    tags$label("Connected with..."),
    switchInput(
      inputId = "modifier2And",
      onLabel = "And",
      onStatus = "success",
      offLabel = "Or",
      size = "mini",
      offStatus = "info"
    ),
    
    uiOutput("mod2Warn"),

    # X-axis
    selectInput(
      inputId = "xAxisChoice",
      label = "Choose a variable for your x-axis",
      choices = c(
        "Year" = "orig_yr",
        "Destination Port" = "dest_loc_abr",
        "Origin Port" = "orig_loc_abr"
      )
    ),

    # Y-axis
    selectInput(
      inputId = "yAxisChoice",
      label = "Choose a variable for your y-axis",
      choices = c(
        "Quantity Shipped" = "textile_quantity",
        "Total Value (Dutch Guldens)" = "total_value",
        "Price per Unit" = "price_per_unit"
      )
    ),

    # Download buttons
    downloadButton("downloadData", "Download Current Data"),
    downloadButton("downloadDatafull", "Download Complete Data"),
  ),

  # Output plotly ####
  mainPanel(
    plotlyOutput("mainGraph", height = "640px"),
    # Table for debugging
    # tableOutput('table'),
  ),
)
