# UI
fluidPage(
  titlePanel("Textiles, Modifiers, and Values"),

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
      label = "Choose your textile",
      choices = sort(unique(wicvoc$textile_name))
    ),

    # First modifier
    uiOutput("modifier1Choice"),

    # Second modifier
    uiOutput("modifier2Choice"),

    # X-axis
    selectInput(
      inputId = "xAxisChoice",
      label = "Choose a variable for your x-axis",
      choices = c(
        "Year" = "orig_yr",
        "Destination Port" = "loc_dest",
        "Origin Port" = "loc_orig"
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
    plotlyOutput("mainGraph"),
    # Table for debugging
    # tableOutput('table'),
  ),
)
