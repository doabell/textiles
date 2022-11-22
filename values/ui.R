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
    switchInput(
      inputId = "modifier1And",
      onLabel = "AND",
      onStatus = "success",
      offLabel = "OR",
      offStatus = "info"
    ),
    # tags$p("Choose ", tags$b("OR"), " to see results that match ", tags$i("any"), " of the selected modifiers"),
    # tags$p("Choose ", tags$b("AND"), " to see results that match ", tags$i("all"), " of the selected modifiers"),
    uiOutput("mod1Warn"),


    # Second modifier
    uiOutput("modifier2Choice"),

    # And or or?
    switchInput(
      inputId = "modifier2And",
      onLabel = "AND",
      onStatus = "success",
      offLabel = "OR",
      offStatus = "info"
    ),
    # tags$p("Choose ", tags$b("OR"), " to see results that match ", tags$i("any"), " of the selected modifiers"),
    # tags$p("Choose ", tags$b("AND"), " to see results that match ", tags$i("all"), " of the selected modifiers"),
    uiOutput("mod2Warn"),

    # X-axis
    selectInput(
      inputId = "xAxisChoice",
      label = "Choose a variable for the x-axis",
      choices = c(
        "Year" = "orig_yr",
        "Destination Region" = "dest_loc_abr",
        "Origin Region" = "orig_loc_abr"
      )
    ),

    # Y-axis
    selectInput(
      inputId = "yAxisChoice",
      label = "Choose a variable for the y-axis",
      choices = c(
        "Quantity Shipped" = "textile_quantity",
        "Total Value (Dutch guilders)" = "total_value",
        "Average Price per Unit (Dutch guilders)" = "price_per_unit"
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
