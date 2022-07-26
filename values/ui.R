##Front End Code
fluidPage(
  titlePanel("Textiles, Modifiers, and Value"),
  
  
  ##Our selection inputs, such as textile 1, modifers, etc.
  sidebarPanel(
    ##Textile 1 Selection
    selectInput(inputId="textileName1",
                label="Choose your textile",
                choices=sort(unique(wicvoc$textile_name))), ##unique() makes sure there are no duplicate options, sort() orders options alphabetically 
    
    ##This allows the user to choose a modifier for the first textile. The actual selection is created on server-side, since the relevant modifiers depend upon the selected textile
    uiOutput("modifier1Choice"), 
    
    ##This allows the user to choose a modifier for the second textile. The actual selection is created on server-side, since the relevant modifiers depend upon the selected textile
    uiOutput("modifier2Choice"),
    
    ##Selection for what variable should be on each axis
    selectInput(inputId="xAxisChoice",
                label="Choose a category for your X axis",
                choices=c("Year"="orig_yr","Destination Port"="loc_dest","Origin Port"="loc_orig")),##This allows the user to choose a modifier for the first textile
    selectInput(inputId="yAxisChoice",
                label="Choose a variable for your Y axis",
                choices =c("Total Value (Dutch Guldens)"="total_value","Price per Piece"="price_per_piece")),
    
    downloadButton("downloadData", "Download Current Data"),
    downloadButton("downloadDatafull", "Download Complete Data"),
  ),
  mainPanel(
    plotlyOutput("mainGraph")
  ),
)