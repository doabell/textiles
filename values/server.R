# Backend
function(input, output, session) {
  # No scientific notation
  options(scipen = 1000000)

  # Modifiers ####
  output$modifier1Choice <- renderUI({
    wicvoc.filtered <- filter(wicvoc, textile_name == input$textileName) # I filter so that we're only looking at modifiers for the selected textile. This line is why this is written in the server; I need to know what was selected for the textile
    modifiers <- setdiff(unique(c(wicvoc.filtered$textile_color_arch, wicvoc.filtered$textile_quality_arch)), c(NA))
    if (length(modifiers) != 0) {
      selectInput(
        inputId = "textile1mods",
        multiple = TRUE,
        label = paste("Choose one modifier for", input$textileName),
        choices = modifiers
      )
    } else {
      # TODO fix bug where switching from baftas to illegible
      # gives empty graph - reset modifiers or set them here
    }
    # we make the drop down options only what applies to the selected textile
  })

  output$modifier2Choice <- renderUI({ # Here, I create the modifier choice ui
    wicvoc.filtered <- filter(wicvoc, textile_name == input$textileName) # I filter so that we're only looking at modifiers for the selected textile. This line is why this is written in the server; I need to know what was selected for the textile
    modifiers <- setdiff(unique(c(wicvoc.filtered$textile_color_arch, wicvoc.filtered$textile_quality_arch)), c(NA))
    if (length(modifiers) != 0) {
      selectInput(
        inputId = "textile2mods",
        multiple = TRUE,
        label = paste("Choose another modifier for", input$textileName),
        choices = modifiers
      )
    } # we make the drop down options only what applies to the selected textile
  })

  # Main graph ####
  output$mainGraph <- renderPlotly({
    textile1.data <- filter(wicvoc, textile_name == input$textileName) # First, I split up the data into two parts,
    textile2.data <- textile1.data
    if (!is.null(input$textile1mods)) { # if no modifiers are selected for textile 1, I don't want to filter it at all
      # if(length(intersect(unique(wicvoc$textile_color_arch), input$textile1mods)) != 0){ #If modifiers are selected, but none are color modifiers, we don't want to filter for colors
      textile1.data %<>% filter(textile_color_arch %in% input$textile1mods | textile_quality_arch %in% input$textile1mods) # If we do select a color(s) as a modifier, we want to filter for that color(s)
      # }
      # if(length(intersect(unique(wicvoc$textile_quality_arch), input$textile1mods)) != 0){ #If modifiers are selected, but none are quality modifiers, we don't want to filter for quality
      # textile1.data %<>% filter(textile_quality_arch %in% input$textile1mods) #If we do select a quality modifier, we want to filter for that
      # }
      textile1.data %<>% mutate(textile_name = paste(textile_name, paste(input$textile1mods, collapse = ", "), sep = ": ")) # I use string manipulation such that textile_name reflects what we've filtered for when textile_name appears on the legend
    } #
    if (!is.null(input$textile2mods)) { # if no modifiers are selected for textile 2, I don't want to filter it at all
      # if(length(intersect(unique(wicvoc$textile_color_arch), input$textile2mods)) != 0){ #If modifiers are selected, but none are color modifiers, we don't want to filter for colors
      textile2.data %<>% filter(textile_color_arch %in% input$textile2mods | textile_quality_arch %in% input$textile2mods) # If we do select a color(s) as a modifier, we want to filter for that color(s)
      # }
      # if(length(intersect(unique(wicvoc$textile_quality_arch), input$textile2mods)) != 0){ #If modifiers are selected, but none are quality modifiers, we don't want to filter for quality
      # textile2.data %<>% filter(textile_quality_arch %in% input$textile2mods) #If we do select a quality modifier, we want to filter for that
      # }
      textile2.data %<>% mutate(textile_name = paste(textile_name, paste(input$textile2mods, collapse = ", "), sep = ": ")) # I use string manipulation such that textile_name reflects what we've filtered for when textile_name appears on the legend
    } #

    rbind(textile1.data, textile2.data) %>% # Concatenates the data for textile 1 and 2, which has already been filtered for modifiers
      filter(!is.na(total_value)) %>% # if we don't have value information, we can't include the textile on our graph
      mutate(total_value = as.numeric(total_value)) %>% # Converts guldens to numeric
      ggplot() +
      geom_col(aes_string(
        x = input$xAxisChoice,
        y = input$yAxisChoice,
        fill = "textile_name"
      ), position = "dodge") +
      labs(
        x = switch(input$xAxisChoice,
          "orig_yr" = "Year",
          "loc_dest" = "Destination",
          "loc_orig" = "Origin"
        ),
        y = switch(input$yAxisChoice,
          "textile_quantity" = "Quantity Shipped",
          "total_value" = "Total Value (Dutch Gulders)",
          "price_per_piece" = "Price Per Piece (Dutch Gulders)"
        ),
        fill = "Textile"
      ) + # Note the use of switch in the two lines above. This is so that we have nice label titles for each selection of our axises.
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) -> mainggplot

    ggplotly(mainggplot)
  })

  # Download buttons ####
  output$downloadData <- downloadHandler(
    # use function for reactiveness
    # see ?downloadHandler
    filename = function() {
      input$textileName %>%
        str_replace_all(" ", "_") %>%
        paste0("_textile_data.csv")
    },
    content = function(file) {
      filter(wicvoc, textile_name == input$textileName) %>%
        write_csv(file)
    },
    contentType = "text/csv"
  )

  output$downloadDatafull <- downloadHandler(
    filename = "full_textile_data.csv",
    content = function(file) {
      write_csv(wicvoc, file)
    },
    contentType = "text/csv"
  )
}
