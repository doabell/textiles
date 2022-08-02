# Backend
function(input, output, session) {
  # No scientific notation
  options(scipen = 1000000)

  # Current textile

  # Modifiers ####
  output$modifier1Choice <- renderUI({
    # Grab current textile
    current_textile <<- filter(
      wicvoc,
      textile_name == input$textileName
    )

    # List all modifiers for this textile
    modifiers <<- current_textile %>%
      # Columns defined in global.R
      select(all_of(modvec)) %>%
      sapply(unique)

    modifiers_list <<- modifiers %>%
      reduce(union) %>%
      setdiff(c(NA))

    if (length(modifiers_list) != 0) {
      selectInput(
        inputId = "textile1mods",
        multiple = TRUE,
        label = paste("Choose one modifier for", input$textileName),
        choices = modifiers_list
      )
    } else {
      # TODO fix bug where switching from baftas to illegible
      # gives empty graph - reset modifiers or set them here
    }
  })

  output$modifier2Choice <- renderUI({
    if (length(modifiers_list) != 0) {
      selectInput(
        inputId = "textile2mods",
        multiple = TRUE,
        label = paste("Choose another modifier for", input$textileName),
        choices = modifiers_list
      )
    } else {
      # TODO fix bug where switching from baftas to illegible
      # gives empty graph - reset modifiers or set them here
    }
  })

  # Main graph ####
  output$mainGraph <- renderPlotly({
    
    # Grab current textile again for reactiveness
    current_textile <<- filter(
      wicvoc,
      textile_name == input$textileName
    )

    # Modifier 1
    # if (!is.null(input$textile1mods)) {
      # Grab all rows with modifier
      mod1data <- current_textile %>%
        rowwise() %>%
        # AND: all(x %in% y)
        # works when input is NULL
        # OR: any(x %in% y)
        # TODO OR, does not work if input is NULL
        filter(all(input$textile1mods %in% c_across(
          all_of(modvec)
        ))) %>%
        # Rename
        mutate(textile_name = paste0(
          textile_name, ": ",
          toString(input$textile1mods)
        ))
    # }
    
    # Modifier 2
    # if (!is.null(input$textile2mods)) {
      # Grab all rows with modifier
      mod2data <- current_textile %>%
        rowwise() %>%
        # AND: all(x %in% y)
        # works when input is NULL
        # OR: any(x %in% y)
        # TODO OR, does not work if input is NULL
        filter(all(input$textile2mods %in% c_across(
          all_of(modvec)
        ))) %>%
        # Rename
        mutate(textile_name = paste0(
          textile_name, ": ",
          toString(input$textile2mods)
        ))
    # }
    
    # Merge two data frames
    # If both null: copy
    if (is.null(input$textile1mods) & is.null(input$textile2mods)) {
      plot_data <- current_textile
    } else{
      plot_data <- bind_rows(mod1data, mod2data) %>%
        # Plot what we have data on
        filter(!is.na(input$yAxisChoice))
    }
    

    # Aggregate y by x
    # y: quantity shipped (sum)
    # y: total value (sum)
    # y: price per piece (average)
    # TODO deal with units
    
    plot_data %<>%
      group_by(textile_name, !!sym(input$xAxisChoice)) %>%
      summarise(price_per_piece = mean(price_per_piece),
                textile_quantity = sum(textile_quantity),
                total_value = sum(total_value))
    
    # Table for debugging
    # output$table <- renderTable(plot_data)
    
    # Finally plot
    mainggplot <- plot_data %>%
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
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 80)) +
      # Title
      ggtitle(paste("Values for", input$textileName))

    ggplotly(mainggplot) %>%
      layout(font = list(family = "Lato"))
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
      current_textile %>%
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
