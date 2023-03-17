# Backend
function(input, output, session) {
  # No scientific notation
  options(scipen = 1000000)

  # When user selects a textile
  # Mutate data on current textile
  # Removes NAs for the selected y axis
  # Call with current_textile()
  current_textile <- eventReactive(
    c(input$textileName, input$yAxisChoice),
    {
      wicvoc %>%
        filter(
          textile_name == input$textileName,
          textile_unit == unitvec[[input$textileName]]
        ) %>%
        drop_na(input$yAxisChoice)
    }
  )

  # Keep NAs for user download
  current_textile_download <- eventReactive(
    input$textileName,
    {
      wicvoc %>%
        filter(
          textile_name == input$textileName,
          textile_unit == unitvec[[input$textileName]]
        )
    }
  )

  # When user selects a textile
  # List all modifiers for this textile
  # Call with modifiers_list()
  # Modifier columns `modvec` defined in global.R
  modifiers_list <- eventReactive(
    input$textileName,
    {
      current_textile() %>%
        select(all_of(modvec)) %>%
        sapply(unique) %>%
        map(sort) %>%
        map(as.list) %>%
        setNames(modnames)
    }
  )

  # Modifier choice UIs ####
  output$modifier1Choice <- renderUI({
    # if (length(modifiers_list()) != 0) {
    selectInput(
      inputId = "textile1mods",
      multiple = TRUE,
      label = paste("Choose modifier(s) for", input$textileName),
      choices = modifiers_list()
    )
    # }
  })

  output$modifier2Choice <- renderUI({
    # if (length(modifiers_list()) != 0) {
    selectInput(
      inputId = "textile2mods",
      multiple = TRUE,
      label = paste("Choose additional modifier(s) for", input$textileName),
      choices = modifiers_list()
    )
    # }
  })

  # Main graph ####
  output$mainGraph <- renderPlotly({

    # Modifier 1 ####
    # Contains modifiers
    if (!is.null(input$textile1mods)) {

      # TRUE: AND
      if (input$modifier1And) {
        # Grab all rows with modifier
        mod1data <- current_textile() %>%
          rowwise() %>%
          # AND: all(x %in% y)
          filter(
            all(
              input$textile1mods %in% c_across(all_of(modvec))
            )
          ) %>%
          # Rename
          mutate(textile_name = paste0(
            textile_name, ": ",
            toString(input$textile1mods)
          ))
      } else {
        # FALSE: OR
        # Grab all rows with modifier
        mod1data <- current_textile() %>%
          rowwise() %>%
          # OR: any(x %in% y)
          filter(
            any(
              input$textile1mods %in% c_across(all_of(modvec))
            )
          ) %>%
          # Rename
          mutate(textile_name = paste0(
            textile_name, ": ",
            toString(input$textile1mods)
          ))
      }
    } else {
      # No modifiers
      mod1data <- current_textile() %>%
        mutate(textile_name = paste0(
          textile_name, ": all"
        ))
    }

    # Modifier 2 ####
    # Contains modifiers
    if (!is.null(input$textile2mods)) {

      # TRUE: AND
      if (input$modifier2And) {
        # Grab all rows with modifier
        mod2data <- current_textile() %>%
          rowwise() %>%
          # AND: all(x %in% y)
          filter(
            all(
              input$textile2mods %in% c_across(all_of(modvec))
            )
          ) %>%
          # Rename
          mutate(textile_name = paste0(
            textile_name, ": ",
            toString(input$textile2mods)
          ))
      } else {
        # FALSE: OR
        # Grab all rows with modifier
        mod2data <- current_textile() %>%
          rowwise() %>%
          # OR: any(x %in% y)
          filter(
            any(
              input$textile2mods %in% c_across(all_of(modvec))
            )
          ) %>%
          # Rename
          mutate(textile_name = paste0(
            textile_name, ": ",
            toString(input$textile2mods)
          ))
      }
    } else {
      # No modifiers
      mod2data <- current_textile() %>%
        mutate(textile_name = paste0(
          textile_name, ": all"
        ))
    }

    # Modifier warnings
    output$mod1Warn <- renderUI({
      if (nrow(mod1data) < 1) {
        tags$label(
          class = "text-danger",
          paste(
            "No",
            input$textileName,
            "with all these modifiers!"
          )
        )
      }
    })

    output$mod2Warn <- renderUI({
      if (nrow(mod2data) < 1) {
        tags$label(
          class = "text-danger",
          paste(
            "No",
            input$textileName,
            "with all these modifiers!"
          )
        )
      }
    })

    # Merge two data frames
    # If both null: copy
    if (is.null(input$textile1mods) & is.null(input$textile2mods)) {
      plot_data <- current_textile()
    } else {
      plot_data <- bind_rows(mod1data, mod2data)
    }


    # Aggregate y by x
    # y: quantity shipped (sum)
    # y: total value (sum)
    # y: price per piece (average)
    plot_data %<>%
      group_by(textile_name, !!sym(input$xAxisChoice)) %>%
      summarise(
        price_per_unit = mean(price_per_unit),
        textile_quantity = sum(textile_quantity),
        total_value = sum(total_value)
      )

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
          "dest_loc_abr" = "Destination",
          "orig_loc_abr" = "Origin"
        ),
        y = switch(input$yAxisChoice,
          "textile_quantity" = "Quantity Shipped",
          "total_value" = "Total Value (Dutch Gulders)",
          "price_per_unit" = paste(
            "Price Per",
            unitvec[[input$textileName]],
            "(Dutch Gulders)"
          )
        ),
        fill = "Textile"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 80)) +
      # Wrap x axis
      scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
      # Title
      ggtitle(paste0(
        input$textileName,
        " (unit: ",
        unitvec[[input$textileName]],
        ")"
      ))

    # Make interactive
    # Fonts don't work in png export
    # https://github.com/plotly/plotly.js/issues/4885
    # https://stackoverflow.com/q/42402584
    mainplotly <- ggplotly(mainggplot) %>%
      layout(font = list(family = 'Lato, -apple-system, system-ui, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif'))

    # Style plotly
    # https://plotly.com/r/configuration-options/#customizing-modebar-download-plot-button
    plotly::config(mainplotly, toImageButtonOptions = list(
      format = "png",
      filename = input$textileName,
      scale = 4
    ))
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
      current_textile_download() %>%
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
