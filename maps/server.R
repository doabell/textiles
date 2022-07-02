# Server function

function(input, output, session) {




  #
  # res_mod <- callModule(
  #   module = selectizeGroupServer,
  #   id = "my-filters",
  #   data = joined.data.original,
  #   vars = c("var_one", "var_two", "var_three", "var_four", "var_five")
  # )
  #






  reactive_data <- reactive({

    # reading in all of the inputs, isolating them
    dataSet <- input$dataSet
    dataType <- input$dataType
    regionChoice <- input$regionChoice
    textileName <- input$textileName
    colors <- input$colors
    patterns <- input$patterns
    process <- input$process
    fibers <- input$fibers
    geography <- input$geography
    qualities <- input$qualities
    # inferredQualities <- isolate(input$inferredQualities)
    area <- input$zoomTo
    input


    data <- joined.data


    private_filter_by <- function(d, col, data_col) {
      if (length(col) != 0 && !is.null(col)) {
        d <- d %>%
          filter(data_col %in% col)
      }
      return(d)
    }


    if (input$dataSet != "Both") {
      data <- private_filter_by(data, input$dataSet, data$company)
    }
    data <- private_filter_by(data, input$textileName, data$textile_name)
    data <- filter_colors(data, input$colors)
    data <- private_filter_by(data, input$patterns, data$textile_pattern_arch)
    data <- private_filter_by(data, input$process, data$textile_process_arch)
    data <- private_filter_by(data, input$fibers, data$textile_fiber_arch)
    data <- private_filter_by(data, input$geography, data$textile_geography_arch)
    data <- private_filter_by(data, input$qualities, data$textile_quality_arch)
    # data <- private_filter_by(data,input$inferredQualities,data$textile_quality_inferred)
    data <- private_filter_by(data, input$year, data[[return_yrColname(input$regionChoice)]])

    # browser()

    return(data)
  })


  output$TextileName <- renderUI({
    selectizeInput(
      inputId = "textileName",
      label = "Choose textile(s) of interest",
      choices = levels(factor(reactive_data()$textile_name)),
      selected = input$textileName,
      multiple = TRUE
    )
  })


  output$Colors <- renderUI({
    pre_unique <- str_split(unique(reactive_data()$colorList), ", ")

    list <- c()
    for (i in 1:length(pre_unique)) {
      list <- append(list, pre_unique[[i]])
    }

    color_choices <- unique(as.vector(list))



    selectizeInput(
      inputId = "colors",
      label = "Choose color(s) of interest",
      choices = color_choices,
      selected = input$colors,
      multiple = TRUE
    )
  })




  output$Pattern <- renderUI({
    patterns <-
      unique(as.vector(reactive_data()$textile_pattern_arch))

    selectizeInput(
      inputId = "Pattern",
      label = "Choose pattern(s) of interest",
      choices = patterns,
      selected = input$patterns,
      multiple = TRUE
    )
  })


  output$Process <- renderUI({
    selectizeInput(
      inputId = "process",
      label = "Choose process(es) of interest",
      choices = levels(factor(reactive_data()$textile_process_arch)),
      selected = input$process,
      multiple = TRUE
    )
  })


  output$Fibers <- renderUI({
    selectizeInput(
      inputId = "fibers",
      label = "Choose fiber(s) of interest",
      choices = levels(factor(reactive_data()$textile_fiber_arch)),
      selected = input$fibers,
      multiple = TRUE
    )
  })

  # output$InferredQualities <- renderUI({

  #   selectizeInput(inputId = "inferredQualities",
  #                  label = "Choose value range(s) of interest",
  #                  choices = levels(factor(reactive_data()$textile_quality_inferred)),
  #                  selected = input$inferredQualities,
  #                  multiple = TRUE)



  # })

  output$Geography <- renderUI({
    selectizeInput(
      inputId = "geography",
      label = "Choose geography of interest",
      choices = levels(factor(reactive_data()$textile_geography_arch)),
      selected = input$geography,
      multiple = TRUE
    )
  })

  output$Qualities <- renderUI({
    user_choices <- levels(factor(joined.data$textile_quality_arch))


    selectizeInput(
      inputId = "qualities",
      label = "Choose quality(s) of interest",
      choices = user_choices,
      selected = input$qualities,
      multiple = TRUE
    )
  })

  output$Year <- renderUI({
    user_choices <- levels(factor(c(reactive_data()$orig_yr, reactive_data()$dest_yr)))

    selectizeInput(
      inputId = "year",
      label = "Year:",
      choices = user_choices,
      selected = input$year,
      multiple = TRUE
    )

    # user_choices in input year
  })


  observe




  # creates table
  output$update_inputs <- renderDataTable(
    searchDelay = 1000,
    # isolate(filter_by_inputs(joined.data.original,isolate(input)))}) #filters the data for what has been searched
    reactive_data()
  )

  # Downloadable .xls of table dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataSet, ".xls", sep = "")
    },
    content = function(file) {
      write_excel_csv(

        # isolate(filter_by_inputs(joined.data.original,isolate(input)))

        reactive_data(), file
      )
    }
  )

  # The map of countries to be rendered
  output$countriesMap <- renderLeaflet({

    # reading in all of the inputs, isolating them
    dataSet <- input$dataSet
    dataType <- input$dataType
    regionChoice <- input$regionChoice
    textileName <- input$textileName
    colors <- input$colors
    patterns <- input$patterns
    process <- input$process
    fibers <- input$fibers
    geography <- input$geography
    qualities <- input$qualities
    # inferredQualities <- input$inferredQualities
    area <- input$zoomTo

    # Every time, we want to start with all of the data to filter through
    # joined.data <- joined.data.original

    # Use the function to filter the inputs
    # joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))

    joined.data <- reactive_data()


    choice <- get_regionChoice(regionChoice)
    totalValues <- filter_totalValue(joined.data, regionChoice, dataSet)

    map.data@data <- left_join(map.data.original@data, # Join with the map data, using the original map data each time
      totalValues,
      by = c("ADMIN" = choice)
    )

    # This will be used to zoom to a specific region on the map
    latLongZoom <- latLongZoom.original %>%
      filter(Area == area)

    viewLat <- latLongZoom[, "Lat"]
    viewLong <- latLongZoom[, "Long"]
    viewZoom <- latLongZoom[, "Magnify"]

    # create the actual map
    create_leaflet_map(map.data, totalValues, dataType, c(viewLat, viewLong, viewZoom))
  })



  # want to integrate ggploty to have interactie charts


  # Used to render the plot for pie chart
  output$pieChart <- renderPlot({
    name <- input$countriesMap_shape_click$id

    # only want to do this if they clicked on a country
    if (length(name) != 0) {
      # Read in all of the inputs, but isolated
      modifier <- input$pieChart
      dataSet <- input$dataSet
      regionChoice <- input$regionChoice
      textileName <- input$textileName
      colors <- input$colors
      patterns <- input$patterns
      process <- input$process
      fibers <- input$fibers
      geography <- input$geography
      qualities <- input$qualities
      # inferredQualities <- input$inferredQualities

      # Again, reusing the original data
      # joined.data <- joined.data.original

      # Filter all the inputs
      # joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))


      joined.data <- reactive_data()

      choice <- get_regionChoice(regionChoice) # get dest or orig

      # We care specifically about the destination here
      pie.data <- joined.data %>%
        filter(joined.data[choice] == name) %>%
        select(
          textile_quantity,
          deb_dec,
          all_of(modifier),
          company
        )


      #   if(regionChoice == "Destination"){ #Only dest_country
      #   pie.data <- joined.data %>%
      #     filter(dest_country == name) %>%
      #     select(textile_quantity,
      #            deb_dec,
      #            all_of(modifier),
      #            company)
      # }
      # else { #Only orig_country
      #   pie.data <- joined.data %>%
      #     filter(orig_country == name) %>%
      #     select(textile_quantity,
      #            deb_dec,
      #            all_of(modifier),
      #            company)
      # }

      # Omit na of the selected columns to avoid errors
      if (input$omitNAs) {
        if (modifier == "colorList") {
          pie.data <- pie.data %>%
            mutate(colorList = ifelse(colorList == "No color indicated", NA, colorList))
        }
        pie.data <- pie.data %>%
          na.omit()
      } else { # Fix a problem for if NA is the only data point
        pie.data[3][is.na(pie.data[3])] <- "None indicated"
      }

      if (dataSet != "Both") { # Controlling for company selection
        pie.data <- pie.data %>%
          filter(company == dataSet)
      }

      if (input$dataType == "Quantity") { # If they're interested in quantity
        if (nrow(pie.data) != 0) { # check to see if there are values left to publish
          pie.data %>%
            ggplot(aes(
              x = "",
              y = textile_quantity
            )) +
            geom_bar(
              stat = "identity",
              width = 1,
              aes_string(fill = modifier)
            ) +
            coord_polar("y", start = 0) + # This line in particular changes the bar chart to a pie chart
            labs(
              x = NULL,
              y = NULL,
              fill = NULL
            ) +
            scale_fill_viridis(
              discrete = TRUE,
              name = paste(names(modVec)[modVec == modifier]),
              option = "magma"
            ) +
            theme_void() +
            ggtitle(label = paste(names(modVec)[modVec == modifier], "distribution for", name, "with these filters."))
        } else { # No rows were found
          ggplot() +
            ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
        }
      } else { # This will do total value the same way, except graphing deb_dec
        if (nrow(pie.data) != 0) {
          pie.data %>%
            ggplot(aes(
              x = "",
              y = deb_dec
            )) +
            geom_bar(
              stat = "identity",
              width = 1,
              aes_string(fill = modifier)
            ) +
            coord_polar("y", start = 0) +
            labs(
              x = NULL,
              y = NULL,
              fill = NULL
            ) +
            scale_fill_viridis(
              discrete = TRUE,
              name = paste(names(modVec)[modVec == modifier]),
              option = "magma"
            ) +
            theme_void() +
            ggtitle(label = paste(names(modVec)[modVec == modifier], "monetary distribution for", name, "with these filters."))
        } else {
          ggplot() +
            ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
        }
      }
    } else { # This comes up if they have not clicked any countries
      ggplot() +
        ggtitle(label = "Select a country with data for these textiles in order to display a pie chart here.")
    }
  })

  # Rendering the bar chart - this works nearly the exact same way as the pie chart
  # except when it is graphing the outputs, it is doing so with a bar chart instead of a pie chart
  output$barChart <- renderPlot({
    name <- input$countriesMap_shape_click$id

    values <- c()


    joined.data <- reactive_data()


    if (!is.null(name) && length(name) != 0) {
      modifier <- input$barChart
      modifierObj <- paste("`", names(modVec)[modVec == modifier], "`", sep = "")
      dataSet <- input$dataSet
      dataType <- input$dataType
      regionChoice <- input$regionChoice
      textileName <- input$textileName
      colors <- input$colors
      patterns <- input$patterns
      process <- input$process
      fibers <- input$fibers
      geography <- input$geography
      qualities <- input$qualities
      # inferredQualities <- input$inferredQualities
      # orig_yr <- input$orig_yr
      year <- input$year
      facet <- input$facet
      # dest_yr <- input$dest_yr



      values <- c(
        "name" = name,
        "modifier" = modifier,
        "modifierObj" = modifierObj,
        "dataSet" = dataSet,
        "dataType" = dataType,
        "regionChoice" = regionChoice,
        "textileName" = textileName,
        "colors" = colors,
        "patterns" = patterns,
        "process" = process,
        "fibers" = fibers,
        "geography" = geography,
        "qualities" = qualities,
        # 'inferredQualities' = inferredQualities,
        #' orig_yr' = orig_yr,
        "year" = year,
        "facet" = facet
      )



      # joined.data <- joined.data.original

      # joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))


      if (regionChoice == "Destination") {
        bar.data <- joined.data %>%
          filter(dest_country == name) %>%
          select(
            textile_quantity,
            deb_dec,
            orig_yr,
            dest_yr,
            all_of(modifier),
            company
          )
      } else {
        bar.data <- joined.data %>%
          filter(orig_country == name) %>%
          select(
            textile_quantity,
            deb_dec,
            orig_yr,
            dest_yr,
            all_of(modifier),
            company
          )
      }

      if (input$omitNAs) {
        if (modifier == "colorList") {
          bar.data <- bar.data %>%
            mutate(colorList = ifelse(colorList == "No color indicated", NA, colorList))
        }

        bar.data <- bar.data %>%
          na.omit()
      } else {
        bar.data[4][is.na(bar.data[4])] <- "None indicated"
      }

      if (dataSet != "Both") {
        bar.data <- bar.data %>%
          filter(company == dataSet)
      }

      # ggplotly
      createBarChart(bar.data, values)
    } else {
      ggplot() +
        ggtitle(label = paste("No data for these filters."))
    }
  })
} # function(input, output, session) {
