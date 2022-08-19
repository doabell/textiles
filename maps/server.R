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
  
  output$twoRegions <- renderUI({
    c1 <- ifelse(
      length(input$regionMap_shape_click$id) != 0,
      input$regionMap_shape_click$id,
      "Select on map"
       )
    c2 <- ifelse(
      length(input$regionMap2_shape_click$id) != 0,
      input$regionMap2_shape_click$id,
      "Select on map"
    )
    tagList(
    tags$p(tags$b("Region 1: "), c1),
    tags$p(tags$b("Region 2: "), c2))
    
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
    pre_unique <- str_split(unique(reactive_data()$textile_color_arch), ", ")

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
      write_csv(

        # isolate(filter_by_inputs(joined.data.original,isolate(input)))

        reactive_data(), file
      )
    }
  )

  # The map of countries to be rendered
  output$regionMap <- renderLeaflet({

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

    # Every time, we want to start with all of the data to filter through
    # joined.data <- joined.data.original

    # Use the function to filter the inputs
    # joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))

    joined.data <- reactive_data()


    choice <- get_regionChoice(regionChoice)
    totalValues <- filter_totalValue(joined.data, regionChoice, dataSet)

    map.data@data <- left_join(map.data@data, # Join with the map data, using the original map data each time
      totalValues,
      by = c("region" = choice)
    )


    # create the actual map
    create_leaflet_map(map.data, totalValues, dataType, c(30, 53, 2))
  })

  output$regionMap2 <- renderLeaflet({

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

    # Every time, we want to start with all of the data to filter through
    # joined.data <- joined.data.original

    # Use the function to filter the inputs
    # joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))

    joined.data <- reactive_data()


    choice <- get_regionChoice(regionChoice)
    totalValues <- filter_totalValue(joined.data, regionChoice, dataSet)

    map.data@data <- left_join(map.data@data, # Join with the map data, using the original map data each time
      totalValues,
      by = c("region" = choice)
    )


    # create the actual map
    create_leaflet_map(map.data, totalValues, dataType, c(30, 53, 2))
  })



  # want to integrate ggploty to have interactie charts


  # Used to render the plot for pie chart
  output$pieChart <- renderPlot({
    name <- input$regionMap_shape_click$id

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
          total_value,
          all_of(modifier),
          company
        )


      #   if(regionChoice == "Destination"){ #Only dest_loc_region
      #   pie.data <- joined.data %>%
      #     filter(dest_loc_region == name) %>%
      #     select(textile_quantity,
      #            total_value,
      #            all_of(modifier),
      #            company)
      # }
      # else { #Only orig_loc_region_modern
      #   pie.data <- joined.data %>%
      #     filter(orig_loc_region_modern == name) %>%
      #     select(textile_quantity,
      #            total_value,
      #            all_of(modifier),
      #            company)
      # }

      # remove na of the selected columns to avoid errors
      if (modifier == "textile_color_arch") {
        pie.data <- pie.data %>%
          mutate(textile_color_arch = ifelse(textile_color_arch == "No color indicated", NA, textile_color_arch))
      }
      pie.data <- pie.data %>%
        na.omit()


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
            theme(text = element_text(family = "Lato", size = 15)) +
            ggtitle(label = paste(names(modVec)[modVec == modifier], "distribution for", name, "with these filters."))
        } else { # No rows were found
          ggplot() +
            theme(text = element_text(family = "Lato", size = 15)) +
            ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
        }
      } else { # This will do total value the same way, except graphing total_value
        if (nrow(pie.data) != 0) {
          pie.data %>%
            ggplot(aes(
              x = "",
              y = total_value
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
            theme(text = element_text(family = "Lato", size = 15)) +
            ggtitle(label = paste(names(modVec)[modVec == modifier], "monetary distribution for", name, "with these filters."))
        } else {
          ggplot() +
            theme(text = element_text(family = "Lato", size = 15)) +
            ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
        }
      }
    } else { # This comes up if they have not clicked any countries
      ggplot() +
        theme(text = element_text(family = "Lato", size = 15)) +
        ggtitle(label = "Select a region with data for these textiles in order to display a pie chart here.")
    }
  })

  output$pieChart2 <- renderPlot({
    name <- input$regionMap2_shape_click$id

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
          total_value,
          all_of(modifier),
          company
        )


      #   if(regionChoice == "Destination"){ #Only dest_loc_region
      #   pie.data <- joined.data %>%
      #     filter(dest_loc_region == name) %>%
      #     select(textile_quantity,
      #            total_value,
      #            all_of(modifier),
      #            company)
      # }
      # else { #Only orig_loc_region_modern
      #   pie.data <- joined.data %>%
      #     filter(orig_loc_region_modern == name) %>%
      #     select(textile_quantity,
      #            total_value,
      #            all_of(modifier),
      #            company)
      # }

      # remove na of the selected columns to avoid errors
      if (modifier == "textile_color_arch") {
        pie.data <- pie.data %>%
          mutate(textile_color_arch = ifelse(textile_color_arch == "No color indicated", NA, textile_color_arch))
      }
      pie.data <- pie.data %>%
        na.omit()


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
            theme(text = element_text(family = "Lato", size = 15)) +
            ggtitle(label = paste(names(modVec)[modVec == modifier], "distribution for", name, "with these filters."))
        } else { # No rows were found
          ggplot() +
            theme(text = element_text(family = "Lato", size = 15)) +
            ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
        }
      } else { # This will do total value the same way, except graphing total_value
        if (nrow(pie.data) != 0) {
          pie.data %>%
            ggplot(aes(
              x = "",
              y = total_value
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
            theme(text = element_text(family = "Lato", size = 15)) +
            ggtitle(label = paste(names(modVec)[modVec == modifier], "monetary distribution for", name, "with these filters."))
        } else {
          ggplot() +
            theme(text = element_text(family = "Lato", size = 15)) +
            ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
        }
      }
    } else { # This comes up if they have not clicked any countries
      ggplot() +
        theme(text = element_text(family = "Lato", size = 15)) +
        ggtitle(label = "Select a region with data for these textiles in order to display a pie chart here.")
    }
  })

  # Rendering the bar chart - this works nearly the exact same way as the pie chart
  # except when it is graphing the outputs, it is doing so with a bar chart instead of a pie chart
  output$barChart <- renderPlot({
    name <<- input$regionMap_shape_click$id

    values <- c()


    joined.data <- reactive_data()


    if (!is.null(name) && length(name) != 0) {
      modifier <- input$barChart
      modifierObj <- names(modVec)[modVec == modifier]
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
        "year" = year
      )



      # joined.data <- joined.data.original

      # joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))


      if (regionChoice == "Destination") {
        bar.data <- joined.data %>%
          filter(dest_loc_region == name) %>%
          select(
            dest_loc_region,
            orig_loc_region_modern,
            textile_quantity,
            total_value,
            orig_yr,
            dest_yr,
            all_of(modifier),
            company
          )
      } else {
        bar.data <- joined.data %>%
          filter(orig_loc_region_modern == name) %>%
          select(
            dest_loc_region,
            orig_loc_region_modern,
            textile_quantity,
            total_value,
            orig_yr,
            dest_yr,
            all_of(modifier),
            company
          )
      }


      if (modifier == "textile_color_arch") {
        bar.data <- bar.data %>%
          mutate(textile_color_arch = ifelse(textile_color_arch == "No color indicated", NA, textile_color_arch))
      }

      bar.data <- bar.data %>%
        na.omit()


      if (dataSet != "Both") {
        bar.data <- bar.data %>%
          filter(company == dataSet)
      }

      # ggplotly
      createBarChart(bar.data, values)
    } else {
      ggplot() +
      theme(text = element_text(family = "Lato", size = 15)) +
        ggtitle(label = paste("No data for these filters."))
    }
  })


  # Rendering the 2nd bar chart
  output$barChart2 <- renderPlot({
    name2 <<- input$regionMap2_shape_click$id

    values <- c()


    joined.data <- reactive_data()


    if (!is.null(name2) && length(name2) != 0) {
      modifier <- input$barChart
      modifierObj <- names(modVec)[modVec == modifier]
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
      # dest_yr <- input$dest_yr



      values <- c(
        "name" = name2,
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
        "year" = year
      )



      # joined.data <- joined.data.original

      # joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))


      if (regionChoice == "Destination") {
          bar.data2 <- joined.data %>%
            filter(dest_loc_region == name2) %>%
            select(
              orig_loc_region_modern,
              dest_loc_region,
              textile_quantity,
              total_value,
              orig_yr,
              dest_yr,
              all_of(modifier),
              company
            )
        } else {
          bar.data2 <- joined.data %>%
            filter(orig_loc_region_modern == name2) %>%
            select(
              orig_loc_region_modern,
              dest_loc_region,
              textile_quantity,
              total_value,
              orig_yr,
              dest_yr,
              all_of(modifier),
              company
            )
        }


      if (modifier == "textile_color_arch") {
        bar.data2 <- bar.data2 %>%
          mutate(textile_color_arch = ifelse(textile_color_arch == "No color indicated", NA, textile_color_arch))
      }

      bar.data2 <- bar.data2 %>%
        na.omit()


      if (dataSet != "Both") {
        bar.data2 <- bar.data2 %>%
          filter(company == dataSet)
      }

      # ggplotly
      createBarChart(bar.data2, values)
    } else {
      ggplot() +
      theme(text = element_text(family = "Lato", size = 15)) +
        ggtitle(label = paste("No data for these filters."))
    }
  })


  # Bar chart on comparision pane
  output$barChartCompare <- renderPlot({
    name <<- input$regionMap_shape_click$id
    name2 <<- input$regionMap2_shape_click$id

    values <- c()


    joined.data <- reactive_data()


    if (!is.null(name) && length(name) != 0) {
      modifier <- input$barChart
      modifierObj <- names(modVec)[modVec == modifier]
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
        "year" = year
      )



      # joined.data <- joined.data.original

      # joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))

      
      
      if (regionChoice == "Destination") {
        bar.data <- joined.data %>%
          filter(dest_loc_region == name) %>%
          select(
            dest_loc_region,
            orig_loc_region_modern,
            textile_quantity,
            total_value,
            orig_yr,
            dest_yr,
            all_of(modifier),
            company
          )
      } else {
        bar.data <- joined.data %>%
          filter(orig_loc_region_modern == name) %>%
          select(
            dest_loc_region,
            orig_loc_region_modern,
            textile_quantity,
            total_value,
            orig_yr,
            dest_yr,
            all_of(modifier),
            company
          )
      }
      
      if (modifier == "textile_color_arch") {
        bar.data <- bar.data %>%
          mutate(textile_color_arch = ifelse(textile_color_arch == "No color indicated", NA, textile_color_arch))
      }
      
      bar.data <- bar.data %>%
        na.omit()
      
      
      if (dataSet != "Both") {
        bar.data <- bar.data %>%
          filter(company == dataSet)
      }

      # if 2nd country
      if (!is.null(name2) && length(name2) != 0) {
        if (regionChoice == "Destination") {
          values["modifierObj"] <- "dest_loc_region"
          values["modifier"] <- "dest_loc_region"
          bar.data2 <- joined.data %>%
            filter(dest_loc_region == name2) %>%
            select(
              orig_loc_region_modern,
              dest_loc_region,
              textile_quantity,
              total_value,
              orig_yr,
              dest_yr,
              all_of(modifier),
              company
            )
        } else {
          values["modifierObj"] <- "orig_loc_region_modern"
          values["modifier"] <- "orig_loc_region_modern"
          bar.data2 <- joined.data %>%
            filter(orig_loc_region_modern == name2) %>%
            select(
              orig_loc_region_modern,
              dest_loc_region,
              textile_quantity,
              total_value,
              orig_yr,
              dest_yr,
              all_of(modifier),
              company
            )
        }
        
        if (modifier == "textile_color_arch") {
          bar.data2 <- bar.data2 %>%
            mutate(textile_color_arch = ifelse(textile_color_arch == "No color indicated", NA, textile_color_arch))
        }
        
        bar.data2 <- bar.data2 %>%
          na.omit()
        
        
        if (dataSet != "Both") {
          bar.data2 <- bar.data2 %>%
            filter(company == dataSet)
        }
        
        if (regionChoice == "Origin") {
          bind.data <<- bar.data %>%
            bind_rows(bar.data2) %>%
            group_by(orig_loc_region_modern, orig_yr) %>%
            summarise(
              total_value = sum(total_value),
              textile_quantity = sum(textile_quantity)
            )
        } else {
          bind.data <<- bar.data %>%
            bind_rows(bar.data2) %>%
            group_by(dest_loc_region, dest_yr) %>%
            summarise(
              total_value = sum(total_value),
              textile_quantity = sum(textile_quantity)
            )
        }
        createBarChartCompare(bind.data, values, name2)
      } else {
        # no 2nd country
        # ggplotly
        createBarChartCompare(bar.data, values)
      }
      
    } else {
      ggplot() +
        theme(text = element_text(family = "Lato", size = 15)) +
        ggtitle(label = paste("Select regions with data for these textiles in order to display a bar chart here"))
    }
  })
} # function(input, output, session) {
