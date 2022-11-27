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

  # Refresh page
  observeEvent(input$refresh, {
    session$reload()
  })

  # If the user entered a textile, or any modifiers
  user_input_len <- reactive({
    c(input$textileName, input$colors, input$patterns, input$process, input$fibers, input$geography, input$qualities, input$year) %>%
    length()
  })

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
    # data <- filter_colors(data, input$colors)
    data <- private_filter_by(data, input$colors, data$textile_color_arch)
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
    tagList(
      tags$p(tags$b("Region 1: "), c1)
    )
  })

  output$TextileName <- renderUI({
    selectizeInput(
      inputId = "textileName",
      label = "Choose textile(s) of interest",
      choices = levels(factor(joined.data$textile_name)),
      selected = input$textileName,
      multiple = TRUE
    )
  })


  output$Colors <- renderUI({
    pre_unique <- str_split(unique(joined.data$textile_color_arch), ", ")

    list <- c()
    for (i in 1:length(pre_unique)) {
      list <- append(list, pre_unique[[i]])
    }

    color_choices <- unique(as.vector(list)) %>%
      na.omit()


    if (length(color_choices) != 0) {
      selectizeInput(
        inputId = "colors",
        label = "Choose color(s) of interest",
        choices = color_choices,
        selected = input$colors,
        multiple = TRUE
      )
    }
  })




  output$Pattern <- renderUI({
    patterns <-
      unique(as.vector(joined.data$textile_pattern_arch)) %>%
      na.omit()

    if (length(patterns) != 0) {
      selectizeInput(
        inputId = "Pattern",
        label = "Choose pattern(s) of interest",
        choices = patterns,
        selected = input$patterns,
        multiple = TRUE
      )
    }
  })


  output$Process <- renderUI({
    processes <- levels(factor(joined.data$textile_process_arch)) %>%
      na.omit()
    if (length(processes) != 0) {
      selectizeInput(
        inputId = "process",
        label = "Choose process(es) of interest",
        choices = processes,
        selected = input$process,
        multiple = TRUE
      )
    }
  })


  output$Fibers <- renderUI({
    fiberchoice <- levels(factor(joined.data$textile_fiber_arch)) %>%
      na.omit()
    if (length(fiberchoice) != 0) {
      selectizeInput(
        inputId = "fibers",
        label = "Choose fiber(s) of interest",
        choices = fiberchoice,
        selected = input$fibers,
        multiple = TRUE
      )
    }
  })

  # output$InferredQualities <- renderUI({

  #   selectizeInput(inputId = "inferredQualities",
  #                  label = "Choose value range(s) of interest",
  #                  choices = levels(factor(reactive_data()$textile_quality_inferred)),
  #                  selected = input$inferredQualities,
  #                  multiple = TRUE)



  # })

  output$Geography <- renderUI({
    geos <- levels(factor(joined.data$textile_geography_arch)) %>%
      na.omit()
    
    if (length(geos) != 0) {
      selectizeInput(
        inputId = "geography",
        label = "Choose geography of interest",
        choices = geos,
        selected = input$geography,
        multiple = TRUE
      )
    }
  })

  output$Qualities <- renderUI({
    qual <- levels(factor(joined.data$textile_quality_arch)) %>%
      na.omit()

    if (length(qual) != 0) {
      selectizeInput(
        inputId = "qualities",
        label = "Choose quality(s) of interest",
        choices = qual,
        selected = input$qualities,
        multiple = TRUE
      )
    }
  })

  output$Year <- renderUI({
    user_choices <- unique(joined.data$orig_yr) %>%
      union(unique(joined.data$dest_yr)) %>%
      sort() %>%
      na.omit() %>%
      setdiff("NA") # Somehow has the string "NA" in it

    # should always have a year
    # if (length(user_choices) != 0) {
    selectizeInput(
      inputId = "year",
      label = "Year:",
      choices = user_choices,
      selected = input$year,
      multiple = TRUE
    )
    # }

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
      paste(input$dataSet, ".csv", sep = "")
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

  # want to integrate ggploty to have interactive charts
  # need to figure out how to render fonts properly in plotly


  # Used to render the plot for pie chart
  output$pieChart <- renderPlot({
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
    name <- input$regionMap_shape_click$id

    # Get a copy of the data
    pie.data <- reactive_data() %>%
      select(
          textile_quantity,
          total_value,
          all_of(modifier),
          company,
          dest_loc_region,
          orig_loc_region_modern
      )

    # Get country selected and filter, if they clicked on a country
    if (length(name) != 0) {

      choice <- get_regionChoice(regionChoice) # get dest or orig

      # We care specifically about the destination here
      pie.data <- pie.data %>%
        filter(pie.data[choice] == name)
    } else if (user_input_len() == 0) {
      # User did nothing
      return(
        ggplot() +
          theme(text = element_text(family = "Lato", size = 15)) +
          ggtitle(label = paste("Select a region, textile or modifier to display a pie chart here."))
      )
    } else {
      # User chose modifier(s) but not a country
      # Give name for plot
      name <- "Worldwide"
    }


    if (dataSet != "Both") { # Controlling for company selection
      pie.data <- pie.data %>%
        filter(company == dataSet)
    }
    
    # remove na of the selected columns to avoid errors
    if (modifier == "textile_color_arch") {
      pie.data <- pie.data %>%
        mutate(textile_color_arch = ifelse(textile_color_arch == "No color indicated", NA, textile_color_arch))
    }
    pie.data <- pie.data %>%
      na.omit()

    if (input$dataType == "Quantity") { # If they're interested in quantity
      if (nrow(pie.data) != 0) { # check to see if there are values left to publish
        pie.data %>%
          # Aggregate by name
          # https://stackoverflow.com/questions/64055988/
          group_by(across(all_of(modifier))) %>%
          summarise(textile_quantity = sum(textile_quantity)) %>%
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
          ggtitle(label = paste(names(modVec)[modVec == modifier], "distribution for", name, "with these filters.")) +
          theme(text = element_text(family = "Lato", size = 15),
                legend.position = "bottom",
                plot.title = element_text(hjust = 0.5)) +
          guides(fill = guide_legend(
            title.position = "top",
            ncol = 7
          ))
      } else { # No rows were found
        ggplot() +
          theme(text = element_text(family = "Lato", size = 15),
                plot.title = element_text(hjust = 0.5)) +
          ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
      }
    } else { # This will do total value the same way, except graphing total_value
      if (nrow(pie.data) != 0) {
        pie.data %>%
          # Aggregate by name
          group_by(across(all_of(modifier))) %>%
          summarise(total_value = sum(total_value)) %>%
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
          ggtitle(label = paste(names(modVec)[modVec == modifier], "monetary distribution for", name, "with these filters.")) +
          theme(text = element_text(family = "Lato", size = 15),
                legend.position = "bottom",
                plot.title = element_text(hjust = 0.5)) +
          guides(fill = guide_legend(
            title.position = "top",
            ncol = 7
          ))
      } else {
        ggplot() +
          theme(text = element_text(family = "Lato", size = 15),
                plot.title = element_text(hjust = 0.5)) +
          ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
      }
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
        ggtitle(label = paste("Select a region with data for these textiles in order to display a bar chart here."))
    }
  })

} # function(input, output, session) {
