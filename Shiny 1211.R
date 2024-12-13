  library(shiny)
  library(terra)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(DT)
  library(leaflet)
  library(forecast)
  
  ui <- navbarPage(
    "Mangrove Change Detection",
    
    # Mangrove Analysis Section
    tabPanel(
      "Mangrove Analysis",
      sidebarLayout(
        sidebarPanel(
          sliderInput("dateRange", "Select Date Range:",
                      min = as.Date("2019-01-01"),
                      max = as.Date("2026-12-31"),
                      value = c(as.Date("2019-01-01"), as.Date("2026-12-31")),
                      timeFormat = "%Y-%m"),
          width = 3
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Trend Analysis", plotOutput("trendPlot")),
            tabPanel("Seasonal Patterns", 
                     fluidRow(
                       column(12, plotOutput("groupedBarPlot")),
                       column(12, plotOutput("stackedBarPlot"))
                     )),
            tabPanel("Data Table", DT::dataTableOutput("dataTable")),
            tabPanel("Yearly Comparison", DT::dataTableOutput("yearlyComparisonTable")),
            tabPanel("Monthly Trends", DT::dataTableOutput("monthlyTrendTable")),
            tabPanel("Same Month Comparison", DT::dataTableOutput("sameMonthTable")), 
            tabPanel("Forecasting", plotOutput("forecastPlot"))
          )
        )
      )
    ),
    
    # TIF Comparison Section
    tabPanel(
      "TIF Comparison",
      sidebarLayout(
        sidebarPanel(
          textInput("tifDirectory", "Enter Directory Path for TIF Files:",
                    value = "E:/Desktop/BU/2024 Fall/MA675/Meta/Data"),
          actionButton("loadFiles", "Load TIF Files"),
          actionButton("prevFile", "Previous File"),
          actionButton("nextFile", "Next File"),
          sliderInput("fileIndex", "Select File Index:", min = 1, max = 1, value = 1, step = 1),
          width = 3
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Interactive Map",
                     textOutput("mapTitle"),
                     p("This map shows the overlay of raster data and the Aruba shapefile."),
                     leafletOutput("rasterMap", height = "700px")),
            tabPanel("Difference Map",
                     p("This map shows only pixels where difference = +/-1 (no difference=0)."),
                     leafletOutput("differenceMap", height = "700px"))
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    # Load Shapefile
    ne_shp <- vect("E:/Desktop/BU/2024 Fall/MA675/Meta/SHP/gadm41_ABW_shp/gadm41_ABW_0.shp")
    
    # Reactive value for TIF files
    tifFiles <- reactiveVal()
    
    # Update map title
    output$mapTitle <- renderText({
      req(tifFiles())
      paste("Interactive Map - File:", basename(tifFiles()[input$fileIndex]))
    })
    
    # Load TIF files
    observeEvent(input$loadFiles, {
      req(input$tifDirectory)
      files <- list.files(input$tifDirectory, pattern = "\\.tif$", full.names = TRUE)
      if (length(files) == 0) {
        showNotification("No TIF files found!", type = "error")
      } else {
        tifFiles(files)
        updateSliderInput(session, "fileIndex", min = 1, max = length(files), value = 1)
        showNotification("TIF files loaded successfully!", type = "message")
      }
    })
    # Handling for Missing Files
    currentRaster <- reactive({
      req(tifFiles(), input$fileIndex <= length(tifFiles()))
      if (length(tifFiles()) == 0 || is.null(tifFiles()[input$fileIndex])) {
        return(NULL)
      }
      raster <- rast(tifFiles()[input$fileIndex])
      if (is.na(crs(raster))) {
        crs(raster) <- "EPSG:4326"
      }
      if (ext(raster)[1] < -180 || ext(raster)[3] < -90) {
        ext(raster) <- ext(-70.0647, -69.865, 12.402, 12.633)  # Example: Aruba bounds
      }
      
      raster
    })

    
    previousRaster <- reactive({
      req(tifFiles(), input$fileIndex > 1)
      raster <- rast(tifFiles()[input$fileIndex - 1])
      if (is.na(crs(raster))) {
        crs(raster) <- "EPSG:4326"
      }
      raster
    })
    
    # Processed raster for interactive map (remove 0)
    processedRaster <- reactive({
      req(currentRaster())
      r <- currentRaster()
      r[r == 0] <- NA
      r
    })
    
    # Difference raster: only plot difference = +/-1
    differenceRaster <- reactive({
      req(currentRaster(), previousRaster())
      diffRaster <- currentRaster() - previousRaster()
      # Set difference=0 to NA, keep only +/-1
      diffRaster[diffRaster == 0] <- NA
      diffRaster
    })
    
    # Reactive values to store map bounds for both maps
    mapBoundsMain <- reactiveVal(NULL)
    mapBoundsDiff <- reactiveVal(NULL)
    
    # Save current map bounds for main map
    observe({
      req(input$rasterMap_bounds)
      mapBoundsMain(input$rasterMap_bounds)
    })
    
    # Save current map bounds for difference map
    observe({
      req(input$differenceMap_bounds)
      mapBoundsDiff(input$differenceMap_bounds)
    })
    
    # Navigate files
    observeEvent(input$prevFile, {
      if (input$fileIndex > 1) {
        updateSliderInput(session, "fileIndex", value = input$fileIndex - 1)
      }
    })
    
    observeEvent(input$nextFile, {
      if (input$fileIndex < length(tifFiles())) {
        updateSliderInput(session, "fileIndex", value = input$fileIndex + 1)
      }
    })
    
    # Render Interactive Map
    output$rasterMap <- renderLeaflet({
      req(processedRaster())
      raster <- processedRaster()
      color_pal <- colorNumeric("green", domain = c(1), na.color = "transparent")
      
      leaflet() %>%
        addProviderTiles("OpenStreetMap") %>%
        addRasterImage(raster, colors = color_pal, opacity = 1, project = TRUE) #%>%
        #addLegend(pal = color_pal, values = c(1), title = "Vegetation") # add legend or not
    })
    
    # Update main map on fileIndex change
    observeEvent(input$fileIndex, {
      req(processedRaster())
      raster <- processedRaster()
      color_pal <- colorNumeric("green", domain = c(1), na.color = "transparent")
      
      proxy <- leafletProxy("rasterMap") %>%
        clearImages() %>%
        addRasterImage(raster, colors = color_pal, opacity = 1, project = TRUE) %>%
        clearControls() %>%
        addLegend(pal = color_pal, values = c(1), title = "Vegetation")
      
      # If bounds are known, restore them
      if (!is.null(mapBoundsMain())) {
        proxy %>% fitBounds(
          lng1 = mapBoundsMain()$west,
          lat1 = mapBoundsMain()$south,
          lng2 = mapBoundsMain()$east,
          lat2 = mapBoundsMain()$north
        )
      }
    })
    
    # Render Difference Map
    output$differenceMap <- renderLeaflet({
      req(differenceRaster())
      diffR <- differenceRaster()
      color_pal <- colorNumeric("RdYlGn", domain = c(-1, 1), na.color = "transparent")
      leaflet() %>%
        addProviderTiles("OpenStreetMap") %>%
        addRasterImage(diffR, colors = color_pal, opacity = 1, project = TRUE) %>%
        addLegend(pal = color_pal, values = c(-1, 1), title = "Change (-1, 1)", opacity = 1)
    })
    
    # Update difference map on fileIndex change
    observeEvent(input$fileIndex, {
      req(differenceRaster())
      diffR <- differenceRaster()
      color_pal <- colorNumeric("RdYlGn", domain = c(-1,1), na.color = "transparent")
      
      proxy <- leafletProxy("differenceMap") %>%
        clearImages() %>%
        addRasterImage(diffR, colors = color_pal, opacity = 1, project = TRUE) %>%
        clearControls() %>%
        addLegend(pal = color_pal, values = c(-1,1), title = "Difference")
      
      if (!is.null(mapBoundsDiff())) {
        proxy %>% fitBounds(
          lng1 = mapBoundsDiff()$west,
          lat1 = mapBoundsDiff()$south,
          lng2 = mapBoundsDiff()$east,
          lat2 = mapBoundsDiff()$north
        )
      }
    })
    
    # Vegetation Data Processing
    vegetation_data <- reactive({
      # Directory containing TIF files
      tif_directory <- input$tifDirectory
      tif_files <- list.files(tif_directory, pattern = "\\.tif$", full.names = TRUE)
      results <- data.frame(FileName = character(), Date = as.Date(character()), VegetationCount = integer())
      for (file in tif_files) {
        raster <- rast(file)
        values <- values(raster)
        vegetation_count <- sum(values == 1, na.rm = TRUE)
        file_name <- basename(file)
        date_str <- sub(".*_(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", file_name)
        date <- as.Date(date_str)
        results <- rbind(results, data.frame(FileName = file_name, Date = date, VegetationCount = vegetation_count))
      }
      results <- results %>% arrange(Date) %>%
        mutate(
          PercentChange = (VegetationCount - lag(VegetationCount)) / lag(VegetationCount) * 100,
          Hectares = VegetationCount * 0.09
        )
      results
    })
    
    # Update sliderInput dynamically
    observe({
      data <- vegetation_data()
      updateSliderInput(session, "dateRange", min = min(data$Date), max = max(data$Date))
    })
    
    # Trend Analysis Plot
    output$trendPlot <- renderPlot({
      data <- vegetation_data() %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
      ggplot(data, aes(x = Date, y = VegetationCount)) +
        geom_line(color = "blue") +
        geom_point() +
        geom_smooth(method = "loess", color = "red", se = FALSE) +
        annotate("text", x = max(data$Date), y = max(data$VegetationCount), 
                 label = paste("Max:", max(data$VegetationCount)), hjust = 1) +
        labs(title = "Mangrove Vegetation Trend", x = "Date", y = "Vegetation Count") +
        theme_minimal()
    })
    
    # # Rate of Change Plot
    # output$ratePlot <- renderPlot({
    #   data <- vegetation_data() %>%
    #     arrange(Date) %>%  # Ensure data is sorted by date
    #     filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
    #     mutate(
    #       PercentChange = (VegetationCount - lag(VegetationCount)) / lag(VegetationCount) * 100, # Calculate rate of change
    #       Month = format(Date, "%b %Y")  # Add a readable month-year format
    #     )
    #   
    #   ggplot(data, aes(x = Month, y = PercentChange, fill = PercentChange > 0)) +
    #     geom_bar(stat = "identity", show.legend = FALSE) +
    #     geom_text(aes(label = ifelse(is.na(PercentChange), "", sprintf("%.1f%%", PercentChange))),
    #               vjust = ifelse(data$PercentChange > 0, -0.5, 1.5),
    #               color = "black", size = 3.5) +
    #     scale_fill_manual(values = c("red", "green")) +
    #     labs(
    #       title = "Monthly Rate of Change in Vegetation",
    #       x = "Month",
    #       y = "Rate of Change (%)"
    #     ) +
    #     theme_minimal() +
    #     theme(
    #       axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    #       text = element_text(size = 14)                    # Increase overall text size
    #     )
    # })
    
    
    # Grouped Bar Plot (Monthly Count by Year)
    output$groupedBarPlot <- renderPlot({
      data <- vegetation_data() %>%
        mutate(Year = year(Date), Month = month(Date, label = TRUE)) %>%
        group_by(Year, Month) %>%
        summarize(TotalCount = sum(VegetationCount, na.rm = TRUE)) %>%
        ungroup()
      
      ggplot(data, aes(x = Month, y = TotalCount, fill = factor(Year))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Monthly Count (Grouped by Year)", x = "Month", y = "Total Vegetation Count", fill = "Year") +
        theme_minimal()
    })
    
    # Stacked Bar Plot (Cumulative Monthly Count Across Years)
    output$stackedBarPlot <- renderPlot({
      data <- vegetation_data() %>%
        mutate(Year = factor(year(Date)), Month = month(Date, label = TRUE)) %>%
        group_by(Month, Year) %>%
        summarize(TotalCount = sum(VegetationCount, na.rm = TRUE)) %>%
        ungroup()
      
      ggplot(data, aes(x = Month, y = TotalCount, fill = Year)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = TotalCount), position = position_stack(vjust = 0.5), size = 4, color = "white") +
        labs(title = "Cumulative Monthly Count (Stacked by Year)", x = "Month", y = "Total Vegetation Count", fill = "Year") +
        theme_minimal()
    })
    
    # Data Table
    output$dataTable <- DT::renderDataTable({
      vegetation_data() %>%
        filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
        mutate(PercentChange = round(PercentChange,2),
               Hectares = round(Hectares, 2) # Rounded to 2 decimal places
        )
    })
    
    # Forecasting Plot
    output$forecastPlot <- renderPlot({
      data <- vegetation_data()
      ts_data <- ts(data$VegetationCount, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12)
      model <- auto.arima(ts_data)
      forecast_data <- forecast(model, h = 12)
      autoplot(forecast_data) +
        labs(title = "Forecasting Future Vegetation", x = "Time", y = "Vegetation Count") +
        theme_minimal()
    })
    
    # Yearly Comparison Table
    output$yearlyComparisonTable <- DT::renderDataTable({
      data <- vegetation_data() %>%
        mutate(Year = year(Date)) %>%
        group_by(Year) %>%
        summarize(
          TotalCount = sum(VegetationCount, na.rm = TRUE)
        ) %>%
        mutate(
          PercentChange = round((TotalCount - lag(TotalCount)) / lag(TotalCount) * 100, 2)
        ) %>%
        ungroup()
      datatable(data, options = list(pageLength = 5), rownames = FALSE)
    })
    
    output$monthlyTrendTable <- DT::renderDataTable({
      data <- vegetation_data() %>%
        mutate(Month = month(Date, label = TRUE)) %>%
        group_by(Month) %>%
        summarize(
          AverageCount = round(mean(VegetationCount, na.rm = TRUE), 1) # Rounded to 1 decimal place
        ) %>%
        ungroup()
      
      datatable(data, options = list(pageLength = 12), rownames = FALSE)
    })
    
    output$sameMonthTable <- DT::renderDataTable({
      data <- vegetation_data() %>%
        mutate(Year = year(Date), Month = month(Date, label = TRUE)) %>%
        group_by(Month, Year) %>%
        summarize(
          TotalCount = sum(VegetationCount, na.rm = TRUE)
        ) %>%
        arrange(Month, Year) %>%
        group_by(Month) %>%
        mutate(
          PercentChange = round((TotalCount - lag(TotalCount)) / lag(TotalCount) * 100, 2)
        ) %>%
        ungroup()
      
      datatable(data, options = list(pageLength = 12), rownames = FALSE)
    })
  }
  
  # Run the App
  shinyApp(ui = ui, server = server)
