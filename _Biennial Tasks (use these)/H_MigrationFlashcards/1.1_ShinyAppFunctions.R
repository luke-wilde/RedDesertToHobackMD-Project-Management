runApp <- function(){
  
  # Function to create plots for each bioyear
  create_plots <- function(bioyr_data, bioyr) {
    
    year <- year(min(bioyr_data$date))
    
    
    start_date <- as.Date(paste(year, "-01-01", sep = ""))
    end_date <- as.Date(paste(year + 1, "-06-30", sep = ""))
    
    NSD <- ggplot(bioyr_data) + 
      geom_line(aes(x = as.Date(date), y = -1*R2n), linewidth = 0.9) + 
      labs(y = "Net-square Displacement") + 
      scale_x_date(limits = c(start_date, end_date), date_breaks = "months", date_labels = "%m") + 
      theme_classic() + 
      theme(legend.position = "none")
    
    elev <- ggplot(bioyr_data) + 
      geom_line(aes(x = as.Date(date), y = elev)) + 
      scale_x_date(limits = c(start_date, end_date), date_breaks = "months", date_labels = "%m") + 
      labs(y = "Elevation (m)") + 
      theme_classic() + 
      theme(legend.position = "none")
    
    speed <- ggplot(bioyr_data) + 
      geom_line(aes(x = as.Date(date), y = dist/dt)) + 
      scale_x_date(limits = c(start_date, end_date), date_breaks = "months", date_labels = "%m") + 
      labs(y = "Speed (m/s)") + 
      theme_classic() + 
      theme(legend.position = "none")
    
    lat <- ggplot(bioyr_data) + 
      geom_line(aes(x = as.Date(date), y = st_coordinates(bioyr_data)[, 2]), linewidth = 0.9) + 
      scale_x_date(limits = c(start_date, end_date), date_breaks = "months", date_labels = "%m") + 
      labs(y = "Latitude (UTM_N)") + 
      theme_classic() + 
      theme(legend.position = "none")
    
    long <- ggplot(bioyr_data) + 
      geom_line(aes(x = as.Date(date), y = st_coordinates(bioyr_data)[, 1]), linewidth = 0.9) + 
      scale_x_date(limits = c(start_date, end_date), date_breaks = "months", date_labels = "%m") +  
      labs(y = "Longitude (UTM_E)", x = "Month") + 
      theme_classic() + 
      theme(legend.position = "none")
    
    # Combine plots
    combined_plot <- subplot(
      ggplotly(NSD + ggtitle(bioyr), height = 2000),
      ggplotly(lat),
      #ggplotly(elev),
      #ggplotly(long),
      nrows = 2, shareX = F, titleY = TRUE
    )
    
    return(combined_plot)
  }
  
  # Create Shiny app
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
      .subtitle {
        font-size: 1.5em;  /* Adjust the size as needed */
      }
    "))
    ),
    titlePanel("RDH Migration Records"),
    div(class = "subtitle", "Bio-year start: July 1, or Mar for first record-year. All deer need to survive to June 15 to be non-NA"),  # Add subtitle here
  
    sidebarLayout(
      sidebarPanel(
        selectInput("bioyr", "Select Bioyear:", choices = NULL),
        radioButtons("strat", "Migration Strategy:", choices = c("Long", "Med", "Short", "Resident", "NA")),
        #textInput("strat", "Migration Strategy (long, med, short, res, NA):"),
        # New slider for month selection
        sliderInput("month_range", "Select Months to Map:", 
                    min = 1, max = 12, 
                    value = c(1, 12),
                    step = 1),
        radioButtons("complete_fall", "Complete Fall:", choices = c("Yes", "No")),
        dateInput("fall_start", "Fall Start Date:", value = NULL),
        dateInput("fall_end", "Fall End Date:", value = NULL),
        radioButtons("fall_falsestart", "Fall Falsestart:", choices = c("Yes", "No")),
        dateInput("fall_falsestart_start", "Fall Falsestart Start Date:", value = NULL),
        dateInput("fall_falsestart_end", "Fall Falsestart End Date:", value = NULL),
        
        textInput("fall_notes", "Fall Notes:"),
        radioButtons("complete_spring", "Complete Spring:", choices = c("Yes", "No")),
        dateInput("spring_start", "Spring Start Date:", value = NULL),
        dateInput("spring_end", "Spring End Date:", value = NULL),
        radioButtons("spring_falsestart", "Spring Falsestart:", choices = c("Yes", "No")),
        dateInput("spring_falsestart_start", "Spring Falsestart Start Date:", value = NULL),
        dateInput("spring_falsestart_end", "Spring Falsestart End Date:", value = NULL),
        textInput("spring_notes", "Spring Notes:"),
        radioButtons("summer_walkabout", "Summer Walkabout:", choices = c("Yes", "No")),
        dateInput("summer_walkabout_start", "Summer Walkabout Start Date:", value = NULL),
        dateInput("summer_walkabout_end", "Summer Walkabout End Date:", value = NULL),
        
        actionButton("submit", "Output Results")
      ),
      
      mainPanel(
        leafletOutput("map", height = "400px"),  # Add map output,
        plotlyOutput("plot", height = "2400px")  # Double the height
        
      )
      
      # mainPanel(
      #   plotlyOutput("plot", height = "2000px")  # Double the height
      # )
    )
  )
  
  server <- function(input, output, session) {
    bioyears <- choice
    
    updateSelectInput(session, "bioyr", choices = bioyears)
    
    observeEvent(input$bioyr, {
      req(input$bioyr)
      bioyr_data <- RDH_nsd %>% filter(burst == input$bioyr)
      print(paste("Selected Bioyear:", input$bioyr))  # Debugging statement
      print(head(bioyr_data))  # Debugging statement to check the filtered data
      year <- year(min(bioyr_data$date))
      
      updateDateInput(session, "spring_start", value = as.Date(paste(year+1, "-03-01", sep = "")))
      updateDateInput(session, "spring_end", value = as.Date(paste(year+1, "-05-31", sep = "")))
      updateDateInput(session, "spring_falsestart_start", value = as.Date(paste(year+1, "-02-01", sep = "")))
      updateDateInput(session, "spring_falsestart_end", value = as.Date(paste(year+1, "-02-28", sep = "")))
      updateDateInput(session, "summer_walkabout_start", value = as.Date(paste(year, "-06-01", sep = "")))
      updateDateInput(session, "summer_walkabout_end", value = as.Date(paste(year, "-08-31", sep = "")))
      updateDateInput(session, "fall_start", value = as.Date(paste(year, "-09-01", sep = "")))
      updateDateInput(session, "fall_end", value = as.Date(paste(year, "-11-30", sep = "")))
      updateDateInput(session, "fall_falsestart_start", value = as.Date(paste(year, "-08-01", sep = "")))
      updateDateInput(session, "fall_falsestart_end", value = as.Date(paste(year, "-08-31", sep = "")))
      
      updateRadioButtons(session, "spring_falsestart", selected = "Yes")
      updateRadioButtons(session, "summer_walkabout", selected = "Yes")
      updateRadioButtons(session, "fall_falsestart", selected = "Yes")
      updateRadioButtons(session, "complete_spring", selected = "Yes")
      updateRadioButtons(session, "complete_fall", selected = "Yes")
      
      # Reset text inputs
      updateTextInput(session, "strat", value = "")
      updateTextInput(session, "spring_notes", value = "")
      updateTextInput(session, "fall_notes", value = "")
      
    })
    
    observe({
      
      #No
      if (input$complete_spring == "No") {
        updateDateInput(session, "spring_start", value = as.Date(paste("1899-01-01", sep = "")))
        updateDateInput(session, "spring_end", value = as.Date(paste("1899-01-01", sep = "")))
      }  
      
      
      if (input$spring_falsestart == "No") {
        updateDateInput(session, "spring_falsestart_start", value = as.Date(paste("1899-01-01", sep = "")))
        updateDateInput(session, "spring_falsestart_end", value = as.Date(paste("1899-01-01", sep = "")))
      } 
      
      
      if (input$summer_walkabout == "No") {
        updateDateInput(session, "summer_walkabout_start", value = as.Date(paste("1899-01-01", sep = "")))
        updateDateInput(session, "summer_walkabout_end", value = as.Date(paste("1899-01-01", sep = "")))
      } 
      
      if (input$complete_fall == "No") {
        updateDateInput(session, "fall_start", value = as.Date(paste("1899-01-01", sep = "")))
        updateDateInput(session, "fall_end", value = as.Date(paste("1899-01-01", sep = "")))
      } 
      
      
      if (input$fall_falsestart == "No") {
        updateDateInput(session, "fall_falsestart_start", value = as.Date(paste("1899-01-01", sep = "")))
        updateDateInput(session, "fall_falsestart_end", value = as.Date(paste("1899-01-01", sep = "")))
      }  
      
    
    })
    
    output$plot <- renderPlotly({
      req(input$bioyr)
      bioyr_data <- RDH_nsd %>% filter(burst == input$bioyr)
      create_plots(bioyr_data, input$bioyr)
    })
    
    output$map <- renderLeaflet({
      req(input$bioyr)
      bioyr_data <- RDH_nsd %>% filter(burst == input$bioyr)
      bioyr_data$month <- as.numeric(format(as.Date(bioyr_data$date), "%m"))  # Convert months to numeric
      
      # Filter data based on selected month range
      filtered_data <- bioyr_data[bioyr_data$month >= input$month_range[1] & 
                                    bioyr_data$month <= input$month_range[2], ]
      
      # Define a color palette
      pal <- colorNumeric(palette = "viridis", domain = filtered_data$month)
      
      leaflet(filtered_data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~st_coordinates(filtered_data)[, 1],
          lat = ~st_coordinates(filtered_data)[, 2],
          color = ~pal(month),
          radius = 3,  # Adjust the radius to make the points smaller
          popup = ~paste("Date:", date, "<br>", "Month:", month)
        ) %>%
        addLegend("bottomright", pal = pal, values = ~month, title = "Month") %>%
        addScaleBar(position = "bottomleft")  # Add a scale bar
    })
    
    observeEvent(input$submit, {
      result <- data.frame(
        BioYear = input$bioyr,
        SpringStartDate = input$spring_start,
        SpringEndDate = input$spring_end,
        SpringFalsestart = input$spring_falsestart,
        SpringFalsestartStartDate = input$spring_falsestart_start,
        SpringFalsestartEndDate = input$spring_falsestart_end,
        SpringNotes = input$spring_notes,
        CompleteSpring = input$complete_spring,
        SummerWalkabout = input$summer_walkabout,
        SummerWalkaboutStartDate = input$summer_walkabout_start,
        SummerWalkaboutEndDate = input$summer_walkabout_end,
        FallStartDate = input$fall_start,
        FallEndDate = input$fall_end,
        FallFalsestart = input$fall_falsestart,
        FallFalsestartStartDate = input$fall_falsestart_start,
        FallFalsestartEndDate = input$fall_falsestart_end,
        FallNotes = input$fall_notes,
        CompleteFall = input$complete_fall,
        MigrationStrategy = input$strat
      )
      
      # Append to existing R object
      if (exists("results_df")) {
        results_df <<- rbind(results_df, result)
      } else {
        results_df <<- result
      }
      
      # write.csv(results_df, paste0("RDH_MigrationMapper_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE)
      
      # Automatically bring up the next bioyr in line
      current_index <- which(bioyears == input$bioyr)
      next_index <- ifelse(current_index < length(bioyears), current_index + 1, length(bioyears))
      updateSelectInput(session, "bioyr", selected = bioyears[next_index])
      
      # Reset Yes/No buttons
      updateRadioButtons(session, "complete_spring", selected = "Yes")
      updateRadioButtons(session, "spring_falsestart", selected = "Yes")
      updateRadioButtons(session, "summer_walkabout", selected = "Yes")
      updateRadioButtons(session, "complete_fall", selected = "Yes")
      updateRadioButtons(session, "fall_falsestart", selected = "Yes")
      updateRadioButtons(session, "strat", selected = "NA")
      
      # Reset month range slider to full range
      updateSliderInput(session, "month_range", value = c(1, 12))
    })
  }
  
  
  
  
  shinyApp(ui, server)
}