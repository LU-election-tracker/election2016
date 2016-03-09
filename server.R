# Election tracker's Shiny server. 
#
# Written by Henry Ward and Adam Loy, 2015.
# Licensed under the MIT license (tldrlegal.com/license/mit-license)


######
### VARIABLES, UTILITY FUNCTIONS AND PREP
######


# Gets parent folder
main_folder <- "./"
data_folder <- file.path(main_folder, "data")

# Opens and formats RCP poll data for democrats
dem_rcp <- read.csv(file.path(data_folder, "rcp_dem_full.tsv"), sep = "\t")
rcp_row <- which(apply(dem_rcp, 1, function(x) any(grepl("RCP Average", x))))
dem_rcp <- dem_rcp[-c(rcp_row), ]
dem_rcp <- format_polls(dem_rcp, dem_candidates)

# Opens and formats RCP poll data for republicans
gop_rcp <- read.csv(file.path(data_folder, "rcp_gop_full.tsv"), sep = "\t")
rcp_row <- which(apply(gop_rcp, 1, function(x) any(grepl("RCP Average", x))))
gop_rcp <- gop_rcp[-c(rcp_row), ]
gop_rcp <- format_polls(gop_rcp, gop_candidates)

# Opens and formats Pollster poll data
dem_pollster <- read.csv(file.path(data_folder, "pollster_dem.tsv"), sep = "\t")
gop_pollster <- read.csv(file.path(data_folder, "pollster_gop.tsv"), sep = "\t")
colnames(dem_pollster)[1] <- "Poll"
colnames(gop_pollster)[1] <- "Poll"
dem_pollster <- format_polls(dem_pollster, dem_candidates, format_dates = FALSE)
gop_pollster <- format_polls(gop_pollster, gop_candidates, format_dates = FALSE)

# Gets election summary data
summary_string <- election_summary_string(data_folder, full = TRUE)

# Opens politifacts data
facts <- read.csv(file.path(data_folder, "politifact_data.tsv"), sep = "\t")

# Variables for plotting funding info with ggplot
dem_funding <-   format_funding(
  read.csv(file.path(data_folder, "os_dem.tsv"), sep = "\t"))
gop_funding <-   format_funding(
  read.csv(file.path(data_folder, "os_gop.tsv"), sep = "\t"))

# Joins democrat and republican data instead of reading full funding file.
# Faster and easier to work with, but ignores independents
dem_funding$Party <- "Democrat"
gop_funding$Party <- "Republican"
full_funding <- rbind(dem_funding, gop_funding)

# Variables for setting date range of plots
dem_date_range_str <- NULL
dem_start_date <- NULL
dem_end_date <- NULL
gop_date_range_str <- NULL
gop_start_date <- NULL
gop_end_date <- NULL

# Variables for plot colors
dem_colors <- "Set1"
dem_color_num <- 3
gop_colors <- "Set1"
gop_color_num <- 14
funding_grayscale <- FALSE

# Sets default plot data to pollster
dem <- dem_pollster
gop <- gop_pollster


######
### SERVER
######


shinyServer(
  
  # Code here is run once when app is launched
  
  function(input, output) {
      
    # Code here is run each time user visits app
    
    ### Homepage functions
    
    # Download handler for democrat plot download
    output$download_summary <- downloadHandler(
      filename = "election_summary.txt",
      content = function(file) {
        write(summary_string, file)
      }
    )
    
    ### Democrat plotting functions
    
    # Updates input for democrat plot and chooses plotting function
    dem_input <- reactive({
      
      # Sets data source
      if (input$dem_plot_data == "rcp") {
        dem <- dem_rcp
      } else if (input$dem_plot_data == "pollster") {
        dem <- dem_pollster
      }
      
      # Updates candidate list
      # TODO - update candidates faster
      # Can you use subset/filter with the entire vector of candidates that you have?
      candidates <- setdiff(dem_candidates, updated_dem())
      for (c in candidates) {
        dem <- subset(dem, Candidate != c)
      }
      
      # Updates date range based on user input
      dem_date_range_str <- strsplit(as.character(input$dem_date_range), "to")
      dem_start_date <- dem_date_range_str[[1]]
      dem_end_date <- dem_date_range_str[[2]]
      
      # Plots using ggplot depending on type of plot
      p <- NULL
      if (input$dem_plot_type == "smooth") {
        p <- plot_polls_ggplot(dem, 
                               plot_type = "smooth", 
                               n_colors = dem_color_num, 
                               set = dem_colors,
                               start_date = dem_start_date, 
                               end_date = dem_end_date)
      } else if (input$dem_plot_type == "line") {
        p <- plot_polls_ggplot(dem, 
                               plot_type = "line", 
                               n_colors = dem_color_num, 
                               set = dem_colors,
                               start_date = dem_start_date, 
                               end_date = dem_end_date)
      } else if (input$dem_plot_type == "both") {
        p <- plot_polls_ggplot(dem, 
                               plot_type = "both", 
                               n_colors = dem_color_num, 
                               set = dem_colors,
                               start_date = dem_start_date, 
                               end_date = dem_end_date)
      }
      return(p)
    })
    
    # Prints democratic pollng plot
    output$dem_plot <- renderPlot({
      print(dem_input())
    })
    
    # Disables download and file options initially
    disable("download_dem")
    disable("dem_plot_options")
    
    # Enables download and file options if there is a plot
    observe({
      if (input$dem_update > 0) {
        enable("download_dem")
        enable("dem_plot_options")
      }
    })
    
    # Download handler for democrat plot download
    output$download_dem <- downloadHandler(
      filename = function() {
        paste("dem_polls", input$dem_img_type, sep='') 
      },
      content = function(file) {
        # Creates a new graphics device based on user input for image type and resolution
        device <- choose_device(input$dem_img_type, res = input$dem_img_dpi)
        
        # Saves plot to a file using ggsave
        ggsave(file, plot = dem_input(), device = device,
               width = input$dem_plot_width, 
               height = input$dem_plot_height)
      }
    )
    
    # Gets updated list of democratic candidates from user
    updated_dem <- eventReactive(input$dem_update, {
      input$dem_selected
    })
      
    ### Republican plotting functions
    
    # Updates input for democrat plot and chooses plotting function
    gop_input <- reactive({
      
      # Sets data source
      if (input$gop_plot_data == "rcp") {
        gop <- gop_rcp
      } else if (input$gop_plot_data == "pollster") {
        gop <- gop_pollster
      }
      
      # Updates candidate list
      # TODO - update candidates faster
      candidates <- setdiff(gop_candidates, updated_gop())
      for (c in candidates) {
        gop <- subset(gop, Candidate != c)
      }
      
      # Updates date range based on user input
      gop_date_range_str <- strsplit(as.character(input$gop_date_range), "to")
      gop_start_date <- gop_date_range_str[[1]]
      gop_end_date <- gop_date_range_str[[2]]
      
      # Plots poll information based on desired type of plot
      p <- NULL
      if (input$gop_plot_type == "smooth") {
        p <- plot_polls_ggplot(gop, 
                               plot_type = "smooth", 
                               n_colors = gop_color_num, 
                               set = gop_colors,
                               start_date = gop_start_date, 
                               end_date = gop_end_date)
      } else if (input$gop_plot_type == "line") {
        p <- plot_polls_ggplot(gop, 
                               plot_type = "line", 
                               n_colors = gop_color_num, 
                               set = gop_colors,
                               start_date = gop_start_date, 
                               end_date = gop_end_date)
      } else if (input$gop_plot_type == "both") {
        p <- plot_polls_ggplot(gop, 
                               plot_type = "both", 
                               n_colors = gop_color_num, 
                               set = gop_colors,
                               start_date = gop_start_date, 
                               end_date = gop_end_date)
      }
      return(p)
    })
    
    # Prints republican polling plot
    output$gop_plot <- renderPlot({
      print(gop_input())
    })
    
    # Disables download and file options initially
    disable("download_gop")
    disable("gop_plot_options")
    
    # Enables download and file options if there is a plot
    observe({
      if (input$gop_update > 0) {
        enable("download_gop")
        enable("gop_plot_options")
      }
    })
    
    # Download handler for republican plot download
    output$download_gop <- downloadHandler(
      filename = function() {
        paste("gop_polls", input$gop_img_type, sep='') 
      },
      content = function(file) {
        # Creates a new graphics device based on user input for image type and resolution
        device <- choose_device(input$gop_img_type, res = input$gop_img_dpi)
        
        # Saves plot to a file using ggsave
        ggsave(file, plot = gop_input(), device = device,
               width = input$gop_plot_width, 
               height = input$gop_plot_height)
      }
    )
    
    # Gets updated list of republican candidates from user
    updated_gop <- eventReactive(input$gop_update, {
      input$gop_selected
    })
    
    ### Funding plotting functions
    
    # Updates input for funding plot and chooses plotting function
    funding_input <- reactive({
      
      # Sets type of plot and y-axis label based on user input
      type <- ylab <- ""
      if (input$funding_source == "all") { type <- ylab <- "All" } 
      else if (input$funding_source == "campaign") { type <- ylab <- "Campaign" } 
      else if (input$funding_source == "super_pac") { type <- ylab <- "SuperPAC" } 
      else if (input$funding_source == "other") { type <- ylab <- "Other" }
      
      # Uses grayscale if requested
      if (input$funding_grayscale) {
        funding_grayscale <- TRUE
      } else {
        funding_grayscale <- FALSE
      }
      
      # Plots using ggplot depending on desired group of candidates
      p <- NULL
      if (input$funding_groups == "all") {
        p <- plot_funding_ggplot(full_funding, type = type, ylab = "",
                                 grayscale = funding_grayscale)
      } else if (input$funding_groups == "dem") {
        p <- plot_funding_ggplot(dem_funding, type = type, ylab = "",
                                 grayscale = funding_grayscale,
                                 party = "dem")
      } else if (input$funding_groups == "gop") {
        p <- plot_funding_ggplot(gop_funding, type = type, ylab = "",
                                 grayscale = funding_grayscale,
                                 party = "gop")
      }
      return(p)
    })
    
    # Prints funding plot
    output$funding_plot <- renderPlot({
      print(funding_input())
    })
      
    # Download handler for funding plot download
    output$download_funding <- downloadHandler(
      filename = function() {
        paste("funding", input$funding_img_type, sep='') 
      },
      content = function(file) {
        device <- choose_device(input$funding_img_type, res = input$funding_img_dpi)
        ggsave(file, plot = funding_input(), device = device,
               width = input$funding_plot_width, 
               height = input$funding_plot_height)
      }
    )
    
    ### Politifact plotting functions
    
    # Updates input for funding plot and chooses plotting function
    facts_input <- reactive({
      
      # Updates candidate list
      # TODO - update candidates faster
      candidates <- setdiff(all_candidates, updated_all())
      for (c in candidates) {
        facts <- subset(facts, Candidate != c)
      }
      
      # Plots using ggplot depending on desired group of candidates
      p <- plot_facts_ggplot(facts)
      return(p)
    })
    
    # Prints politifact candidate comparison plot
    output$facts_plot <- renderPlot({
      print(facts_input())
    })
    
    # Disables download and file options initially
    disable("download_facts")
    disable("facts_plot_options")
    
    # Enables download and file options if there is a plot
    observe({
      if (input$facts_update > 0) {
        enable("download_facts")
        enable("facts_plot_options")
      }
    })
    
    # Download handler for funding plot download
    output$download_facts <- downloadHandler(
      filename = function() {
        paste("facts", input$funding_img_type, sep='') 
      },
      content = function(file) {
        device <- choose_device(input$facts_img_type, res = input$facts_img_dpi)
        ggsave(file, plot = facts_input(), device = device,
               width = input$facts_plot_width, 
               height = input$facts_plot_height)
      }
    )
    
    # Gets updated list of all candidates from user
    updated_all <- eventReactive(input$facts_update, {
      input$facts_selected
    })
  
})