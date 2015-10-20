library(rvest)
library(tidyr)
library(lubridate)
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(Cairo)
library(scales)

######
### VARIABLES, UTILITY FUNCTIONS AND CALLS
######

# Assumes tracking file in same folder
tracking <- 'get_polls.R'
source(tracking)

# Gets parent folder
main_folder <- "./"
data_folder <- file.path(main_folder, "data")

# Variables for plotting democrats with ggplot
dem <- read.csv(file.path(data_folder, "rcp_dem_full.tsv"), sep = "\t")
dem <- format_polls(dem, dem_candidates)
dem_original <- dem

# Variables for plotting republicans with ggplot
gop <- read.csv(file.path(data_folder, "rcp_gop_full.tsv"), sep = "\t")
gop <- format_polls(gop, gop_candidates)
gop_original <- gop

# Variables for plotting poll info with ggplot
dem_funding <-   format_funding(
  read.csv(file.path(data_folder, "os_dem.tsv"), sep = "\t"))
gop_funding <-   format_funding(
  read.csv(file.path(data_folder, "os_gop.tsv"), sep = "\t"))
full_funding <-  format_funding(
  read.csv(file.path(data_folder, "os_full.tsv"), sep = "\t"))


######
### SERVER
######

shinyServer(
  
  # Code here is run once when app is launched
  
  function(input, output) {
      
    # Code here is run each time user visits app
    plot_rcp_ggplot(dem_original, plot_type = "smooth", n_colors = 6, set = "RdBu")
    plot_rcp_ggplot(gop_original, plot_type = "smooth", n_colors = 6, set = "RdBu")
    
    ### Democrat plotting functions
    
    # Updates input for democrat plot and chooses plotting function
    dem_input <- reactive({
      
      # TODO - update candidates faster
      candidates <- setdiff(dem_candidates, updated_dem())
      for (c in candidates) {
        dem <- subset(dem, Candidate != c)
      }
      
      # Plots using ggplot depending on type of plot
      p <- NULL
      if (input$dem_plot_type == "smooth") {
        p <- plot_rcp_ggplot(dem, plot_type = "smooth", n_colors = 6, set = "RdBu")
      } else if (input$dem_plot_type == "line") {
        p <- plot_rcp_ggplot(dem, plot_type = "line", n_colors = 6, set = "RdBu")
      } else if (input$dem_plot_type == "both") {
        p <- plot_rcp_ggplot(dem, plot_type = "both", n_colors = 6, set = "RdBu")
      }
      return(p)
    })
    
    # Prints democratic pollng plot
    output$dem_plot <- renderPlot({
      print(dem_input())
    })
    
    # Download handler for democrat plot download
    output$download_dem <- downloadHandler(
      filename = function() { paste("dem_polls", '.png', sep='') },
      content = function(file) {
        device <- function(..., width, height) 
          grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, plot = dem_input(), device = device)
      }
    )
    
    # Gets updated list of democratic candidates from user
    updated_dem <- eventReactive(input$dem_update, {
      input$dem_selected
    })
      
    ### Republican plotting functions
    
    # Updates input for democrat plot and chooses plotting function
    gop_input <- reactive({
      
      # TODO - update candidates faster
      candidates <- setdiff(gop_candidates, updated_gop())
      for (c in candidates) {
        gop <- subset(gop, Candidate != c)
      }
      
      # Plots poll information based on desired type of plot
      p <- NULL
      if (input$gop_plot_type == "smooth") {
        p <- plot_rcp_ggplot(gop, plot_type = "smooth", n_colors = 16, set = "Set1")
      } else if (input$gop_plot_type == "line") {
        p <- plot_rcp_ggplot(gop, plot_type = "line", n_colors = 16, set = "Set1")
      } else if (input$gop_plot_type == "both") {
        p <- plot_rcp_ggplot(gop, plot_type = "both", n_colors = 16, set = "Set1")
      }
      return(p)
    })
    
    # Prints republican polling plot
    output$gop_plot <- renderPlot({
      print(gop_input())
    })
    
    # Download handler for republican plot download
    output$download_gop <- downloadHandler(
      filename = function() { paste("gop_polls", '.png', sep='') },
      content = function(file) {
        device <- function(..., width, height) 
          grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, plot = gop_input(), device = device)
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
      else if (input$funding_source == "super_pac") { type <- ylab <- "Super PAC" } 
      else if (input$funding_source == "other") { type <- ylab <- "Other" }
      
      # Plots using ggplot depending on desired group of candidates
      p <- NULL
      if (input$funding_groups == "all") {
        p <- plot_funding_ggplot(full_funding, type = type, ylab = "")
      } else if (input$funding_groups == "dem") {
        p <- plot_funding_ggplot(dem_funding, type = type, ylab = "")
      } else if (input$funding_groups == "gop") {
        p <- plot_funding_ggplot(gop_funding, type = type, ylab = "")
      }
      return(p)
    })
    
    # Prints funding plot
    output$funding_plot <- renderPlot({
      print(funding_input())
    })
    
    # Download handler for funding plot download
    output$download_funding <- downloadHandler(
      filename = function() { paste("funding", '.png', sep='') },
      content = function(file) {
        device <- function(..., width, height) 
          grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, plot = funding_input(), device = device)
      }
    )
  
})