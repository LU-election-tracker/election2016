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
#main_folder <- "C:/Users/Navi/Dropbox/Public/Junior Year/R/election2016"
main_folder <- "./"
data_folder <- file.path(main_folder, "data")

# Variables for plotting democrats with ggplot
dem <- read.csv(file.path(data_folder, "rcp_dem_full.tsv"), sep = "\t")
dem <- format_polls(dem, dem_candidates)
dem_original <- dem
dem_pal <- colorRampPalette(brewer.pal(9, "RdBu"))

# Variables for plotting republicans with ggplot
gop <- read.csv(file.path(data_folder, "rcp_gop_full.tsv"), sep = "\t")
gop <- format_polls(gop, gop_candidates)
gop_original <- gop
gop_pal <- colorRampPalette(brewer.pal(9, "Set1"))

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

shinyServer(function(input, output) {
  
  ### Democrat plotting functions
  
  output$dem_plot <- renderPlot({
    
    # TODO - update candidates faster
    candidates <- setdiff(dem_candidates, updated_dem())
    for (c in candidates) {
      dem <- subset(dem, Candidate != c)
    }
    
    # Plots using ggplot depending on type of plot
    if (input$dem_plot_type == "smooth") {
      ggplot(dem, aes(x = End, y = avg, color = Candidate)) + 
        geom_smooth(aes(group = Candidate), method = "loess") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(values = dem_pal(6)) +
        scale_x_date() +
        labs(x = "Month", y = "Average Percent Support")
      
    } else if (input$dem_plot_type == "line") {
      ggplot(dem, aes(x = End, y = avg, color = Candidate)) + 
        geom_line(aes(group = Candidate)) +
        geom_point() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(values = dem_pal(6)) +
        scale_x_date() +
        labs(x = "Month", y = "Average Percent Support")
      
    } else if (input$dem_plot_type == "both") {
      ggplot(dem, aes(x = End, y = avg, color = Candidate)) + 
        geom_smooth(aes(group = Candidate), method = "loess") +
        geom_line(aes(group = Candidate)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(values = dem_pal(6)) +
        scale_x_date() +
        labs(x = "Month", y = "Average Percent Support")
    }
  })
  
  # Gets updated list of democratic candidates from user
  updated_dem <- eventReactive(input$dem_update, {
    input$dem_selected
  })
  
  ### Republican plotting functions
  
  output$gop_plot <- renderPlot({
    
    # TODO - update candidates faster
    candidates <- setdiff(gop_candidates, updated_gop())
    for (c in candidates) {
      gop <- subset(gop, Candidate != c)
    }
    
    # Plots poll information based on desired type of plot
    if (input$gop_plot_type == "smooth") {
      ggplot(gop, aes(x = End, y = avg, color = Candidate)) + 
        geom_smooth(aes(group = Candidate), method = "loess") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(values = gop_pal(16)) +
        scale_x_date() +
        labs(x = "Month", y = "Average Percent Support")
      
    } else if (input$gop_plot_type == "line") {
      ggplot(gop, aes(x = End, y = avg, color = Candidate)) + 
        geom_line(aes(group = Candidate)) +
        geom_point() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(values = gop_pal(16)) +
        scale_x_date() +
        labs(x = "Month", y = "Average Percent Support")
      
    } else if (input$gop_plot_type == "both") {
      ggplot(gop, aes(x = End, y = avg, color = Candidate)) +
        geom_smooth(aes(group = Candidate), method = "loess") +
        geom_line(aes(group = Candidate)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(values = gop_pal(16)) +
        scale_x_date() +
        labs(x = "Month", y = "Average Percent Support")
    }
  })
  
  # Gets updated list of republican candidates from user
  updated_gop <- eventReactive(input$gop_update, {
    input$gop_selected
  })
  
  ### Funding plotting functions
  
  output$funding_plot <- renderPlot({
    
    # Plots using ggplot depending on desired group of candidates
    if (input$funding_groups == "all") {
      plot_funding_ggplot(full_funding)
    } else if (input$funding_groups == "dem") {
      plot_funding_ggplot(dem_funding)
    } else if (input$funding_groups == "gop") {
      plot_funding_ggplot(gop_funding)
    }
  })
  
})