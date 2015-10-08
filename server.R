library(rvest)
library(tidyr)
library(lubridate)
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(Cairo)

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
dem <- read.csv(file.path(data_folder, "rcp_dem_full.csv"), sep = "\t")
dem <- format_polls(dem, dem_candidates)
dem_original <- dem
dem_pal <- colorRampPalette(brewer.pal(9, "RdBu"))

# Variables for plotting republicans with ggplot
gop <- read.csv(file.path(data_folder, "rcp_gop_full.csv"), sep = "\t")
gop <- format_polls(gop, gop_candidates)
gop_original <- gop
gop_pal <- colorRampPalette(brewer.pal(9, "Set1"))

######
### SERVER
######

shinyServer(function(input, output) {
  
  ### Democrat plotting functions
  
  # Plots using ggplot depending on type of plot
  output$dem_plot <- renderPlot({
    
    # TODO - update candidates faster
    candidates <- setdiff(dem_candidates, updated_dem())
    for (c in candidates) {
      dem <- subset(dem, Candidate != c)
    }
    
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
  
  output$funding <- renderPlot({
    
  })
  
})