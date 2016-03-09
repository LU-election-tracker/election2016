# Election tracker's Shiny UI. 
#
# Written by Henry Ward and Adam Loy, 2015.
# Licensed under the MIT license (tldrlegal.com/license/mit-license)


######
### LIBRARIES AND SCRIPTS
######


# Gets parent and data folders
main_folder <- "./"
data_folder <- file.path(main_folder, "data")

# Sources main functions and loads scripts
election_main <- file.path("src", "election_main.R")
load_scripts(main_folder)


######
### VARIABLES, UTILITY FUNCTIONS AND PREP
######


# List of all democratic and gop candidates
dem_candidates <- c("Clinton", "Sanders", "Biden", "Webb", "O.Malley", "Chafee")
gop_candidates <- c("Trump", "Carson", "Fiorina", "Rubio", "Bush", "Cruz", 
                    "Kasich", "Christie", "Huckabee", "Paul", "Santorum",
                    "Pataki", "Jindal", "Graham", "Walker")

# Updates candidates list
dem <- read.csv(file.path(data_folder, "rcp_dem_full.tsv"), sep = "\t")
gop <- read.csv(file.path(data_folder, "rcp_gop_full.tsv"), sep = "\t")
dem_candidates <- remove_candidates(dem_candidates, dem)
gop_candidates <- remove_candidates(gop_candidates, gop)
all_candidates <- c(dem_candidates, gop_candidates)

# Gets election summary data
summary_string <- election_summary_string(data_folder, full = FALSE)
summaries <- strsplit(summary_string, "\n")

# Sets reactive plotting types and values
plot_types <- c("Smooth" = "smooth", "Line" = "line", "Smooth + Points" = "both")
plot_data <- c("Pollster" = "pollster", "Real Clear Politics" = "rcp")
funding_types <- c("All" = "all", "Campaign" = "campaign", "Super PAC" = "super_pac", 
                   "Other" = "other")
funding_groups <- c("All" = "all", "Democrats" = "dem", "Republicans" = "gop")

# Plot saving options
image_types <- c("png" = ".png", "jpeg" = ".jpg", "tiff" = ".tif")
image_width <- 8
image_height <- 7

# Plot date values
min_date = "2014-01-01"

# Gets current date
current_date <- Sys.Date()


######
### SITE UI
######


shinyUI(navbarPage("LU Election Tracker 2016", 
                   theme = shinytheme("cerulean"),
                   
  tabPanel("Home",
           
           # Enables shinyjs package
           shinyjs::useShinyjs(),
           
           sidebarLayout(
             position = "left",
             
             sidebarPanel(
               p(strong(summaries[[1]][2])),
               p(summaries[[1]][3]),
               p(summaries[[1]][4]),
               p(strong(summaries[[1]][6])),
               p(summaries[[1]][7]),
               p(summaries[[1]][8]),
               p(summaries[[1]][9]),
               downloadButton("download_summary", "Download"),
               width = 3
               ),
             
             mainPanel(
               p(h3("News")),
               p(strong("2/9/16")),
               p("You can now compare candidates by their weighted",
                 a("Politifact", href = politifact_address), " scores!"),
               p(strong("12/12/15")),
               p("An article walking through the creation of this app was posted ",
                 a("here", href = loy_blog_address)),
               p(strong("11/10/15")),
               p("Site officially launched."),
               
               #p(h3("Upcoming Features")),
               #p("State-by-state map of primary results."),
               #p("Poll summaries on homepage."),
               
               p(h3("What We Do")),
               p("We provide a non-partisan collection
                 of up-to-date plots and statistical analyses charting 
                 the 2016 US primary and general elections."),
               p("As the elections progress, we will update this site 
                 with additional features."),
               p("Fork us on ",
                 a("Github.", href = git_address),
                 "Created entirely in R with ",
                 a("Shiny.", href = shiny_address))
               )
           )
          ),
    
  tabPanel("Democrats",
           sidebarLayout(
             sidebarPanel(
               selectInput("dem_plot_type", "Plot Type:", 
                           plot_types
                           ),
               selectInput("dem_plot_data", "Data:", 
                           plot_data
               ),
               dateRangeInput("dem_date_range", "Date range:",
                              start = Sys.Date() - 90,
                              end = Sys.Date() - 3,
                              min = min_date, 
                              max = Sys.Date() - 1,
                              weekstart = 0,
                              separator = "to",
                              width = "100%"),
               checkboxGroupInput("dem_selected", "Candidates",
                            choices=dem_candidates,
                            selected=dem_candidates
                            ),
               actionButton("dem_update", "Show and update plot!", width = "100%"),
               p(),
               downloadButton("download_dem", "Download"),
               checkboxInput("dem_plot_options", "See download options?", value = FALSE),
               conditionalPanel("input.dem_plot_options == true",
                  selectInput("dem_img_type", "Image type:", image_types),
                  sliderInput("dem_img_dpi", "DPI:", min=100, max=1000, value=300),
                  numericInput("dem_plot_width", "Image width:", image_width),
                  numericInput("dem_plot_height", "Image height:", image_height)
               ),
               width = 3
             ),
             mainPanel(
               plotOutput("dem_plot", height = "450px", width = "850px"
               ),
               p(paste("Press the 'Show and update plot' button to display the graph.", 
                       "Last updated " , current_date)),
               p()
               )
             )
           ),
    
  tabPanel("Republicans",
           sidebarLayout(
             sidebarPanel(
               selectInput("gop_plot_type", "Plot Type:", 
                           plot_types
               ),
               selectInput("gop_plot_data", "Data:", 
                           plot_data
               ),
               dateRangeInput("gop_date_range", "Date range:",
                              start = Sys.Date() - 90,
                              end = Sys.Date() - 3,
                              min = min_date, 
                              max = Sys.Date() - 1,
                              weekstart = 0,
                              separator = "to",
                              width = "100%"),
               checkboxGroupInput("gop_selected", "Candidates",
                                  choices=gop_candidates,
                                  selected=gop_candidates
               ),
               actionButton("gop_update", "Show and update plot!", width = "100%"),
               p(),
               downloadButton('download_gop', 'Download'),
               checkboxInput("gop_plot_options", "See download options?", value = FALSE),
               conditionalPanel("input.gop_plot_options == true",
                  selectInput("gop_img_type", "Image type:", image_types),
                  sliderInput("gop_img_dpi", "DPI:", min=100, max=1000, value=300),
                  numericInput("gop_plot_width", "Image width:", image_width),
                  numericInput("gop_plot_height", "Image height:", image_height)
               ),
               width = 3
             ),
             mainPanel(
               plotOutput("gop_plot", height = "450px", width = "850px"
               ),
               p(paste("Press the 'Show and update plot' button to display the graph.", 
                       "Last updated " , current_date)),
               p()
               )
             )
           ),
  
  tabPanel("Funding",
           sidebarLayout(
             sidebarPanel(
               selectInput("funding_source", "Source:", 
                           choices=funding_types
               ),
               selectInput("funding_groups", "Candidates:", 
                           choices=funding_groups
               ),
               checkboxInput("funding_grayscale", "No colors?", value = FALSE),
               downloadButton('download_funding', 'Download'),
               checkboxInput("funding_plot_options", "See download options?", value = FALSE),
               conditionalPanel("input.funding_plot_options == true",
                  selectInput("funding_img_type", "Image type:", image_types),
                  sliderInput("funding_img_dpi", "DPI:", min=100, max=1000, value=300),
                  numericInput("funding_plot_width", "Image width:", image_width),
                  numericInput("funding_plot_height", "Image height:", image_height)
               ),
               width = 3
             ),
             mainPanel(
               plotOutput("funding_plot", height = "450px", width = "850px"
               )
             )
           )
  ),
  
  tabPanel("Facts",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("facts_selected", "Candidates",
                                  choices=all_candidates,
                                  selected=all_candidates
               ),
               p(),
               actionButton("facts_update", "Show and update plot!", width = "100%"),
               p(),
               downloadButton("download_facts", "Download"),
               checkboxInput("facts_plot_options", "See download options?", value = FALSE),
               conditionalPanel("input.facts_plot_options == true",
                                selectInput("facts_img_type", "Image type:", image_types),
                                sliderInput("facts_img_dpi", "DPI:", min=100, max=1000, value=300),
                                numericInput("facts_plot_width", "Image width:", image_width),
                                numericInput("facts_plot_height", "Image height:", image_height)
               ),
               width = 3
             ),
             mainPanel(
               plotOutput("facts_plot", height = "450px", width = "850px"
               ),
               p(),
               p(paste("Press the 'Show and update plot' button to display the graph.", 
                       "Last updated ", current_date)),
               p(),
               p("Data used with permission and transformed by weighting different
                 ruling counts before plotting.")
             )
           )
  ),

  tabPanel("About",
           sidebarLayout(
             sidebarPanel(
               p(a("Pollster Democrat Polls", href = pollster_dem_address)),
               p(a("Pollster Republican Polls", href = pollster_gop_address)),
               p(a("Real Clear Politics Democrat Polls", href = dem_rcp_polls)),
               p(a("Real Clear Politics Republican Polls", href = gop_rcp_polls)),
               p(a("Open Secrets Funding", href = os_address)),
               p(a("Politifact", href = politifact_address)),
               p(a("Github", href = git_address)),
               p(a("Blog", href = loy_address)),
               width = 3
             ),
             mainPanel(
               p(h3("Data")),
               p("All data updated every day at midnight CST."),
               p(),
               p("Poll data is scraped from both Real Clear Politics and Pollster, and 
                  all funding data is scraped from Open Secrets. Links to the data 
                  sources are on the left. Poll numbers are averaged by week per 
                  candidate before plotting. Politifact data is used with permission
                  and transformed before ploting."),
               
               p(h3("Us")),
               p("Created by Lawrence University student Henry Ward and Prof. Adam 
                 Loy. Source code for the entirety of this site is available on ",
                 a("Github.", href = git_address),
                 "Fork us! All code is licensed under the ",
                 a("MIT license.", href = "https://www.tldrlegal.com/l/mit")
               ))
             )
           ),
  
  footer = p("Visit us on ",
             a("Github.", href = git_address),
             "Site created by Henry Ward and ",
             a("Adam Loy", href = loy_address),
             style = "position: relative; bottom:3%; left: 35%; padding:5px; color:gray;")
))