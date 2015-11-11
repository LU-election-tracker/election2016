library(shinythemes)

######
### GLOBAL VARIABLES
######

# Assumes tracking file in same folder
tracking <- 'get_polls.R'
source(tracking)

# Gets parent folder
main_folder <- "./"
data_folder <- file.path(main_folder, "data")

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

# Sets reactive plotting types and values
plot_types <- c("Smooth" = "smooth", "Line" = "line", "Both" = "both")
plot_data <- c("Pollster" = "pollster", "Real Clear Politics" = "rcp")
funding_types <- c("All" = "all", "Campaign" = "campaign", "Super PAC" = "super_pac", 
                   "Other" = "other")
funding_groups <- c("All" = "all", "Democrats" = "dem", "Republicans" = "gop")

# HTTP addresses
rcp_dem_address <- "http://www.realclearpolitics.com/epolls/2016/president/us/2016_democratic_presidential_nomination-3824.html"
rcp_gop_address <- "http://www.realclearpolitics.com/epolls/2016/president/us/2016_republican_presidential_nomination-3823.html"
pollster_dem_address <- "http://elections.huffingtonpost.com/pollster/2016-national-democratic-primary"
pollster_gop_address <- "http://elections.huffingtonpost.com/pollster/2016-national-gop-primary"
os_address <- "https://www.opensecrets.org/pres16/outsidegroups.php"
git_address <- "https://github.com/LU-election-tracker/election2016"
loy_address <- "http://aloy.github.io/posts/"
shiny_address <- "http://shiny.rstudio.com/"
lawrence_address <- "http://www.lawrence.edu/"

######
### SITE UI
######

shinyUI(navbarPage("LU Election Tracker 2016", theme = shinytheme("cerulean"),
                   
  tabPanel("Home",
                              mainPanel(
                                p(h3("News")),
                                p("Site officially launched on 11/10/15!"),
                                
                                p(h3("Upcoming Features")),
                                p("State-by-state map of primary results and customizable
                                  date ranges for all plots."),
                                
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
           ),
    
  tabPanel("Democrats",
           sidebarLayout(
             sidebarPanel(
               actionButton("dem_update", "Update!", width = "100%"),
               p(),
               selectInput("dem_plot_type", "Plot Type:", 
                           plot_types
                           ),
               selectInput("dem_plot_data", "Data:", 
                           plot_data
               ),
               checkboxGroupInput("dem_selected", "Candidates",
                            choices=dem_candidates,
                            selected=dem_candidates
                            ),
               downloadButton('download_dem', 'Download')
               ),
             mainPanel(
               plotOutput("dem_plot", height = "450px", width = "850px"
               )
               )
             )
           ),
    
  tabPanel("Republicans",
           sidebarLayout(
             sidebarPanel(
               actionButton("gop_update", "Update!", width = "100%"),
               p(),
               selectInput("gop_plot_type", "Plot Type:", 
                           plot_types
               ),
               selectInput("gop_plot_data", "Data:", 
                           plot_data
               ),
               checkboxGroupInput("gop_selected", "Candidates",
                                  choices=gop_candidates,
                                  selected=gop_candidates
               ),
               downloadButton('download_gop', 'Download')
             ),
             mainPanel(
               plotOutput("gop_plot", height = "450px", width = "850px"
               )
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
               downloadButton('download_funding', 'Download')
             ),
             mainPanel(
               plotOutput("funding_plot", height = "450px", width = "850px"
               )
             )
           )
  ),

  tabPanel("About",
           sidebarLayout(
             sidebarPanel(
               p(a("Pollster Democrat Polls", href = pollster_dem_address)),
               p(a("Pollster Republican Polls", href = pollster_gop_address)),
               p(a("Real Clear Politics Democrat Polls", href = rcp_dem_address)),
               p(a("Real Clear Politics Republican Polls", href = rcp_gop_address)),
               p(a("Open Secrets Funding", href = os_address)),
               p(a("Github", href = git_address)),
               p(a("Blog", href = loy_address))
             ),
             mainPanel(
               p(h3("Data")),
               p("All data updated every day at midnight CST."),
               p(),
               p("Poll data is scraped from both Real Clear Politics and Pollster, and 
                  all funding data is scraped from Open Secrets. Links to the data 
                  sources are on the left. Poll numbers are averaged by week per 
                  candidate before plotting."),
               
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
             style = "position: absolute; bottom:3%; left: 35%; padding:5px; color:gray;")
))