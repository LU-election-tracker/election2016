library(shinythemes)

######
### GLOBAL VARIABLES
######

dem_candidates <- c("Clinton", "Sanders", "Biden", "Webb", "O.Malley", "Chafee")
gop_candidates <- c("Trump", "Carson", "Fiorina", "Rubio", "Bush", "Cruz", 
                    "Kasich", "Christie", "Huckabee", "Paul", "Santorum",
                    "Pataki", "Jindal", "Graham", "Walker")
plot_types <- c("Smooth" = "smooth", "Line" = "line", "Both" = "both")
funding_types <- c("All" = "all", "Campaign" = "campaign", "Super PAC" = "super_pac", 
                   "Other" = "other")
funding_groups <- c("All" = "all", "Democrats" = "dem", "Republicans" = "gop")

rcp_dem_address <- "http://www.realclearpolitics.com/epolls/2016/president/us/2016_democratic_presidential_nomination-3824.html"
rcp_gop_address <- "http://www.realclearpolitics.com/epolls/2016/president/us/2016_republican_presidential_nomination-3823.html"
os_address <- "https://www.opensecrets.org/pres16/outsidegroups.php"
git_address <- "https://github.com/LU-election-tracker/election2016"
lawrence_address <- "http://www.lawrence.edu/"

######
### SITE UI
######

shinyUI(navbarPage("LU Election Tracker 2016", theme = shinytheme("cerulean"),
    
  tabPanel("Democrats",
           sidebarLayout(
             sidebarPanel(
               selectInput("dem_plot_type", "Plot Type:", 
                           plot_types
                           ),
               checkboxGroupInput("dem_selected", "Candidates",
                            choices=dem_candidates,
                            selected=dem_candidates
                            ),
               actionButton("dem_update", "Update!")
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
               selectInput("gop_plot_type", "Plot Type:", 
                           plot_types
               ),
               checkboxGroupInput("gop_selected", "Candidates",
                                  choices=gop_candidates,
                                  selected=gop_candidates
               ),
               actionButton("gop_update", "Update!")
             ),
             mainPanel(
               plotOutput("gop_plot", height = "450px", width = "850px"
               )
             )
             #plotOutput("plot")
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
               )
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
               p(a("Democrat Poll Info", href = rcp_dem_address)),
               p(a("Republican Poll Info", href = rcp_gop_address)),
               p(a("Github", href = git_address))
             ),
             mainPanel(
               p(h3("Data")),
               
               p("All poll data is scraped from Real Clear Politics, and all funding
                 data is scraped from Open Secrets. Links to the data sources are
                 on the left. Poll numbers are averaged by week per candidate before
                 plotting. Data are updated daily at midnight CST." ),
               
               p(h3("Us")),
               
               p("Created by Lawrence University student Henry Ward and Prof. Adam 
                 Loy. Source code for the entirety of this site is available on ",
                 a("Github.", href = "https://github.com/LU-election-tracker/election2016"),
                 "All code is licensed under the ",
                 a("MIT license.", href = "https://www.tldrlegal.com/l/mit")
               ))
             )
           )
))