
######
### GLOBAL VARIABLES
######

dem_candidates <- c("Clinton", "Sanders", "Biden", "Webb", "O.Malley", "Chafee")
gop_candidates <- c("Trump", "Carson", "Fiorina", "Rubio", "Bush", "Cruz", 
                    "Kasich", "Christie", "Huckabee", "Paul", "Santorum",
                    "Pataki", "Jindal", "Graham", "Walker")
plot_types <- c("Smooth" = "smooth", "Line" = "line", "Both" = "both")
funding_types <- c("Campaign" = "campaign", "Super PAC" = "super_pac", 
                   "Other" = "other")
funding_groups <- c("All" = "all", "Democrats" = "dem", "Republicans" = "gop")

######
### SITE UI
######

shinyUI(navbarPage("LU Election Tracker 2016",
    
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
           p("Visit us on ",
           a("Github.", 
             href = "https://github.com/LU-election-tracker/election2016"),
           "All code licensed under the ",
           a("MIT license.", href = "https://www.tldrlegal.com/l/mit")
           ))
))