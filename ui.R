
######
### GLOBAL VARIABLES
######

dem_candidates <- c("Clinton", "Sanders", "Biden", "Webb", "O.Malley", "Chafee")
gop_candidates <- c("Trump", "Carson", "Fiorina", "Rubio", "Bush", "Cruz", 
                    "Kasich", "Christie", "Huckabee", "Paul", "Santorum",
                    "Pataki", "Jindal", "Graham", "Walker")
plot_types <- c("Smooth" = "smooth", "Line" = "line")
funding_types <- c("Campaign", "Super PAC", "Other")

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
               selectInput("fundingSource", "Funding Source:", 
                           choices=funding_types
               )
             ),
             mainPanel(
               
             )
             #plotOutput("plot")
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