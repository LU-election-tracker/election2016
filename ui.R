shinyUI(fluidPage(
  titlePanel("LU Election Tracker 2016"),
  
  navlistPanel(
    
    "Democrats",
    tabPanel("Poll Averages", 
             img(src = "dem.png", height=500, width = 900)),
    tabPanel("Funding"),
    
    "Republicans",
    tabPanel("Poll Averages",
             img(src = "gop.png", height=500, width = 900)),
    tabPanel("Funding"),
    
    "Info",
    tabPanel("About",
             p("Visit us on ",
             a("Github.", 
               href = "https://github.com/LU-election-tracker/election2016"),
             "All code licensed under the ",
             a("MIT license.", href = "https://www.tldrlegal.com/l/mit")
             ))
  )
))