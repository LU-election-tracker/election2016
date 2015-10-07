library(rvest)
library(tidyr)
library(lubridate)
library(reshape2)
library(dplyr)
library(RColorBrewer)

######
### GLOBAL VARIABLES
######

dem_candidates <- c("Clinton", "Sanders", "Biden", "Webb", "O.Malley", "Chafee")
gop_candidates <- c("Trump", "Carson", "Fiorina", "Rubio", "Bush", "Cruz", 
                    "Kasich", "Christie", "Huckabee", "Paul", "Santorum",
                    "Pataki", "Jindal", "Graham")

######
### UTILITY functioNs
######

# Formats a given RCP poll date using lubridate.
# Rounds value to nearest given interval
format_date <- function(d, year = 2015, interval = "week") {
  d <- parse_date_time(c(d), "%m%d") + years(year)
  d <- floor_date(d, interval)
}

# Formats given RCP poll and melts it by end date for given list of candidates.
# Cuts off all polls before 2/26/15
format_polls <- function(df, candidates, ids = c("End", "Poll")) {
  
  # Removes RCP average
  rcp_row <- which(apply(dem, 1, function(x) any(grepl("RCP Average", x))))
  df <- df[-c(rcp_row),]
  
  # Removes all polls before Qunnipiac poll on 2/26/15
  last_row <- which(apply(dem, 1, function(x) any(grepl("2/26", x))))
  df <- df[1:last_row,]
  
  # Converts end column to dates
  df$End <- as.Date(df$End, format="%m/%d")
    
  # Melts data frame
  df <- melt(df, id.vars = ids, measure.vars = candidates, 
                      variable.name = "Candidate")
  df %>% group_by(End, Candidate) %>% summarise(avg = mean(as.numeric(value)))
}

######
### MAIN FUNCTIONS
######

# Scrapes a given Real Clear Politics page and writes updated csv files with
# most recent data and complete poll data. Splits sample column into number
# and type if sample available (not available for GOP candidates)
scrape_rcp <- function(rcp_address, recent_out, full_out, sample = TRUE) {
  
  # Reads recent poll table as a data frame
  rcp <- html(rcp_address)
  recent <- rcp %>% html_nodes(".data") %>% .[[1]] %>% html_table()
  recent <- separate(data = recent, col = Date, into = c("Start", "End"), sep = " - ")
  if (sample) {
    recent <- separate(data = recent, col = Sample, into = c("Sample", "Type"), sep = "\\ ", fill = "left")
  }
  
  # Reads full poll table as a data frame
  full <- rcp %>% html_nodes(".data") %>% .[[2]] %>% html_table()
  full <- separate(data = full, col = Date, into = c("Start", "End"), sep = " - ")
  if (sample) {
    full <- separate(data = full, col = Sample, into = c("Sample", "Type"), sep = "\\ ", fill = "left")
  }
  
  # Writes both tables out as csvs, separated by tabs
  write.table(recent, recent_out, sep = "\t", quote = FALSE, row.names = FALSE)
  write.table(full, full_out, sep = "\t", quote = FALSE, row.names = FALSE)
}

# Scrapes the Open Secrets website for funding information
scrape_os<- function(os_address, funding_out) {
  opensecrets <- html("https://www.opensecrets.org/pres16/outsidegroups.php")
  funding <- opensecrets %>% html_node("table") %>% html_table()
}

# Updates Real Clear Politics poll csv files, outputting to the given folder.
update_rcp <- function(main_folder, data_folder) {
  dem_address <- "http://www.realclearpolitics.com/epolls/2016/president/us/2016_democratic_presidential_nomination-3824.html"
  gop_address <- "http://www.realclearpolitics.com/epolls/2016/president/us/2016_republican_presidential_nomination-3823.html"
  scrape_rcp(dem_address, 
             file.path(data_folder, "rcp_dem_recent.csv"),
             file.path(data_folder, "rcp_dem_full.csv"))
  scrape_rcp(gop_address, 
             file.path(data_folder, "rcp_gop_recent.csv"),
             file.path(data_folder, "rcp_gop_full.csv"),
             sample = FALSE)
}

# Creates plots for a given rcp table
plot_rcp <- function(main_folder, data_folder) {
  
  # Opens poll summary files into data frames
  dem <- read.csv(file.path(data_folder, "rcp_dem_full.csv"), sep = "\t")
  gop <- read.csv(file.path(data_folder, "rcp_gop_full.csv"), sep = "\t")
  
  # Formats data frames and plots party averages over time
  dem_plot <- plot_over_time(format_polls(dem, dem_candidates), main_folder, "dem.png")
  gop_plot <- plot_over_time(format_polls(gop, gop_candidates), main_folder, "gop.png", 
                             n_colors = 15, set = "Set1")
}

# Plots candidates polling results over a given time by week
plot_over_time <- function(df, main_folder = "", plot_name = "", n_colors = 6, 
                           set = "RdBu") {
  
  # Color pallete to extrapolate from
  full_pal <- colorRampPalette(brewer.pal(9, set))
  
  # Base plot for given polls
  ggplot(df, aes(x = End, y = avg, color = Candidate)) + 
    geom_smooth(aes(group = Candidate), method = "loess") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = full_pal(n_colors)) +
    scale_x_date() +
    labs(x = "Month", y = "Average Percent Support")
  
  # Saves to "www" folder as png image if folder and plot name given
  if ((plot_name != "") && (main_folder != "")) {
    ggsave(file = file.path(main_folder, "www", plot_name))
  }
}

# Archives old poll information
archive <- function(main_folder, data_folder, archive_folder) {
  
  # Gets all current files
  dem_plot <-"dem.png"
  dem_recent <- "rcp_dem_recent.csv"
  dem_full <- "rcp_dem_full.csv"
  gop_plot <- "gop.png"
  gop_recent <- "rcp_gop_recent.csv"
  gop_full <- "rcp_gop_full.csv"
  
  # Creates an archive folder for the current day. Overwrites if exists
  archive <- file.path(archive_folder, as.character(Sys.Date()))
  dir.create(archive, showWarnings = FALSE)
  
  # Archives current files. Overwrites if exists
  file.copy(file.path(data_folder, dem_plot), file.path(archive, dem_plot), overwrite = TRUE)
  file.copy(file.path(data_folder, dem_recent), file.path(archive, dem_recent), overwrite = TRUE)
  file.copy(file.path(data_folder, dem_full), file.path(archive, dem_full), overwrite = TRUE)
  file.copy(file.path(data_folder, gop_plot), file.path(archive, gop_plot), overwrite = TRUE)
  file.copy(file.path(data_folder, gop_recent), file.path(archive, gop_recent), overwrite = TRUE)
  file.copy(file.path(data_folder, gop_full), file.path(archive, gop_full), overwrite = TRUE)
}


# Main update function. Updates polling data and graphs.
track <- function(main_folder) {
  
  # Creates folder to hold poll data and folder to hold shiny data
  data_folder <- file.path(main_folder, "data")
  shiny_folder <- file.path(main_folder, "www")
  archive_folder <- file.path(data_folder, "archive")
  if(dir.exists(main_folder) == FALSE) { dir.create(main_folder) }
  if(dir.exists(data_folder) == FALSE) { dir.create(data_folder) }
  if(dir.exists(shiny_folder) == FALSE) { dir.create(shiny_folder) }
  if(dir.exists(archive_folder) == FALSE) { dir.create(archive_folder) }
  
  # Updates Real Clear Politics polls
  update_rcp(main_folder, data_folder)
  
  # Creates plots
  plot_rcp(main_folder, data_folder)
  
  # Archives data by current date
  invisible(archive(main_folder, data_folder, archive_folder))
}

# Sample usage:
# main_folder <- file.path("C:", "Users", "Navi", "Dropbox", "Public", "Junior Year", "R", "election2016", "data")
# track(main_folder)

# Misc. code:
# dem <- read.csv(file.path(main_folder, "rcp_dem_full.csv"), sep = "\t")
# gop <- read.csv(file.path(main_folder, "rcp_gop_full.csv"), sep = "\t")
# file.info(file.path(main_folder, "rcp_dem_full.csv"))$mtime