library(rvest)
library(tidyr)

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

# Updates Real Clear Politics poll csv files, outputting to the given folder.
update_rcp <- function(folder) {
  dems_address <- "http://www.realclearpolitics.com/epolls/2016/president/us/2016_democratic_presidential_nomination-3824.html"
  reps_address <- "http://www.realclearpolitics.com/epolls/2016/president/us/2016_republican_presidential_nomination-3823.html"
  scrape_rcp(dems_address, 
             file.path(folder, "rcp_dem_recent.csv"),
             file.path(folder, "rcp_dem_full.csv"))
  scrape_rcp(reps_address, 
             file.path(folder, "rcp_gop_recent.csv"),
             file.path(folder, "rcp_gop_full.csv"),
             sample = FALSE)
}

# Creates plots for a given rcp table
plot_rcp <- function(table_file) {
  table - read.csv(file, sep = "\t")
}

# Archives old poll information
archive <- function(main_folder, data_folder) {
  
  # Gets all current files
  dem_recent <- "rcp_dem_recent.csv"
  dem_full <- "rcp_dem_full.csv"
  gop_recent <- "rcp_gop_recent.csv"
  gop_full <- "rcp_gop_full.csv"
  
  # Creates an archive folder for the current day. Overwrites if exists
  archive <- file.path(data_folder, as.character(Sys.Date()))
  dir.create(archive, showWarnings = FALSE)
  
  # Archives current files. Overwrites if exists
  file.copy(file.path(main_folder, dem_recent), file.path(archive, dem_recent),
            overwrite = TRUE)
  file.copy(file.path(main_folder, dem_full), file.path(archive, dem_full), 
            overwrite = TRUE)
  file.copy(file.path(main_folder, gop_recent), file.path(archive, gop_recent), 
            overwrite = TRUE)
  file.copy(file.path(main_folder, gop_full), file.path(archive, gop_full), 
            overwrite = TRUE)
}


# Main update function. Updates polling data and graphs.
track <- function() {
  main_folder <- file.path("C:", "Users", "Navi", "Dropbox", "Public", "Junior Year", 
                      "R", "election2016", "Poll_data")
  data_folder <- file.path(main_folder, "Archive")
  if(dir.exists(main_folder) == FALSE) { dir.create(main_folder) }
  if(dir.exists(data_folder) == FALSE) { dir.create(data_folder) }
  update_rcp(main_folder)
  invisible(archive(main_folder, data_folder))
}

# Sample debug code
# source("C:\\Users\\Navi\\Dropbox\\Public\\Junior Year\\R\\election2016\\get_polls.R")
# track()
# dem <- read.csv(file.path(main_folder, "rcp_dem_full.csv"), sep = "\t")
# gop <- read.csv(file.path(main_folder, "rcp_gop_full.csv"), sep = "\t")
# file.info(file.path(main_folder, "rcp_dem_full.csv"))$mtime