library(rvest)
library(tidyr)
library(lubridate)
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(scales)

######
### GLOBAL VARIABLES
######

dem_candidates <- c("Clinton", "Sanders", "Biden", "Webb", "O.Malley", "Chafee")
gop_candidates <- c("Trump", "Carson", "Fiorina", "Rubio", "Bush", "Cruz", 
                    "Kasich", "Christie", "Huckabee", "Paul", "Santorum",
                    "Pataki", "Jindal", "Graham", "Walker")

######
### UTILITY FUNCTIONS
######

# Removes candidates who dropped out of the race
remove_candidates <- function(candidates, df) {
  cn <- colnames(df)
  candidates <- candidates[candidates %in% cn]
  return(candidates)
}

# Appends years to a col in a dataframe. Assumes date column named "End." Returns data frame. 
# Assumes values exist for January and the df goes backwards in years.
format_date_col <- function(df, format = "%m/%d") {
  
  # Converts col to date
  df$End <- as.Date(df$End, format = format)
  
  # Iterates through each value in column, going backwards a year
  # for next month after last value in January. Uses lubridate
  current_year = 0
  current_date <- previous_month <- current_month <- NULL
  for(i in 1:length(df$End)) {
    current_date <- df$End[i]
    current_month <- month(as.Date(current_date, format)) # as.Date not needed?
    if (is.null(previous_month)) {
      previous_month = current_month
    }
    if (previous_month == 1) {
      if(current_month != 1) {
        current_year = current_year - 1
      }
    }
    df$End[i] <- current_date + years(current_year)
    previous_month <- current_month
  }
  
  # Explicitly returns df
  return(df)
}

# Formats given RCP poll and melts it by end date for given list of candidates.
# Cuts off all polls before 2/26/15
# Is this specific to RCP?
format_polls <- function(df, candidates, cutoff = NULL, ids = c("End", "Poll"),
                         format_dates = TRUE) {
  
  # Removes candidates from table if no longer in race
  # Look into using select or subset to make this one line?
  column_names <- colnames(df)
  for (can in candidates) {
    if (is.element(can, column_names) == FALSE) {
      candidates <- setdiff(candidates, c(can))
    }
  }
  
  # Removes all polls before given cutoff
  # What about dates between poll dates?
  if (!is.null(cutoff)) {
    last_row <- which(apply(df, 1, function(x) any(grepl(cutoff, x))))
    df <- df[1:last_row,]
  }
  
  # Converts end column to dates with the correct year
  # WARNING: this method is slow, so use only when necessary
  df <- format_date_col(df)
    
  # Melts data frame
  df <- melt(df, id.vars = ids, measure.vars = candidates, 
                      variable.name = "Candidate")
  df %>% group_by(End, Candidate) %>% summarise(avg = mean(as.numeric(value)))
}

# Formats given funding table and melts it
format_funding <- function(df, na.rm = TRUE) {
  
  # Drops organization column
  df <- df[, c("Candidate", "Type", "Total.Raised")] # added comma?
  
  # Renames funding column to 'Raised'
  names(df)[names(df) == 'Total.Raised'] <- 'Raised'
  
  # Removes NA and zero values if specified
  if (na.rm) {
    df <- df[df$Raised != "0N/A",]
    df <- df[df$Raised != "$0",]
  }
  
  # Removes dollar signs and columns from funding column
  df$Raised <- substring(df$Raised, 2)
  df$Raised <- as.numeric(gsub(",","", df$Raised))
  
  # Melts data frame, summing super PAC, campaign and other spending
  df <- melt(df, na.rm = na.rm)
  df %>% group_by(Candidate, Type, variable) %>% summarise(Total = sum(as.numeric(value)))
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

# Scrapes a given Pollster page and writes updated csv poll files.
# Note that the dates column is named "Dates" here instead of RCP's "Date"
scrape_pollster <- function(pollster_address, out_file) {
  
  # Reads poll table as a data frame
  pollster <- html(pollster_address)
  out <- pollster %>% html_node("#poll-table") %>% html_table()
  
  # Splits date col into start and end date
  out <- separate(data = out, col = Dates, into = c("Start", "End"), sep = " - ")
  
  # Remove all types of whitespace and "NEW!" from data frame
  out <- data.frame(apply(out, 2, function(x) gsub("\n", "", x, fixed = TRUE)))
  out <- data.frame(apply(out, 2, function(x) gsub(" ", "", x, fixed = TRUE)))
  out <- data.frame(apply(out, 2, function(x) gsub("NEW!", "", x, fixed = TRUE)))
  
  # Writes tables out as a csv, separated by tabs
  write.table(out, out_file, sep = "\t", quote = FALSE, row.names = FALSE)
}

# Scrapes the Open Secrets website for funding information
scrape_funding <- function(full_out, dem_out, gop_out) { # Pass in dem/gop candidate list?
  
  # Readings funding data as a data frame
  opensecrets <- html("https://www.opensecrets.org/pres16/outsidegroups.php")
  funding <- opensecrets %>% html_node("table") %>% html_table()
  
  # Splits table into dem and gop tables
  funding_dem <- funding[(funding$Candidate %in% dem_candidates), ]
  funding_gop <- funding[(funding$Candidate %in% gop_candidates), ]
  
  # Writes tables out for all candidates, democrats and republicans
  write.table(funding, full_out, sep = "\t", quote = FALSE, row.names = FALSE)
  write.table(funding_dem, dem_out, sep = "\t", quote = FALSE, row.names = FALSE)
  write.table(funding_gop, gop_out, sep = "\t", quote = FALSE, row.names = FALSE)
}

# Updates Open Secrets funding information, outputting to the given folder
update_funding <- function(data_folder) {
  scrape_funding(file.path(data_folder, "os_full.tsv"), 
             file.path(data_folder, "os_dem.tsv"),
             file.path(data_folder, "os_gop.tsv"))
}

# Updates Real Clear Politics poll csv files, outputting to the given folder
update_rcp <- function(data_folder) {
  dem_address <- "http://www.realclearpolitics.com/epolls/2016/president/us/2016_democratic_presidential_nomination-3824.html"
  gop_address <- "http://www.realclearpolitics.com/epolls/2016/president/us/2016_republican_presidential_nomination-3823.html"
  scrape_rcp(dem_address, 
             file.path(data_folder, "rcp_dem_recent.tsv"),
             file.path(data_folder, "rcp_dem_full.tsv"))
  scrape_rcp(gop_address, 
             file.path(data_folder, "rcp_gop_recent.tsv"),
             file.path(data_folder, "rcp_gop_full.tsv"),
             sample = FALSE)
}

# Updates Pollster poll csv files, outputting to the given folder
update_pollster <- function(data_folder) {
  dem_address <- "http://elections.huffingtonpost.com/pollster/2016-national-democratic-primary"
  gop_address <- "http://elections.huffingtonpost.com/pollster/2016-national-gop-primary"
  scrape_pollster(dem_address,
                  file.path(data_folder, "pollster_dem.tsv"))
  scrape_pollster(gop_address,
                  file.path(data_folder, "pollster_gop.tsv")) 
}

# Creates plots for a given rcp table. Wrapper for plot_polls_gplot
plot_rcp <- function(main_folder, data_folder) {
  # Remove duplicate candidate names?
  
  # Opens poll summary files into data frames
  dem <- read.csv(file.path(data_folder, "rcp_dem_full.tsv"), sep = "\t")
  gop <- read.csv(file.path(data_folder, "rcp_gop_full.tsv"), sep = "\t")
  
  # Removes RCP average from both data frames
  rcp_row <- which(apply(dem, 1, function(x) any(grepl("RCP Average", x))))
  dem <- dem[-c(rcp_row),]
  rcp_row <- which(apply(gop, 1, function(x) any(grepl("RCP Average", x))))
  gop <- gop[-c(rcp_row),]
  
  # Formats data frames and plots party averages over time
  # Removes all polls before CNN poll on 6/28/15
  # Add some controls for tuning graphics? (dpi, size, etc.)
  dem_plot <- plot_polls_ggplot(format_polls(dem, dem_candidates, "6/28"), main_folder, "rcp_dem.png")
  gop_plot <- plot_polls_ggplot(format_polls(gop, gop_candidates, "6/28"), main_folder, "rcp_gop.png", 
                             n_colors = 16, set = "Set1")
}

# Creates plots for a given pollster table. Wrapper for plot_pollster_gplot
plot_pollster <- function(main_folder, data_folder) {
  
  # Opens poll summary files into data frames
  dem <- read.csv(file.path(data_folder, "pollster_dem.tsv"), sep = "\t")
  gop <- read.csv(file.path(data_folder, "pollster_gop.tsv"), sep = "\t")
  
  # Changes first column name to "Poll" in order to match RCP format
  colnames(dem)[1] <- "Poll"
  colnames(gop)[1] <- "Poll"
  
  # Formats data frames and plots party averages over time
  dem_plot <- plot_polls_ggplot(format_polls(dem, dem_candidates), main_folder, "pollster_dem.png")
  gop_plot <- plot_polls_ggplot(format_polls(gop, gop_candidates), main_folder, "pollster_gop.png", 
                              n_colors = 16, set = "Set1")
}

# Plots candidates polling results over a given time by week
plot_polls_ggplot <- function(df, main_folder = "", plot_name = "", plot_type = "smooth",
                              n_colors = 6, set = "RdBu", xlim = NULL, ylim = NULL) {
  
  # Color pallete to extrapolate from
  full_pal <- colorRampPalette(brewer.pal(9, set))
  
  # Standard plot for given polls
  p <- ggplot(df, aes(x = End, y = avg, color = Candidate)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = full_pal(n_colors)) +
    scale_x_date() +
    labs(x = "Month", y = "Average Percent Support")
  
  # Sets type of plot to generate: smooth, line or both.
  if (plot_type == "smooth") {
    p <- p + geom_smooth(aes(group = Candidate), method = "loess")
  }
  else if (plot_type == "line") {
    p <- p + geom_line(aes(group = Candidate)) + geom_point()
  } else if (plot_type == "both") {
    p <- p + geom_smooth(aes(group = Candidate), method = "loess") +
      #geom_line(aes(group = Candidate))
      geom_point(aes(shape = Candidate))
  } else {
    stop("Incorrect plot type given: use smooth, line, or both")
  }
  
  # Saves to "www" folder as png image if folder and plot name given
  if ((plot_name != "") && (main_folder != "")) {
    ggsave(file = file.path(main_folder, "www", plot_name))
  }
  
  # Explicitly returns plot
  return(p)
}

# Creates plots for a given funding table. Wrapper for plot_funding_ggplot
plot_funding <- function(main_folder, data_folder) {
  
  # Opens funding summary files into data frames
  dem <-   read.csv(file.path(data_folder, "os_dem.tsv"), sep = "\t")
  gop <-   read.csv(file.path(data_folder, "os_gop.tsv"), sep = "\t")
  full <-  read.csv(file.path(data_folder, "os_full.tsv"), sep = "\t")
  
  # Formats data frames and plots funding for all candidates and by party
  dem_plot <- plot_funding_ggplot(format_funding(dem), main_folder, "dem_funding.png")
  gop_plot <- plot_funding_ggplot(format_funding(gop), main_folder, "gop_funding.png")
  full_plot <- plot_funding_ggplot(format_funding(full), main_folder, "full_funding.png")
}

# Plots candidates funding totals. If type is set to default, sums all values into
# a single number per candidate. Else, only displays given type of funding
plot_funding_ggplot <- function(df, main_folder = "", plot_name = "", type = "All",
                                xlab = "", ylab = "") {
  
  # Sets type of funding to display. "All" sums all values and "other" cuts 
  # non-campaign and super pac funds
  if (type == "Other") {
    df <- df[df$Type != "Campaign",]
    df <- df[df$Type != "Super PAC",]
  }
  else if (type != "All") {
    df <- df[df$Type == type,]
  }
  
  # Standard bar plot, summing all values for a given type of funding per candidate
  p <- ggplot(df, aes(x=reorder(Candidate, Total, function(x) -1*(sum(x))), y=Total)) + 
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15)) + 
    scale_y_continuous(labels = dollar) + 
    labs(x = "", y = ylab) 
  
  # Saves to "www" folder as png image if folder and plot name given
  if ((plot_name != "") && (main_folder != "")) {
    ggsave(file = file.path(main_folder, "www", plot_name))
  }
  
  # Explicitly returns plot
  return(p)
}

# Archives old poll information
archive <- function(main_folder, data_folder, shiny_folder, archive_folder) {
  
  # Gets all current files
  rcp_dem_plot <-   "rcp_dem.png"
  rcp_dem_recent <- "rcp_dem_recent.tsv"
  rcp_dem_full <-   "rcp_dem_full.tsv"
  
  rcp_gop_plot <-   "rcp_gop.png"
  rcp_gop_recent <- "rcp_gop_recent.tsv"
  rcp_gop_full <-   "rcp_gop_full.tsv"
  
  pollster_dem_plot <-  "pollster_dem.png"
  pollster_gop_plot <-  "pollster_gop.png"
  pollster_dem <-       "pollster_dem.tsv"
  pollster_gop <-       "pollster_gop.tsv"
  
  os_dem <-   "os_dem.tsv"
  os_gop <-   "os_gop.tsv"
  os_full <-  "os_full.tsv"
  
  os_dem_plot <-   "dem_funding.png"
  os_gop_plot <-   "gop_funding.png"
  os_full_plot <-  "full_funding.png"
  
  # Creates an archive folder for the current day. Overwrites if exists
  archive <- file.path(archive_folder, as.character(Sys.Date()))
  dir.create(archive, showWarnings = FALSE)
  
  # Archives current files. Overwrites if exists
  file.copy(file.path(shiny_folder, rcp_dem_plot), file.path(archive, rcp_dem_plot), overwrite = TRUE)
  file.copy(file.path(data_folder, rcp_dem_recent), file.path(archive, rcp_dem_recent), overwrite = TRUE)
  file.copy(file.path(data_folder, rcp_dem_full), file.path(archive, rcp_dem_full), overwrite = TRUE)
  
  file.copy(file.path(shiny_folder, rcp_gop_plot), file.path(archive, rcp_gop_plot), overwrite = TRUE)
  file.copy(file.path(data_folder, rcp_gop_recent), file.path(archive, rcp_gop_recent), overwrite = TRUE)
  file.copy(file.path(data_folder, rcp_gop_full), file.path(archive, rcp_gop_full), overwrite = TRUE)
  
  file.copy(file.path(shiny_folder, pollster_dem_plot), file.path(archive, pollster_dem_plot), overwrite = TRUE)
  file.copy(file.path(data_folder, pollster_gop_plot), file.path(archive, pollster_gop_plot), overwrite = TRUE)
  file.copy(file.path(data_folder, pollster_dem), file.path(archive, pollster_dem), overwrite = TRUE)
  file.copy(file.path(data_folder, pollster_gop), file.path(archive, pollster_gop), overwrite = TRUE)
  
  file.copy(file.path(data_folder, os_dem), file.path(archive, os_dem), overwrite = TRUE)
  file.copy(file.path(data_folder, os_gop), file.path(archive, os_gop), overwrite = TRUE)
  file.copy(file.path(data_folder, os_full), file.path(archive, os_full), overwrite = TRUE)
  
  file.copy(file.path(shiny_folder, os_dem_plot), file.path(archive, os_dem_plot), overwrite = TRUE)
  file.copy(file.path(shiny_folder, os_gop_plot), file.path(archive, os_gop_plot), overwrite = TRUE)
  file.copy(file.path(shiny_folder, os_full_plot), file.path(archive, os_full_plot), overwrite = TRUE)
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
  
  # Updates all polls and funding
  update_rcp(data_folder)
  update_pollster(data_folder)
  update_funding(data_folder)
  
  # Creates and saves plots for polls and funding
  plot_rcp(main_folder, data_folder)
  plot_pollster(main_folder, data_folder)
  plot_funding(main_folder, data_folder)
  
  # Archives data by current date
  invisible(archive(main_folder, data_folder, shiny_folder, archive_folder))
}

# Sample usage:
# main_folder <- file.path("C:", "Users", "Navi", "Dropbox", "Public", "Junior Year", "R", "election2016", "data")
# track(main_folder)

# Misc. code:
# file.info(file.path(main_folder, "rcp_dem_full.csv"))$mtime