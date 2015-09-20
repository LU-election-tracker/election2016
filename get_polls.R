library(rvest)

# Scrapes a given Real Clear Politics page and writes updated csv files with
# most recent data and complete poll data
scrape_rcp <- function(rcp_address, recent_out, full_out) {
  rcp <- html(rcp_address)
  recent <- rcp %>% html_nodes(".data") %>% .[[1]] %>% html_table()
  full <- rcp %>% html_nodes(".data") %>% .[[2]] %>% html_table()
  write.table(recent, recent_out, quote = FALSE)
  write.table(full, full_out, quote = FALSE)
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
             file.path(folder, "rcp_gop_full.csv"))
}

# Main update function. Updates polling data and graphs.
track <- function() {
  folder <- file.path("C:", "Users", "Navi", "Dropbox", "Public", "Junior Year", 
                      "R", "election2016", "Poll_data")
  if(dir.exists(folder) == FALSE) {
    dir.create(folder)
  }
  update_rcp(folder)
}