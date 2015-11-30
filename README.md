# election2016

A framework with an accompanying website to track the 2016 primary and general elections. Scrapes data from Real Clear Politics, Pollster and Open Secrets to plot polling averages and total money raised. Also archives poll information on a daily basis each time the program is run.

The website can currently be found [here](https://henryw.shinyapps.io/election2016).

While the Shiny app will run without errors after downloading this repo, to run this yourself with updated data, source the get_polls.R file and call the function track([folder_name]) on a given folder name. This will scrape data from the aforementioned sites before archiving polling and funding summary tsv files, as well as ggplot graphs, for each data source. Then, you can proceed to run the Shiny app and it will display the updated data.

Created by Henry Ward and Adam Loy. Licensed under the [MIT license](https://tldrlegal.com/license/mit-license).
