# Set Working Directory
setwd("C:/.../Next Gen Public Site")

library(RSelenium)
library(XML)
library(tidyverse)
library(glue)
library(lubridate)
library(purrr)

# Clear Environment
rm(list = ls())

# Create function that dynamically creates a vector of urls based on the most recently completed week of
# the current season and the next gen stats category (passing, rushing, receiving)
create_urls <- function(category, most_recent_week = 0){
    scaffold <- tibble(szn = list(c(2016:year(Sys.time())))) %>%
        unnest() %>%
        mutate(wk = list(1:17)) %>%
        unnest() %>%
        mutate(url = glue("https://nextgenstats.nfl.com/stats/{category}/{szn}/{wk}/")) %>%
        filter(if_else(szn == year(Sys.time()) & wk >= most_recent_week, FALSE, TRUE))
    return(scaffold$url)
}

# Create function that reads in a url of a next gen stats leaderboard and scrapes the corresponding table
scrape_leaderboard <- function(webpage){
    # Nagivate to page
    remDr$navigate(webpage)
    # Wait five seconds for webpage to load
    Sys.sleep(3)
    # Parse the table so we can read it in as an HTML table
    doc <- htmlParse(remDr$getPageSource()[[1]])
    # Save dataframe of table
    df <- readHTMLTable(doc)[[2]]
    # Clean column names from first element of list and assign them to the df
    coln <- colnames(readHTMLTable(doc)[[1]])[1:ncol(df)]
    coln <- str_extract(coln, "^(([A-z][A-Z]+%?)|(\\+/-)|(8\\+D%))(?=([A-Z][a-z].+)|\\b|8|%)")
    colnames(df) <- coln
    # Add columns for Week and Season
    df <- df %>%
        mutate(Season = scaffold[scaffold$url == webpage,]$szn,
               Week = scaffold[scaffold$url == webpage,]$wk)
    return(df)
}

# Open webpage
driver <- rsDriver(browser = c("firefox"))
remDr <- driver[["client"]]
# Run passing, rushing, and receiving url vectors through the scrape_leaderboard function
passing <- map_dfr(create_urls("passing"), scrape_leaderboard)
rushing <- map_dfr(create_urls("rushing"), scrape_leaderboard)
receiving <- map_dfr(create_urls("receiving"), scrape_leaderboard)
#Close when finished
remDr$close()
driver[["server"]]$stop()

write_csv(passing, "NGS Public Passing by Week.csv")
write_csv(rushing, "NGS Public Rushing by Week.csv")
write_csv(receiving, "NGS Public Receiving by Week.csv")
