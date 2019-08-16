# Scraping Next Gen Stats Public Data Week by Week with RSelenium

Inspired by **Ben Baldwin's** ([@benbbaldwin](https://twitter.com/benbbaldwin)) nflscrapR ([tutorial](https://gist.github.com/guga31bb/5634562c5a2a7b1e9961ac9b6c568701)) on Github, I wanted to publish some tutorials on web-scraping to get data from other sources.  I recently learned RSelenium through adapting **Sean Clement's** ([@SeanfromSeabeck](https://twitter.com/SeanfromSeabeck)) code when we worked together.  It is an amazing tool, which allows you to open a web browser and navigate through webpages all through your code.  For someone that has spent way too much time in his life mindlessly copying & pasting or downloading pages after page of data tables from the internet, the automation has been a game changer.

I'm far from an expert in RSelenium, and welcome any comments on how to improve my code.  But I've been able to develop proficiency with the package that has provided me with the ability to scrape big databases online.  I suggest following along with this [RSelenium Baics Primer](https://rpubs.com/johndharrison/RSelenium-Basics).

The [Next Gen Stats public database](https://nextgenstats.nfl.com/stats/passing#yards) doesn't include that much, but the ability to view stats by week allows us to manipulate data and obtain datasets that are not available directly from the website (such as team defense statistics).  Luckily the urls for each page are very easily constructed so we won't have to use xpath and css selectors to actually interact with the webpage. I still include a quick example so you can see for yourself how this package allows you to control a brwoser.

-- Keegan Abdoo, [@KeeganAbdoo](https://twitter.com/KeeganAbdoo)

## Set Working Directory and Load Packages

The first thing I do in any R Script is set my working directory.  This is simply a folder on your computer, and I find it helpful with organization.  In more technical terms, it is the enviroment that the objects you create are stored during your R session.

The syntax here is setwd(filepath).  You can find the file path for your desired folder by right clicking on it and copying the address (at least on windows). 

``` r  
setwd("C:/.../Next Gen Public Site")
```
If you haven't already installed any of these packages on your computer, you will need to run the R function install.packages("packagename") before you can load them from your library.

``` r  
library(RSelenium)
library(XML)
library(tidyverse)
library(purrr)
```
I also like to clear my working directory before I get into coding.

``` r  
rm(list = ls())
```

## Function to Construct url Vector

First I create a function that constructs urls for a given stat category up to the most recent week in the current season.  The user will have to supply the most recent week, (although it is possible to dynamically code this, it would require unneccessary complexity).  

The most recent week parameter is preset to 0, so if you don't specify a week, the urls will only include data from previous seasons.  For example, look at the last few rows of the vector produced by the function passing in "passing" as a category, and then see how it compares when we specify that week 6 was the most recent week

```r
create_urls <- function(category, most_recent_week = 0){
    scaffold <- tibble(szn = list(c(2016:year(Sys.time())))) %>%
        unnest() %>%
        mutate(wk = list(1:17)) %>%
        unnest() %>%
        mutate(url = glue("https://nextgenstats.nfl.com/stats/{category}/{szn}/{wk}/")) %>%
        filter(if_else(szn == year(Sys.time()) & wk > most_recent_week, FALSE, TRUE))
    return(scaffold$url)
}

tail(create_urls("passing"))

https://nextgenstats.nfl.com/stats/passing/2018/12/
https://nextgenstats.nfl.com/stats/passing/2018/13/
https://nextgenstats.nfl.com/stats/passing/2018/14/
https://nextgenstats.nfl.com/stats/passing/2018/15/
https://nextgenstats.nfl.com/stats/passing/2018/16/
https://nextgenstats.nfl.com/stats/passing/2018/17/

tail(create_urls("passing", 6))

https://nextgenstats.nfl.com/stats/passing/2018/17/
https://nextgenstats.nfl.com/stats/passing/2019/1/
https://nextgenstats.nfl.com/stats/passing/2019/2/
https://nextgenstats.nfl.com/stats/passing/2019/3/
https://nextgenstats.nfl.com/stats/passing/2019/4/
https://nextgenstats.nfl.com/stats/passing/2019/5/
```

## Remote Drivers and WebElements

Before we create our scraping function, we will open the Remote Driver to explain what's going on within the function.

The function rsDriver opens up a specified browser on your computer (I use firefox here) and starts a selenium server.  It returns a list (which we name "driver") that contains the server and a client.  We can then isolate the client and create a object of class remoteDriver (which we name "remDr"). Oversimplifying this, the remoteDriver allows us to interact with and control the browser from our R console.

```r
driver <- rsDriver(browser = c("firefox"))
remDr <- driver[["client"]]
```
The first step of our function will be to navigate to the supplied url.  It does this by using a method from the remoteDriver object.  Methods are basically the functions that you attach (with a "$") to a remoteDriver to interact with the browser. They include being able to navigate to specific urls, move our mouse to a specified location, screenshot the page, change the window size, etc.  

The most useful methods in my experience, are those that allow you to interact with elements on the page such as filters and search boxes.  Elements are basically just different HTML objects, and we can search for them specifically using xpaths within a findElement() method (here's a great [tutorial for using xpath in a Selenium](https://www.guru99.com/xpath-selenium.html)). What is returned is an object of class webElement.

webElement objects, like remoteDriver objects, can be appended by methods. The webElement methods can be used to scrape data (i.e. getElementAttribute()) and also to click and select on filters. For instance, see how this method clicks on the webElement for the dropdown menu for seasons, expanding it on the Next Gen Stats Passing table. 

```r
remDr$navigate(https://nextgenstats.nfl.com/stats/passing/2018)
remDr$findElement("xpath", "//label[text() = 'Season']//following::i")$clickElement()
```
Again, we don't need to use the filters to navigate through the pages because the urls are very simple, but on other websites it becomes crucial, especially when the data isn't stored in an HTMLtable.

To close a Selenium browser, simply use the close method on the remoteDriver, and then stop the server with a method on the driver.

```r
remDr$close()
driver[["server"]]$stop()
```

## Create our Scraping Function

```r
scrape_leaderboard <- function(webpage){
    # Nagivate to page
    remDr$navigate(webpage)
    # Wait three seconds for webpage to load
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
```
