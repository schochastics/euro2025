# 1. load required packages
library(rvest)
library(dplyr)
library(stringr)

# 2. point to the Wikipedia page
url <- "https://en.wikipedia.org/wiki/St._Jakob-Park"

# 3. read the HTML
page <- read_html(url)

labels <- html_elements(page, ".infobox-label") |> 
  html_text() |> 
  str_trim() |> 
  str_to_lower()

values <- html_elements(page, ".infobox-data") |> 
  html_text() |> 
  str_trim() |> 
  str_remove_all("\\[.*?\\]")

page |> html_element(".geo-nondefault") |> 
  html_element(".geo") |> html_text()
