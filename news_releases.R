library(rvest)

url <- "https://www.osha.gov/news/newsreleases/all"

html <- read_html(url)

dates <- html_nodes(html, "time")

titles <- html_nodes(html, "u")

all <- html_nodes(html, ".field-content")



