library(rvest)

query <- "342479797"


session <- html_session("https://www.osha.gov/pls/imis/InspectionNr.html")
form <- html_form(session[[1]])

insp_list <- ins$activity_nr
insp_list <- data.frame(insp_list, file="")

base_url <- "https://www.osha.gov/pls/imis/establishment.inspection_detail?id="

for (i in 1:nrow(insp_list)) {
  print(i)
  
  link_dl <- paste0
  
}


.table-borderedu strong