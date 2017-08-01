library(readr)
library(lubridate)
library(tidyverse)
library(readxl)
library(stringr)

ins1 <- read_csv("osha/osha_inspection-1.csv")
ins2 <- read_csv("osha/osha_inspection-2.csv")
ins3 <- read_csv("osha/osha_inspection-3.csv")
ins4 <- read_csv("osha/osha_inspection-4.csv")
ins5 <- read_csv("osha/osha_inspection-5.csv")

ins <- rbind(ins1, ins2, ins3, ins4, ins5)
rm(ins1)
rm(ins2)
rm(ins3)
rm(ins4)
rm(ins5)


viol1 <- read_csv("osha/osha_violation-1.csv")
viol2 <- read_csv("osha/osha_violation-2.csv")
viol3 <- read_csv("osha/osha_violation-3.csv")
viol4 <- read_csv("osha/osha_violation-4.csv")
viol5 <- read_csv("osha/osha_violation-5.csv")
viol6 <- read_csv("osha/osha_violation-6.csv")
viol7 <- read_csv("osha/osha_violation-7.csv")
viol8 <- read_csv("osha/osha_violation-8.csv")
viol9 <- read_csv("osha/osha_violation-9.csv")
viol10 <- read_csv("osha/osha_violation-10.csv")
viol11 <- read_csv("osha/osha_violation-11.csv")
viol12 <- read_csv("osha/osha_violation-12.csv")

viol <- rbind(viol1, viol2, viol3, viol4, viol5, viol6, viol7, viol8, viol9, viol10, viol11, viol12)
rm(viol1)
rm(viol2)
rm(viol3)
rm(viol4)
rm(viol5)
rm(viol6)
rm(viol7)
rm(viol8)
rm(viol9)
rm(viol10)
rm(viol11)
rm(viol12)

mega <- left_join(viol, ins, by="activity_nr")

naics <- read_excel("2017_NAICS_Descriptions.xlsx")
naics$Title <- ifelse(str_sub(naics$Title, start=-1)=="T", substr(naics$Title, 1, nchar(naics$Title)-1) , naics$Title)

mega$sector <- substr(mega$naics_code, 1,2)

mega <- left_join(mega, naics, by=c("naics_code"="Code"))

sectors <- read.csv("osha/sectors.csv", stringsAsFactors=F)
sectors$Code <- as.character(sectors$Code)

mega <- left_join(mega, sectors, by=c("sector"="Code"))

mega$open_year <- year(ymd(mega$open_date))
mega$open_month <- month(ymd(mega$open_date), label=T)

industry <- mega %>% 
  group_by(open_year, open_month, Sector_Title) %>% 
  summarize(inspections_opened=n()) %>%
  filter(!is.na(open_month))


industry <- data.frame(industry)


ggplot(industry, aes(x=open_month, y=inspections_opened, group=Sector_Title, color=Sector_Title, fill=Sector_Title)) +
  geom_bar(stat="identity") +
  facet_wrap(~open_year, ncol=4)

industry_2003 <- filter(mega, open_year>=2013) %>% 
  group_by(open_year, open_month, Sector_Title) %>% 
  summarize(inspections_opened=n()) %>%
  filter(!is.na(open_month))

ggplot(industry_2003, aes(x=open_month, y=inspections_opened, group=Sector_Title, color=Sector_Title, fill=Sector_Title)) +
  geom_bar(stat="identity") +
  facet_wrap(~open_year, ncol=1)

industry_2017 <- filter(mega, open_year>=2017) %>% 
  group_by(open_year, open_month, site_state) %>% 
  summarize(inspections_opened=n()) %>%
  filter(!is.na(open_month))

the_chart <- ggplot(industry_2017, aes(x=open_month, y=inspections_opened, group=site_state)) 
the_chart <- the_chart + geom_line(stat="identity")
the_chart <- the_chart + facet_wrap(~site_state, ncol=5, scales = "free")
the_chart <- the_chart + labs(x=NULL, y="Inspections opened", title="OSHA inspections in 2017", caption="Source: OSHA")
the_chart


industry_2016 <- filter(mega, open_year>=2016) %>% 
  group_by(open_year, open_month, site_state) %>% 
  summarize(inspections_opened=n()) %>%
  filter(!is.na(open_month))
industry_2016$year <- as.factor(industry_2016$open_year)

the_chart <- ggplot(industry_2016, aes(x=open_month, y=inspections_opened, group=year, color=year)) 
the_chart <- the_chart + geom_line(stat="identity")
the_chart <- the_chart + facet_wrap(~site_state, ncol=4, scales = "free")
the_chart <- the_chart + labs(x=NULL, y="Inspections opened", title="OSHA inspections in 2016 and 2017", caption="Source: OSHA")
the_chart