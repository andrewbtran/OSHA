library(readr)
library(lubridate)
library(ggplot2)
library(ggjoy)

osha1 <- read_csv("osha/osha_inspection-1.csv")
osha2 <- read_csv("osha/osha_inspection-2.csv")
osha3 <- read_csv("osha/osha_inspection-3.csv")
osha4 <- read_csv("osha/osha_inspection-4.csv")
osha5 <- read_csv("osha/osha_inspection-5.csv")

osha2 <- rbind(osha1, osha2, osha3, osha4, osha5)
rm(osha1)
rm(osha2)
rm(osha3)
rm(osha4)
rm(osha5)

osha$open_year <- year(ymd(osha$open_date))
osha$open_month <- month(ymd(osha$open_date), label=T)

osha_years <- osha %>% 
  group_by(open_year, open_month) %>% 
  summarize(inspections_opened=n()) %>%
  filter(!is.na(open_month))

osha_years <- data.frame(osha_years)
osha_years$open_year2 <- as.factor(osha_years$open_year)

osha$close_year <- year(ymd(osha$close_case_date))
osha$close_month <- month(ymd(osha$close_case_date), label=T)

osha_years2 <- osha %>% 
  group_by(close_year, close_month) %>% 
  summarize(inspections_closed=n()) %>%
  filter(!is.na(close_month))

osha_years2 <- data.frame(osha_years2)
osha_years2$close_year2 <- as.factor(osha_years2$close_year)

osha_years$join <- paste(osha_years$open_year, osha_years$open_month)
osha_years2$join <- paste(osha_years2$close_year, osha_years2$close_month)

osha_years <- full_join(osha_years, osha_years2, by="join")
osha_years <- filter(osha_years, !is.na(open_year2))

osha_split2 <- osha_years[,6:9]
osha_split2 <- filter(osha_split2, !is.na(close_year2))
colnames(osha_split2) <- c("Yr", "Month", "Inspections", "Year")
osha_split2$Type <- "Closed"

osha_split1 <- osha_years[,1:4]
colnames(osha_split1) <- c("Yr", "Month", "Inspections", "Year")
osha_split1$Type <- "Opened"

osha_split <- rbind(osha_split1, osha_split2)

ggplot(osha_years, aes(x=open_month, y=open_year2, group=open_year2,height=inspections)) +
  geom_joy(stat="identity")

ggplot(data=osha_years, aes(x=open_month, y=inspections, group=open_year2)) +
  geom_line(stat="identity") +
  facet_wrap(~open_year2, ncol=4)

osha_split_nomar <- filter(osha_split, Year!=1984 & Month!="March")

ggplot(data=osha_split_nomar, aes(x=Month, y=Inspections, group=Type, color=Type)) +
  geom_line(stat="identity") +
  facet_wrap(~Year, ncol=4)


###

osha1 <- read_csv("osha/osha_violation_event-1.csv")
osha2 <- read_csv("osha/osha_violation_event-2.csv")
osha3 <- read_csv("osha/osha_violation_event-3.csv")
osha4 <- read_csv("osha/osha_violation_event-4.csv")
osha5 <- read_csv("osha/osha_violation_event-5.csv")
osha6 <- read_csv("osha/osha_violation_event-6.csv")
osha7 <- read_csv("osha/osha_violation_event-7.csv")
osha8 <- read_csv("osha/osha_violation_event-8.csv")
osha9 <- read_csv("osha/osha_violation_event-9.csv")
osha10 <- read_csv("osha/osha_violation_event-10.csv")

osha_3 <- rbind(osha1, osha2, osha3, osha4, osha5, osha6, osha7, osha8, osha9, osha10)
rm(osha1)
rm(osha2)
rm(osha3)
rm(osha4)
rm(osha5)
rm(osha6)
rm(osha7)
rm(osha8)
rm(osha9)
rm(osha10)