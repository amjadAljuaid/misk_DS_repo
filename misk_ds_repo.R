#repo

# sales report 
# amjad 
# 06.10.2020


library(tidyverse)
library(rio)
library(lubridate)
library(chron)

prod_report2 <- read_csv("for learning.csv")
getwd()
summary(prod_report2)
glimpse(prod_report2)

#highest premium

prod_report2 %>% 
  filter(Status == "Issued" ) %>%
  group_by(Status,Make,Model) %>%
  summarise(`Gross Premium`) %>% 
  arrange(desc(`Gross Premium`)) 

# Top models purchased----

prod_report2 %>%
  na.omit(prod_report2) %>%
  filter(Status == "Issued" ) %>%
  group_by(Status,Make) %>%
  count(Model) %>%
  arrange(desc(`n`))

# top models quoted but not purchased

prod_report2 %>%
  filter(Status == "Pending" ) %>%
  group_by(Status,Make) %>%
  count(Model) %>%
  arrange(desc(`n`))


# female , male ratio with plot


prod_report2 %>% 
  na.omit(prod_report2) %>%
  select(Gender,`Issued On`) %>%
  separate(`Issued On`, into = c("Date", "Time"),
           sep = " ", remove = FALSE) %>%
  mutate(Date = lubridate::as_date(Date, format = "%d-%m-%Y"),
         Time = hms::as_hms(str_c(Time, ":00"))) %>%
  mutate(date = ymd(Date)) %>% 
  mutate_at(vars(date), funs(year, month, day)) %>%
  arrange(-desc(`month`)) %>%
  select(Gender, month) %>%
  group_by(month) %>%
  count(Gender) %>%
  ggplot(aes( month, n, color=Gender)) +
  scale_x_log10() +
  geom_point()

