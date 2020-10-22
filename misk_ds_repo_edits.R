# Title of the script
# Amjad 
# 06.10.2020
# Description of the analysis (short)

# data from: https://www.kaggle.com/uciml/student-alcohol-consumption

# load the packages
library(tidyverse)
# library(plyr)
library(gridExtra)
library(alluvial)
library(extrafont)
library(rio)
library(lubridate)
library(chron)

# import ----
report_math <- read_csv("student-mat.csv")
report_Portuguese <- read_csv("student-por.csv")

# Examine the data
glimpse(report_math)
glimpse(report_Portuguese)

# Merge the two data sets ----
alcohol <- merge(report_math,
                 report_Portuguese,
                 by = c("school","sex","age","address",
                        "famsize","Pstatus","Medu","Fedu",
                        "Mjob","Fjob","reason","nursery","internet"))


# Question 1 ----
# How is alcohol consumption and academic performance (math) correlated?
# first off, get the average form the three periods (G1, G2 , G3) for each student
# make an ID varialbe to track students
report_math_summary <- report_math %>% 
  select(Walc, Dalc, G1, G2, G3) %>% 
  mutate(ID = seq_len(nrow(report_math))) %>% 
  pivot_longer(-c(ID, Walc, Dalc)) %>% 
  group_by(ID, Walc, Dalc) %>% 
  summarise(Gavg = mean(value))

# First first off, make some plot to explore the relationships
ggplot(report_math_summary, aes(Walc, Gavg)) +
  geom_jitter(width = 0.4, alpha = 0.4) +
  geom_smooth(method = "lm") +
  ggtitle("Weekly")

ggplot(report_math_summary, aes(Dalc, Gavg)) +
  geom_jitter(width = 0.4, alpha = 0.4) +
  geom_smooth(method = "lm") +
  ggtitle("Daily")

# Whata bout each period instead of the mean of all periods
report_math_summary_period <- report_math %>% 
  select(Walc, Dalc, G1, G2, G3) %>% 
  mutate(ID = seq_len(nrow(report_math))) %>% 
  pivot_longer(-c(ID, Walc, Dalc))

# by weekly
ggplot(report_math_summary_period, aes(Walc, value, color = name)) +
  geom_jitter(width = 0.4, alpha = 0.4) +
  geom_smooth(method = "lm") +
  ggtitle("Weekly")

# by daily
ggplot(report_math_summary_period, aes(Dalc, value, color = name)) +
  geom_jitter(width = 0.4, alpha = 0.4) +
  geom_smooth(method = "lm") +
  ggtitle("Daily")

# what about individual students
report_math_summary_period %>% 
  mutate(name = parse_number(name)) %>% 
  ggplot(aes(name, value, group = ID)) +
  geom_line() +
  facet_grid(. ~ Dalc)
  
report_math_summary_period %>% 
  mutate(name = parse_number(name)) %>% 
  ggplot(aes(name, value, group = ID)) +
  geom_line() +
  facet_grid(. ~ Walc)

# mean for each Dalc
report_math_summary %>% 
  group_by(Dalc) %>% 
  summarise(Gavg = mean(Gavg),
            Grange = max(Gavg, na.rm = TRUE) - min(Gavg, na.rm = TRUE) )
# 

alcohol.3 <- alcohol.2%>%
  mutate(grade=G1.mat, Period="period1", SD = sd(G1.mat))%>%
  select(grade, Period, Dalc.x,count, SD)

alcohol.4 <- alcohol.2%>%
  mutate(grade=G2.mat, Period="period2", SD = sd(G2.mat))%>%
  select(grade, Period, Dalc.x,count, SD)

alcohol.5 <- alcohol.2%>%
  mutate(grade=G3.mat, Period="period3", SD = sd(G3.mat))%>%
  select(grade, Period, Dalc.x,count, SD)

alcohol.6 <- rbind(alcohol.3, alcohol.4, alcohol.5)%>%
  mutate(MOE = 2*SD/sqrt(count), Dalc = ifelse(Dalc.x==1, "very low", 
                                               ifelse(Dalc.x==2, "low", 
                                                      ifelse(Dalc.x==3, "moderate",
                                                             ifelse(Dalc.x==4, "high",
                                                                    "very high")))),
         
         Dalc = factor(Dalc, levels = c("very low", "low", "moderate", "high", "very high")))

limits <- aes(ymin=grade-MOE, ymax=grade+MOE)

ggplot(alcohol.6, aes(x=Dalc, y=grade, fill=Period))+
  
  geom_bar(stat = "identity", position = "dodge")+
  
  geom_errorbar(limits, position = "dodge")+
  
  labs(x = "Weekday Alcohol Consumption",
       
       y = "Grade",    
       
       title = "The average math grade for different workday drinking level over time",    
       
       caption = "Data from kaggle.com"
       
  )




