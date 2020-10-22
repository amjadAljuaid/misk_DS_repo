#repo

# sales report 
# amjad 
# 06.10.2020

library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(alluvial)
library(extrafont)
library(tidyverse)
library(rio)
library(lubridate)
library(chron)
getwd()
setwd('/Users/eng.jodi/Desktop/misk_DS_repo/misk_DS_repo')

report_math <- read_csv("student-mat.csv")
report_Portuguese <- read_csv("student-por.csv")


  
  
  #Question 1 : How alcohol consumption is correlated to academic performance.
  alcohol <-merge(report_math,report_Portuguese,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
  
  alcohol.1 <- alcohol %>% 
    select(Walc.x, Dalc.x,G1.x,G1.y,G2.x, G2.y,G3.x, G3.y)
  
  alcohol.1 <- alcohol.1%>% 
    mutate(avg.mat=(G1.x+G2.x+G3.x)/3, 
           avg.por =(G1.y+G2.y+G3.y)/3, 
           diff.mat = G3.x-G1.x, 
           diff.por = G3.y-G1.y)
  
  alcohol.2 <- alcohol.1%>%
     group_by(Dalc.x)%>%
    summarize(grade.mat=mean(avg.mat), 
              grade.por=mean(avg.por),
              G1.mat=mean(G1.x),
              G2.mat=mean(G2.x),
              G3.mat=mean(G3.x),
              count=n())
  
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
  
  

  
  