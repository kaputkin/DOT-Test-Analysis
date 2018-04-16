setwd("/Users/mac/desktop/DOT/Test")
data <- read.csv("bike_year_age_gender.csv")

#conver data types
sapply(data, class)
data$AGE <-  as.integer(data$AGE)

#create sex freq tables
  sex_freqtable <- xtabs(~ AGE + SEX, data=data)
  sex_freqtable <- as.data.frame(sex_freqtable)
   
    library(dplyr)
    sex_freqtable <- sex_freqtable %>% filter(Freq != 0)
    sex_freqtable <- sex_freqtable %>% filter(SEX != '')
    
#create sex freq tables
  year_freqtable <- xtabs(~ CASE_YR+ SEX, data=data)
  year_freqtable <- as.data.frame(year_freqtable)
  
    library(dplyr)
    year_freqtable <- year_freqtable %>% filter(Freq != 0)
    year_freqtable <- year_freqtable %>% filter(SEX != ' ')    
    
    
#create contour freq table
  contour_freqtable <- xtabs(~ AGE + SEX + CASE_YR, data=data)
  contour_freqtable <- as.data.frame(contour_freqtable)

    library(dplyr)
    contour_freqtable <- contour_freqtable %>% filter(Freq != 0)
    contour_freqtable <- contour_freqtable %>% filter(SEX != '')
    contour_freqtable <- contour_freqtable %>% filter(SEX != ' ')
    
##PLOTS##    
#plot age and sex
library(ggplot2)
ggplot(sex_freqtable,aes(x=AGE,y=Freq,color=SEX,group=SEX))+
  geom_point(size=.5)+
  geom_line()

#plot contour
require(reshape2)
countour_freqtable.m <- melt(contour_freqtable)

#plot year charts
library(ggplot2)
ggplot(data = year_freqtable, aes(x = CASE_YR, y = Freq, fill = SEX, color= SEX,group=SEX)) + 
  #geom_bar(stat = "Identity")
  geom_point(size=.5)+
  geom_line()
