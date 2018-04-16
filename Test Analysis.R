setwd("/Users/mac/desktop/DOT/Test")
setwd("C:/Users/alk511/Documents/GitHub/DOT-Test-Analysis")

data <- read.csv("bike_year_age_gender.csv")

#conver data types
sapply(grp_freqtable, class)
data$AGE <-  as.integer(data$AGE)

#create sex freq tables
  sex_freqtable <- xtabs(~ AGE + SEX, data=data)
  sex_freqtable <- as.data.frame(sex_freqtable)
   
    library(dplyr)
    sex_freqtable <- sex_freqtable %>% filter(Freq != 0)
    sex_freqtable <- sex_freqtable %>% filter(SEX != ' ')
    
#create year freq tables
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
  geom_point(size=1)+
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

#summary for age
library(dplyr)
age_summary<- data %>% group_by(SEX, CASE_YR) %>% summarize(AGE=median(AGE))

#age buckets
data$agegrp[data$AGE>=0 & data$AGE<=9]   <- 0
data$agegrp[data$AGE>=10 & data$AGE<=19] <- 1
data$agegrp[data$AGE>=20 & data$AGE<=29] <- 2
data$agegrp[data$AGE>=30 & data$AGE<=39] <- 3
data$agegrp[data$AGE>=40 & data$AGE<=49] <- 4
data$agegrp[data$AGE>=50 & data$AGE<=59] <- 5
data$agegrp[data$AGE>=60 & data$AGE<=69] <- 6
data$agegrp[data$AGE>=70 & data$AGE<=79] <- 7
data$agegrp[data$AGE>=80 & data$AGE<=89] <- 8
data$agegrp[data$AGE>=90 & data$AGE<=99] <- 9

#add gender to age buckets
data$grp <- paste(data$agegrp,data$SEX,sep= "")


#Age Buckets Freq Table
grp_freqtable <- xtabs(~ CASE_YR+ grp, data=data)
grp_freqtable <- as.data.frame(grp_freqtable)
write.csv(grp_freqtable, "grp_freqtable.csv")
grp_freqtable <- read.csv("grp_freqtable.csv")

#stacked age bucket bar chart
ggplot(data = grp_freqtable, aes(x = CASE_YR, y = Freq, fill = grp)) + 
  geom_bar(stat = "Identity")

grp_freqtable$grp <- factor(grp_freqtable$grp,levels(grp_freqtable$grp)[c("0F","1F","2F","3F","4F","5F","6F","7F","8F","9F","0M","1M","2M","3M","4M","5M","6M","7M","8M","9M")])

