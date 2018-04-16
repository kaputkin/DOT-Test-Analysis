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
  geom_point(size=5)+
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

grp_freqtable$scale[grp_freqtable$grp=='0F'] <- 1
grp_freqtable$scale[grp_freqtable$grp=='1F'] <- 2
grp_freqtable$scale[grp_freqtable$grp=='2F'] <- 3
grp_freqtable$scale[grp_freqtable$grp=='3F'] <- 4
grp_freqtable$scale[grp_freqtable$grp=='4F'] <- 5
grp_freqtable$scale[grp_freqtable$grp=='5F'] <- 6
grp_freqtable$scale[grp_freqtable$grp=='6F'] <- 7
grp_freqtable$scale[grp_freqtable$grp=='7F'] <- 8
grp_freqtable$scale[grp_freqtable$grp=='8F'] <- 9
grp_freqtable$scale[grp_freqtable$grp=='9F'] <- 10
grp_freqtable$scale[grp_freqtable$grp=='0M'] <- 11
grp_freqtable$scale[grp_freqtable$grp=='1M'] <- 12
grp_freqtable$scale[grp_freqtable$grp=='2M'] <- 13
grp_freqtable$scale[grp_freqtable$grp=='3M'] <- 14
grp_freqtable$scale[grp_freqtable$grp=='4M'] <- 15
grp_freqtable$scale[grp_freqtable$grp=='5M'] <- 16
grp_freqtable$scale[grp_freqtable$grp=='6M'] <- 17
grp_freqtable$scale[grp_freqtable$grp=='7M'] <- 18
grp_freqtable$scale[grp_freqtable$grp=='8M'] <- 19
grp_freqtable$scale[grp_freqtable$grp=='9M'] <- 20


#add gender to age buckets
data$grp <- paste(data$agegrp,data$SEX,sep= "")
grp_freqtable<- arrange(grp_freqtable, scale)

#Age Buckets Freq Table
grp_freqtable <- xtabs(~ CASE_YR+ grp, data=data)
grp_freqtable <- as.data.frame(grp_freqtable)
write.csv(grp_freqtable, "grp_freqtable.csv")
grp_freqtable <- read.csv("grp_freqtable.csv")

#stacked age bucket bar chart
cols <- c('1' = "#fcfbfd", '2' = "#efedf5", '3 '= "#dadaeb", '4' ="#bcbddc",' 5' ="#9e9ac8", '6 '= "#807dba", '7' = "#6a51a3", '8' ="#54278f", '9' = "#3f007d", '10' = "black", '11' = "#fff5eb", '12' = "#fee6ce",'13' = "#fdd0a2", '14' = "#fdae6b", '15' = "#fd8d3c", '16' = "#f16913", '17' = "#d94801", '18' = "#a63603", '19' = "#7f2704", '20' = "black")

ggplot(data = grp_freqtable, aes(x = CASE_YR, y = Freq, fill = scale)) +
  geom_bar(stat = "Identity") +
  scale_fill_manual(values = cols)

## ^^ https://stackoverflow.com/questions/17331892/order-and-color-of-bars-in-ggplot2-barplot
