setwd("C:/Users/alk511/Documents/GitHub/DOT-Test-Analysis")
total_crash <- read.csv("Total crashes.csv")
total_crash <- total_crash[1:3]

total_crash$color[total_crash$grp == '0F'] = '#FFFF7F'
total_crash$color[total_crash$grp == '1F'] = '#E6FF87'
total_crash$color[total_crash$grp == '2F'] = '#CDFF8F'
total_crash$color[total_crash$grp == '3F'] = '#B5FF97'
total_crash$color[total_crash$grp == '4F'] = '#9CFF9F'
total_crash$color[total_crash$grp == '5F'] = '#83FFA7'
total_crash$color[total_crash$grp == '6F'] = '#6BFFAF'
total_crash$color[total_crash$grp == '7F'] = '#52FFB7'
total_crash$color[total_crash$grp == '8F'] = '#3AFFC0'

ggplot(data=total_crash, aes(x=year, y=Freq, color=grp)) +
  geom_line(size= 4)+
  geom_point(size = 6)+
  scale_color_manual(values = c('#662506','#4d004b','#993404','#810f7c','#cc4c02','#88419d','#ec7014','#8c6bb1','#fe9929','#8c96c6','#fec44f','#9ebcda','#fee391','#bfd3e6','#fff7bc','#e0ecf4','#ffffe5','#f7fcfd'))+
  geom_area(data = year_freqtable, aes(x=CASE_YR, y= Freq, fill = SEX) +
  theme(
    panel.background = element_rect(fill="white"),
    panel.grid.minor.x = element_line(size= 1, colour = "#515151"),
    panel.grid.major = element_line(colour = "#515151", size = 1),
    plot.background = element_rect(fill="white")
  ) 



library(ggplot2)
ggplot(data = year_freqtable, aes(x = CASE_YR, y = Freq, fill = AGE, color= AGE,group=AGE)) + 
  geom_area(alpha=0.8) +
  scale_fill_manual(values=c("#D68028", "#864D9F")) +
  theme(
    panel.background = element_rect(fill="white"),
    panel.grid.minor.x = element_line(size= 1, colour = "#515151"),
    panel.grid.major = element_line(colour = "#515151", size = 1),
    plot.background = element_rect(fill="white")
  ) 
