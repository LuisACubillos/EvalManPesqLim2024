# Clasificación del desembarque
library(ggplot2)
library(dplyr)

# Datos -------------------------------------------------------------------
dt <- read.csv("Lab/Data/congriocol.csv",sep=";")
glimpse(dt)
range(dt$YY)

colnames(dt) <- c("Species","Year","Landing")

p2 <- ggplot(data=dt,aes(x=Year,y=Landing/max(Landing)))+
  geom_rect(xmin = 1954,
            xmax = 1970.5,
            ymin = 1, ymax = 1.1,
            fill = "green", alpha = 0.01)+
  geom_rect(xmin = 1970.5,
            xmax = 2001.5,
            ymin = 1, ymax = 1.1,
            fill = "yellow", alpha = 0.01)+
  geom_rect(xmin = 2001.5,
            xmax = 2003.5,
            ymin = 1, ymax = 1.1,
            fill = "red", alpha = 0.01)+
  geom_rect(xmin = 2003.5,
            xmax = 2019.5,
            ymin = 1, ymax = 1.1,
            fill = "yellow", alpha = 0.01)+
  geom_rect(xmin = 2019.5,
            xmax = 2021.5,
            ymin = 1, ymax = 1.1,
            fill = "red", alpha = 0.01)+
  
  geom_text(aes(x=1962,y=1.05,label="Sustainable"))+
  geom_text(aes(x=1984,y=1.05,label="Overfished"))+
  
  geom_hline(yintercept = 0.5,linetype=3)+
  geom_hline(yintercept = 0.1,linetype=3)+
  geom_point()+
  geom_line()+
  scale_y_continuous(breaks = c(0.1,0.5,1),limits = c(0,1.1))+
  ylab("C/Cmax")+
  theme_bw(14)
p2




