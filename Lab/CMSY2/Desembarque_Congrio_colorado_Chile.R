### Desembarque de congrio colorado en Chile
# Periodo 1957-2021
# 

# Carga de packages -------------------------------------------------------
library(ggplot2)
library(ggpubr)

# Datos -------------------------------------------------------------------
dt <- read.csv("Datos/congriocol.csv",sep=";")
names(dt)


# Grafico del desembarque -------------------------------------------------

p1 <- ggplot(data=dt,aes(x=YY, y=Desembarque))+
  geom_line()+
  geom_point()+
  #geom_bar(position = "dodge",stat="identity",width = 0.8,color="black") +
  #scale_fill_manual(values = c("#6C8EBF","#FFFFFF","orange","#74767a","red","grey70"))+
  #scale_fill_manual(values = colorRampPalette(c("#FFFFFF","grey70","grey10"))(4))+
  labs(x="Año", y="Desembarque (toneladas)")+
  scale_x_continuous(limits = c(1957.5,2021.5))+
  labs(title = "Desembarque nacional",
       subtitle = "Congrio colorado",
       caption = "Fuente: Sernapesca")+
   theme_bw(14)
p1
ggsave("Figuras_ocom/Fig1_desembarque_congrio_colorado.png",p1)



