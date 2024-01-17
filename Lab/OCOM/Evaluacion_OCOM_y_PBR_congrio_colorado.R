#################################################
# Evaluacion de congrio colorado y estimación de
# puntos biológicos de referencia Bmsy, Blim y Fmsy
# 
# Método: Only-catch model
# Modelo: OCOM
# m1 : OCOM con Mortalidad natural = 0.25


# Carga de paquetes necesarios --------------------------------------------
rm(list = ls())
# 1) Paquetes para los modelos de evaluación
#install.packages(devtools)
#devtools::install_github("cfree14/datalimited2")
library(datalimited2)

# 2) Paquetes para graficar
library(ggplot2)
library(ggpubr)


# Datos -------------------------------------------------------------------
dt <- read.csv("Datos/congriocol.csv",sep=";")
# Prepara los datos en objetos separados
ct <- dt$Desembarque
yr <- dt$YY #años
range(yr)

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



# Evaluación modelo m1  -------------------------------
m1 = ocom(year=yr, catch=ct, m=0.25)
#Resumen parámetros
m1$ref_pts
# Parámetros poblacionales
m1$ref_pts[1:2,4]
r = m1$ref_pts[1,4]
k = m1$ref_pts[2,4]

# Puntos biológicos de referencia -----------------------------------------
bmsy = m1$ref_pts[4,4]
fmsy = m1$ref_pts[5,4]
blim = 0.5*bmsy
msy  = m1$ref_pts[3,4]
b1 = m1$ref_ts$b[1]

mitabla <- data.frame(r=r,k=k,bmsy=bmsy,blim=blim,fmsy=fmsy,msy=msy,b1=b1)
mitabla

# Graficos: series de tiempo ----------------------------------------------
dfbiom <- data.frame(yr=yr,  ct=m1$ref_ts$catch,Bt=m1$ref_ts$b,Li=m1$ref_ts$b_lo,Ls=m1$ref_ts$b_hi)
fig4a <- ggplot(data=dfbiom,aes(x=yr,y=Bt))+
  geom_ribbon(aes(ymin=Li,ymax=Ls),fill="grey70")+
  geom_line()+
  geom_hline(yintercept = bmsy,linetype="dashed")+
  geom_hline(yintercept = bmsy/2,linetype=3)+
  #geom_line(aes(x=yr,y=ct),linetype=4)+
  scale_y_continuous(name = "Biomasa (toneladas)",limits = c(0,30000))+
  theme_bw(14)+xlab("Year")
fig4a

dfb2bmsy <- data.frame(yr=yr,B_Bmsy=m1$ref_ts$bbmsy,Li=m1$ref_ts$bbmsy_lo,Ls=m1$ref_ts$bbmsy_hi)
fig4b <- ggplot(data=dfb2bmsy,aes(x=yr,y=B_Bmsy))+
  geom_ribbon(aes(ymin=Li,ymax=Ls),fill="grey70")+
  geom_line()+
  geom_hline(yintercept = 1,linetype="dashed")+
  geom_hline(yintercept = 0.5,linetype=3)+
  scale_y_continuous(name = "B/Bmsy",limits = c(0,2))+
  theme_bw(14)+xlab("Year")
fig4b

dfFt <- data.frame(yr=yr,Ft=m1$ref_ts$f,Li=m1$ref_ts$f_lo,Ls=m1$ref_ts$f_hi)
fig4c <- ggplot(data=dfFt,aes(x=yr,y=Ft))+
  geom_ribbon(aes(ymin=Li,ymax=Ls),fill="grey70")+
  geom_line()+
  geom_hline(yintercept = fmsy,linetype="dashed")+
  #geom_hline(yintercept = 0.5,linetype=3)+
  scale_y_continuous(name = "Mort. Pesca (F)",limits = c(0,0.6))+
  theme_bw(14)+xlab("Year")
fig4c

dfF_fmsy <- data.frame(yr=yr,F_Fmsy=m1$ref_ts$ffmsy,Li=m1$ref_ts$ffmsy_lo,Ls=m1$ref_ts$ffmsy_hi)
fig4d <- ggplot(data=dfF_fmsy,aes(x=yr,y=F_Fmsy))+
  geom_ribbon(aes(ymin=Li,ymax=Ls),fill="grey70")+
  geom_line()+
  geom_hline(yintercept = 1,linetype="dashed")+
  #geom_hline(yintercept = 0.5,linetype=3)+
  scale_y_continuous(name = "F/Fmsy",limits = c(0,5))+
  theme_bw(14)+xlab("Year")
fig4d

kobe <- data.frame(yr=yr,F_Fmsy=m1$ref_ts$ffmsy,B_Bmsy=m1$ref_ts$bbmsy)
fig5 <- ggplot(data=kobe,aes(x=B_Bmsy,y=F_Fmsy))+

  geom_rect(xmin = 0,
            xmax = 0.5,
            ymin = 0, ymax = 5,
            fill = "red", alpha = 0.01)+
  geom_rect(xmin = 1,
            xmax = 2,
            ymin = 0, ymax = 1,
            fill = "green", alpha = 0.01)+
  geom_rect(xmin = 0.5,
            xmax = 1,
            ymin = 0, ymax = 1,
            fill = "yellow", alpha = 0.01)+
  geom_rect(xmin = 0.5,
            xmax = 2,
            ymin = 1, ymax = 5,
            fill = "red", alpha = 0.01)+
  
  geom_hline(yintercept = 1,linetype=2)+
  geom_vline(xintercept = 0.5,linetype=3)+
  geom_point(size=0.8)+
  geom_path(aes(x=B_Bmsy[length(yr)],F_Fmsy[length(yr)]))+
  geom_text(aes(x=B_Bmsy[length(yr)],F_Fmsy[length(yr)],label="2021"),hjust = 0, nudge_x = 0.05)+
  geom_text(aes(x=B_Bmsy[1],F_Fmsy[1],label="1957"),hjust = 0, nudge_x = 0.05)+
  
  scale_y_continuous(name = "F/Fmsy",limits = c(0,5))+
  scale_x_continuous(name = "B/Bmsy",limits = c(0,2.5))+
  theme_bw(14)
fig5

fig6 <- ggarrange(fig4a,fig4c,labels = c("A","B"),ncol=1,nrow = 2)
fig6
fig7 <- ggarrange(fig4b,fig4d,labels = c("A","B"),ncol=1,nrow = 2)
fig7

dir.create("Lab/OCOM/Figs")
ggsave("OCOM/Figs/Fig2_Biom-MortPesca_congriocol.png",plot=fig6,height = 10)
ggsave("OCOM/Figs/Fig3_PBR_congriocol.png",plot=fig7,height = 10)
ggsave("OCOM/Figs/Fig4_kobe_congriocol.png",plot=fig5)

