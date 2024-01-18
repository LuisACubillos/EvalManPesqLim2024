# Clasificación del desembarque
# Analisis de series de tiempo
library(ggplot2)
library(ggpubr)
library(strucchange)
library(stepR)
library(tseries)
library(forecast)

# Datos -------------------------------------------------------------------
dt <- read.csv("Datos/congriocol.csv",sep=";")
names(dt)

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

##### ARIMA
# Auto ARIMA
range(dt$Year)
yt <- log(dt$Landing)
xt <- dt$Year - dt$Year[1]
yt <- ts(yt,start = c(dt$Year[1],1),frequency = 1)
plot(yt,las=1)

# Auto ARIMA
fig2 = ggAcf(yt)+theme_bw(14)
fig2
fig3 = ggPacf(yt)+theme_bw(14)
fig3

m01 <- auto.arima(yt)
summary(m01)
plot(m01$fitted)
plot(dt$Year,dt$Landing,ty="p",pch=19,cex=0.5)
lines(dt$Year,exp(fitted(m01)),lwd=2)

plot(residuals(m01))
hist(residuals(m01),breaks = 20)

#Prueba Dickey-Fuller
# Test de Dickey-Fuller
adf.test(yt,alternative="stationary")

#Prueba de Chow
mod1 = Fstats(yt ~ 1)
sctest(mod1)
# Valor-p < 0.05 => Hay cambios estructurales
# Detección de puntos de quiebre: segmentación
bp <- breakpoints(yt ~ 1)
summary(bp)
plot(bp)
plot(yt,las=1)
lines(bp)

#Prueba de Chow: tendencia local en los segmentos
mod2 <- Fstats(yt ~ xt)
sctest(mod2)
# Valor-p < 0.05 => Hay cambios estructurales
bp2 <- breakpoints(yt~xt)
summary(bp2)
plot(bp2)
plot(yt,las=1)
lines(fitted(bp2,breaks=3),col="darkgrey",lwd=2)
lines(bp2)

#Log-scale
p3 <- ggplot(data=dt,aes(x=Year,y=log(Landing)))+
  geom_point()+
  geom_line()+
  theme_bw(14)
p3


# Test de segmentación stepFit
xt = dt$Year
yt = log(dt$Landing)
mod3 <- stepFit(y=yt,x=xt,alpha=0.1,jumpint = TRUE,confband = TRUE)
plot(xt,yt,pch=16,col="grey30",las=1,ylab="log(Captura)",xlab="Años")
lines(mod3,lwd=3,col="darkgrey")
summary(mod3)
str(mod3)
mod3$leftIndex
mod3$rightIndex
mod3$value
mod3$leftEnd
mod3$rightEnd
nyr <- length(dt$Year)
salto1 <- mod3$rightIndex[1]
salto2 <- mod3$rightIndex[2]
salto3 <- mod3$rightIndex[3]
salto4 <- mod3$rightIndex[4]
salto5 <- mod3$rightIndex[5]
salto6 <- mod3$rightIndex[6]

sd1 <- sd(yt[1:salto1])
sd2 <- sd(yt[(salto1+1):(salto2)])
sd3 <- sd(yt[(salto2+1):(salto3)])
sd4 <- sd(yt[(salto3+1):(salto4)])
sd5 <- sd(yt[(salto4+1):(salto5)])
sd6 <- sd(yt[(salto5+1):(salto6)])

yr <- dt$Year
mct <- c(rep(mod3$value[1],salto1),
         rep(mod3$value[2],(salto2-salto1)),
         rep(mod3$value[3],(salto3-salto2)),
         rep(mod3$value[4],(salto4-salto3)),
         rep(mod3$value[5],(salto5-salto4)),
         rep(mod3$value[6],(nyr-salto5))
)
sct <- c(rep(sd1,salto1),
         rep(sd2,(salto2-salto1)),
         rep(sd3,(salto3-salto2)),
         rep(sd4,(salto4-salto3)),
         rep(sd5,(salto5-salto4)),
         rep(sd6,(salto6-salto5))
)
llow <- mct-sct
lsup <- mct + sct
df <- data.frame(Year=yr,Mean=mct,SD=sct,Li=llow,Ls=lsup,Landing=dt$Landing)

p4 <- ggplot(data=df,aes(x=Year,y=log(Landing)))+
  #geom_ribbon(data=df,aes(ymin=Li,ymax=Ls),fill="grey70")+
  geom_rect(xmin = mod3$leftEnd[1]-0.5,
            xmax = mod3$rightEnd[1]+0.5,
            ymin = mod3$value[1]-sd1, ymax = mod3$value[1]+sd1,
            fill = "grey70", alpha = 0.01)+
  geom_rect(xmin = mod3$leftEnd[2]-0.5,
            xmax = mod3$rightEnd[2]+0.5,
            ymin = mod3$value[2]-sd2, ymax = mod3$value[2]+sd2,
            fill = "grey70", alpha = 0.01)+
  geom_rect(xmin = mod3$leftEnd[3]-0.5,
            xmax = mod3$rightEnd[3]+0.5,
            ymin = mod3$value[3]-sd3, ymax = mod3$value[3]+sd3,
            fill = "grey70", alpha = 0.01)+
  geom_rect(xmin = mod3$leftEnd[4]-0.5,
            xmax = mod3$rightEnd[4]+0.5,
            ymin = mod3$value[4]-sd4, ymax = mod3$value[4]+sd4,
            fill = "grey70", alpha = 0.01)+
  geom_rect(xmin = mod3$leftEnd[5]-0.5,
            xmax = mod3$rightEnd[5]+0.5,
            ymin = mod3$value[5]-sd5, ymax = mod3$value[5]+sd5,
            fill = "grey70", alpha = 0.01)+
  geom_rect(xmin = mod3$leftEnd[6]-0.5,
            xmax = mod3$rightEnd[6]+0.5,
            ymin = mod3$value[6]-sd6, ymax = mod3$value[6]+sd6,
            fill = "grey70", alpha = 0.01)+
  
  geom_point()+
  geom_segment(aes(x=mod3$leftEnd[1]-0.5), y = mod3$value[1],
               xend =mod3$rightEnd[1]+0.5,yend=mod3$value[1])+
  geom_segment(aes(x=mod3$leftEnd[2]-0.5), y = mod3$value[2],
               xend =mod3$rightEnd[2]+0.5,yend=mod3$value[2])+
  geom_segment(aes(x=mod3$leftEnd[3]-0.5), y = mod3$value[3],
               xend =mod3$rightEnd[3]+0.5,yend=mod3$value[3])+
  geom_segment(aes(x=mod3$leftEnd[4]-0.5), y = mod3$value[4],
               xend =mod3$rightEnd[4]+0.5,yend=mod3$value[4])+
  geom_segment(aes(x=mod3$leftEnd[5]-0.5), y = mod3$value[5],
               xend =mod3$rightEnd[5]+0.5,yend=mod3$value[5])+
  geom_segment(aes(x=mod3$leftEnd[6]-0.5), y = mod3$value[6],
               xend =mod3$rightEnd[6]+0.5,yend=mod3$value[6])+
  geom_vline(xintercept = mod3$rightEnd+0.5,linetype=3)+
  
  ylab("log(landings)")+
  scale_y_continuous(breaks = c(5,6,7,8),limits = c(4,8.5))+
  
  theme_bw(14)
p4

fig2 <- ggarrange(p2,p4,labels=c("A","B"),nrow=2,ncol=1)
fig2
ggsave("Report/Figs/Fig02_catch_analysis.jpg",fig2,width = 18,height = 32,units = "cm")

