##############################################
# Regla de control de captura
# Escenario 1: Regla A  F = 0  (veda)
# Escenario 2: Regla B  F = Escala (Mortalidad constante por tramos)
# Escenario 3: Regla C  F = Rampa (mortalidad por pesca variable)
# Escenario 4: Regla D  F = Rampa con veda
###############################################
# Prepara visualización de las RCC
Brel = seq(0,3,0.01)
Ft = Ct = numeric()
Fmsy = 0.3
RMS = 10
# REGLA A: Ft=0
Ft = rep(0,length(Brel))
Ct = rep(0,length(Brel))
A = Ft/Fmsy
Aa = Ct/RMS
# REGLA B:Escala
for(i in 1:length(Brel)){
  if(Brel[i] < 1){
    Ft[i] = 0.5*Fmsy
    Ct[i] = 0.5*RMS
    if(Brel[i] < 0.5){
      Ft[i] =0
      Ct[i] =0}}else{
        Ft[i] = Fmsy
        Ct[i] = RMS}
  }

B = Ft/Fmsy
Ba = Ct/RMS
# REGLA C: Rampa 0-40
for(i in 1:length(Brel)){
  if(Brel[i] < 1){
    Ft[i] = Brel[i]*Fmsy
    Ct[i] = Brel[i]*RMS
  }
  else{Ft[i] = Fmsy
  Ct[i] = RMS
  }
}
C = Ft/Fmsy
Ca = Ct/RMS
# REGLA D: Rampa 20-40
for(i in 1:length(Brel)){
  if(Brel[i] < 1){
    Ft[i] = Fmsy*(Brel[i] - 0.5)/0.5
    Ct[i] = RMS*(Brel[i] - 0.5)/0.5
    if(Brel[i] <= 0.5){
      Ft[i] = 0
      Ct[i] = 0
    }
  }
  else{Ft[i] = Fmsy
  Ct[i] = RMS
  }
}
D = Ft/Fmsy
Da = Ct/RMS



## GRAFICA TODAS
# F-basada
#png(filename = "Figuras_ocom/fig5_reglas_control_captura_congrio.png")
plot(Brel,Brel,type="n",xlab="B/Brms",ylab="F/Fmsy",xlim=c(0,2),ylim=c(0,2),las=1,main="Reglas F-Basadas")
lines(Brel,A,lwd=2)
lines(Brel,B,col="brown",lwd=2)
lines(Brel,C,col="cyan",lwd=2)
lines(Brel,D,col="blue",lwd=2)
abline(v=0.5,lty=2)
abline(v=1,lty=2)
abline(h=1,lty=2)
#dev.off()
#polygon(c(0,0.5,0.5,0),c(2,2,0,0),col=rgb(1,0,0,alpha=0.2), border=NA)
#polygon(c(0.5,1,1,0.5),c(1,1,0,0),col=rgb(0.255,0.255,0,alpha=0.1), border=NA)
#polygon(c(0.5,2.5,2.5,0.5),c(2,2,1,1),col=rgb(0.255,0.255,0,alpha=0.1), border=NA)
#polygon(c(1,2.5,2.5,1),c(1,1,0,0),col=rgb(0,1,0,alpha=0.1), border=NA)

A = Aa
B = Ba
C = Ca
D = Da

# Captura-basada
plot(Brel,Brel,type="n",xlab="B/Brms",ylab="C/RMS",xlim=c(0,2),ylim=c(0,2),las=1,main="Regla Captura-Basada")
lines(Brel,A,lwd=2)
lines(Brel,B,col="brown",lwd=2)
lines(Brel,C,col="cyan",lwd=2)
lines(Brel,D,col="blue",lwd=2)
abline(v=0.5,lty=2)
abline(v=1,lty=2)
abline(h=1,lty=2)
