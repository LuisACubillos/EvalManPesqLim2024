


# Packages ----------------------------------------------------------------
#install.packages("devtools")
#install.packages("TMB")
#devtools::install_github("james-thorson/FishLife")
library(FishLife)

# Funcion Crea Parámetros de Historia de Vida -----------------------------

crea_lh_list <- function(lh,puntual=TRUE,seed = NULL){
  Linf = exp(lh[[1]]$Mean_pred[1])
  K = exp(lh[[1]]$Mean_pred[2])
  Winf = exp(lh[[1]]$Mean_pred[3])
  tmax = exp(lh[[1]]$Mean_pred[4])
  tm = exp(lh[[1]]$Mean_pred[5])
  M = exp(lh[[1]]$Mean_pred[6])
  Lm = exp(lh[[1]]$Mean_pred[7])
  a = Winf/Linf^3
  b = 3
  # Relación stock-recluta
  h = lh[[1]]$Mean_pred[13]
  sigmar = exp(lh[[1]]$Mean_pred[12])
  rho = exp(lh[[1]]$Mean_pred[10])
  # Funciones
  A <- ceiling(tmax)
  t0 <- -10^(-0.392 - 0.275*log10(Linf)-1.038*log10(K))
  edad <- seq(1,A,1)
  lt <- Linf*(1-exp(-K*(edad-t0)))
  wt <- Winf*(1-exp(-K*(edad-t0)))^3
  dm = 2
  pm <- 1/(1+exp(-(lt-Lm)/dm))
  out_lh <- NULL
  out_lh$Linf <- Linf
  out_lh$K <- K
  out_lh$t0 <- t0
  out_lh$Winf <- Winf
  out_lh$tmax <- tmax
  out_lh$tm <- tm
  out_lh$M <- M
  out_lh$Lm <- Lm
  out_lh$a <- a
  out_lh$b <- b
  out_lh$dm <- dm
  out_lh$h <- h
  out_lh$sigmar <- sigmar
  out_lh$rho <- rho
  out_lh$edad <- edad
  out_lh$lt <- lt
  out_lh$wt <- wt
  out_lh$pm <- pm
  return(out_lh)
}


# Ejemplo de aplicación ---------------------------------------------------

gen = "Scomber"
spp = "japonicus"
m1 <- Plot_taxa(Search_species( Genus=gen, Species = spp)$match_taxonomy, mfrow=c(2,3) )


# Parametros de historia de vida ------------------------------------------
lh <- crea_lh_list(lh=m1)

plot(lh$edad,lh$lt,type="b",xlim=c(0,max(lh$edad)),ylim=c(0,max(lh$lt)*1.2),las=1,xlab="Edad",ylab="Longitud (cm)")
plot(lh$edad,lh$wt,type="b",xlim=c(0,max(lh$edad)),ylim=c(0,max(lh$wt)*1.2),las=1,xlab="Edad",ylab="Peso total (gr)")
plot(lh$edad,lh$pm,type="b",xlim=c(0,max(lh$edad)),ylim=c(0,max(lh$pm)*1.2),las=1,xlab="Edad",ylab="Madurez")


# Biomasa deosvante por recluta -------------------------------------------

SPRFmort <- function(Fmort,Tspw){
  n <- length(Fmort)
  amax <- length(age)
  spr <- rep(0,n)
  npr <- rep(0,amax)
  ypr <- rep(0,n)
  npr[1] <- 1
  for(j in 1:n){
    for(i in 2:amax)
    {
      npr[i] <- npr[i-1]*exp(-(M+Fmort[j]*Sel[i-1]))
    }
    for(i in 1:amax)
    {
      spr[j] <- spr[j]+npr[i]*Ph[i]*W[i]*exp(-(M+Fmort[j]*Sel[i])*Tspw)
      ypr[j] <- ypr[j]+Fmort[j]*Sel[i]*npr[i]*W[i]*(1-exp(-(M+Fmort[j]*Sel[i])))/(M+Fmort[j]*Sel[i]) 
    }
  }
  Pspr <- spr/spr[1]*100
  out <- NULL
  out$SPRo <- spr[1]
  out$spr <- spr
  out$Pspr <- Pspr
  out$ypr  <- ypr
  assign("out",out,pos=1)
}

Fmort <- seq(0,2,0.01) # Mortalidad por pesca
Sel <- lh$pm #selectividad=madurez
plot(lh$edad,Sel,type="b",xlim=c(0,max(lh$edad)),ylim=c(0,1),las=1,xlab="Edad",ylab="Selectividad")
age <- seq(1,lh$tmax,1)
W <- lh$wt #peso promedio a la edad
Ph <- lh$pm # fracción de madurez a la edad
M = lh$M # tasa de mortalidad natural
Tspw=7/12 #epoca de desove
Out <- SPRFmort(Fmort,Tspw) # Se llama  a la función previa y se guardan los resultados en Out

# Biomasa desovante por recluta
plot(Fmort,Out$spr,ty="l",las=1,ylab="Biomasa desovante por recluta (gr)",xlab="Mortalidad por pesca (F)")
# Razón de potencial reproductivo
plot(Fmort,Out$Pspr,ty="l",las=1,ylab="SPR (%)",xlab="Mortalidad por pesca (F)")
# Rendimiento por recluta
plot(Fmort,Out$ypr,ty="l",las=1,ylab="YPR (gr)",xlab="Mortalidad por pesca (F)")



# Relación stock-recluta Beverton-Holt ------------------------------------

# Línea de reemplazo
sb <- seq(0,Out$SPRo,length=length(Fmort))
phi = Out$SPRo
rps <- 1/phi
lr <- rps*sb
plot(sb,lr,type="l",lty=2,las=1,ylab="Reclutamiento relativo",xlab="Biomasa desovante por recluta")
points(Out$SPRo,1,pch=19,col="green")

h <- lh$h
R0=1
S0 = phi*R0
spr = Out$spr
alpha <- (1-h)/(4*h)*(S0/R0)
beta  <-  (5*h-1)/(4*h*R0)
Srel <- (spr - alpha)/(beta)
Rrel <- Srel/(alpha + beta*Srel)
plot(sb,lr,type="l",lty=2,las=1,ylab="Reclutamiento relativo",xlab="Biomasa desovante por recluta (gr)")
lines(Srel,Rrel,lwd=1.5)
points(S0,1,pch=19,col="green")
points(0.2*S0,h*R0,pch=19,col="brown")



# Curvas de producción ----------------------------------------------------

Yrel = Out$ypr*Rrel
plot(Fmort,Yrel,type="l",lwd=1.5,ylim=c(0,max(Yrel)*1.2),las=1,ylab="Captura relativa",xlab="Mortalidad por pesca (F)")
plot(Srel,Yrel,type="l",lwd=1.5,ylim=c(0,max(Yrel)*1.2),las=1,ylab="Captura relativa",xlab="Biomasa (S)")
