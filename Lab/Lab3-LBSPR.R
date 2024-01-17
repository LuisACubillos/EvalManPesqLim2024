
library(dplyr)
lfd_reineta = read.csv("Lab/Data/LFD_Brama_artesanal_total.csv")
glimpse(lfd_reineta)


library(LBSPR)
MyPars <- new("LB_pars")
MyPars@Species <- "Reineta"
#NOTA:La maxima longitud no puede ser menor que Linf
MyPars@Linf <- 56.9 # parametros Moyano
# Madurez
MyPars@L50 <- 37.7 #LEAL et al.2017
MyPars@L95 <- 43
#calcular M/K 
MyPars@MK <- 1.94 # M=0.35, K=0.18
MyPars@L_units <- "cm"
Brama_LFD1 <- new("LB_lengths",
                  LB_pars=MyPars,
                  file="Lab/Data/LFD_Brama_artesanal_total.csv",
                  dataType="freq",header=TRUE)
Brama_M1 <- LBSPRfit(MyPars, Brama_LFD1)

glimpse(Brama_M1)
plotSize(Brama_M1)
plotMat(Brama_M1)
plotEsts(Brama_M1)
Brama_M1@Ests

MyPars@SPR <- 0.4
yr <- 14
MyPars@SL50 <- Brama_M1@SL50[yr]
MyPars@SL95 <- Brama_M1@SL95[yr]
plotTarg(MyPars, Brama_LFD1, yr=yr)


Brama_LFD2 <- new("LB_lengths",
                  LB_pars=MyPars,
                  file="Lab/Data/LFD_Artesanal_Reineta_MA5yr.csv",
                  dataType="freq",
                  header=TRUE)
Brama_M2 <- LBSPRfit(MyPars, Brama_LFD2)

plotSize(Brama_M2)
plotMat(Brama_M2)
plotEsts(Brama_M2)
Brama_M2@Ests

