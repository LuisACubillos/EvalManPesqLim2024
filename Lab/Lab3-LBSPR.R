

lfd_reineta = read.csv("Lab/Data/LFD_Brama_artesanal_total.csv")

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
