
#devtools::install_github("https://github.com/criscan/LBPA_r")
library(LBPA)
?LBPA_fits

model1 <- LBPA_fits("Lab/LBPAr/lbpa_dat.xlsx", graph_opt=T, save_opt=T)

# Modelo para centolla Los Lagos:

model2 = LBPA_fits("Lab/LBPAr/lbpa_datos_centolla_LosLagos.xlsx", graph_opt=T, save_opt=F )

#LBSPR
library(LBSPR)

lfd_centolla = data.frame(read_xlsx("Lab/LBPAr/lbpa_datos_centolla_LosLagos.xlsx", sheet = "LF data", col_names = TRUE))
head(lfd_centolla)
colnames(lfd_centolla) = c("Orden","LC", 2012:2019)
#write.csv(lfd_centolla,file = "Lab/Data/lfd_centolla.csv")

MyPars <- new("LB_pars")
MyPars@Species <- "Centolla Los Lagos"
#NOTA:La maxima longitud no puede ser menor que Linf
MyPars@Linf <- 176.8 # parametros
# Madurez
MyPars@L50 <- 116.9
MyPars@L95 <- 129.5
#calcular M/K 
MyPars@MK <- 1.32 # M=0.2, K=0.115
MyPars@L_units <- "mm"
CentollaLL_LFD1 <- new("LB_lengths",
                  LB_pars=MyPars,
                  file="Lab/Data/lfd_centolla.csv",
                  dataType="freq",header=TRUE)
CentollaLL_M1 <- LBSPRfit(MyPars, CentollaLL_LFD1)


plotSize(CentollaLL_M1)
plotMat(CentollaLL_M1)
plotEsts(CentollaLL_M1)
CentollaLL_M1@Ests

MyPars@SPR <- 0.4
yr <- 8
MyPars@SL50 <- CentollaLL_M1@SL50[yr]
MyPars@SL95 <- CentollaLL_M1@SL95[yr]
plotTarg(MyPars, CentollaLL_LFD1, yr=yr)
