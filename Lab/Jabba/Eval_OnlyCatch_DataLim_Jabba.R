#install.packages(devtools)
#devtools::install_github("jabbamo1del/JABBA")

library(JABBA)

File = "/Users/luiscubillos/01Cursos/EvalManPesqLim2024/Lab/Jabba"

# Datos -------------------------------------------------------------------
dt <- read.csv("Datos/congriocol.csv",sep=";")
names(dt)

assessment = "Congrio_col"
output.dir = file.path(File,assessment)
dir.create(output.dir,showWarnings = F)
setwd(output.dir)

# Prepara los datos de huepo ----------------------------------------------
catch = dt[,c(2,3)]
colnames(catch) = c("Year","Total")
congrio <- list()
congrio$catch <- catch

#------------------------------------------------------
# Ajuste simple de JABBA  Solamente Catch
#-------------------------------------------------------
jbinput = build_jabba(catch=congrio$catch,
                      model.type = "Schaefer",
                      assessment=assessment,
                      scenario =  "CatchOnly",
                      Plim=0.2,
                      b.prior=c(0.2,0.3,2002,"bk"),
                      psi.prior = c(0.5,0.3))
# Ajuste JABBA
ccol0 = fit_jabba(jbinput,
                  save.jabba=TRUE,
                  output.dir=output.dir)
# Make individual plots
jbplot_summary(ccol0)
jbplot_catch(ccol0)
jbplot_catcherror(ccol0)
jbplot_ppdist(ccol0)
jbplot_mcmc(ccol0)
jbplot_procdev(ccol0)
jbplot_trj(ccol0,type="B",add=F)
jbplot_trj(ccol0,type="F",add=F)
jbplot_trj(ccol0,type="BBmsy",add=F)
jbplot_trj(ccol0,type="FFmsy",add=F)

ccol0$pars_posterior[,1]
round(ccol0$estimates,3)
round(ccol0$pars,3)
