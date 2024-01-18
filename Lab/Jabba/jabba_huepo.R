# Installation
 install.packages(devtools)
devtools::install_github("jabbamodel/JABBA")
library(JABBA)

df <- read.csv("Data/data_huepo.csv",sep=";")
head(df)

File = "~/Rwork/JabbaSA/huepo/practico1"

#><>><>><>><>><>><>><>><>><>><>><>
# Huepo - Golfo de Arauco
#><>><>><>><>><>><>><>><>><>><>><>
assessment = "HuepoGA"
output.dir = file.path(File,assessment)
dir.create(output.dir,showWarnings = TRUE)
setwd(output.dir)

# Prepara los datos de huepo ----------------------------------------------
cpue = df[,c(1,4)]
colnames(cpue) = c("Year", "G_Arauco")
se = df[,c(1,5)]
colnames(se) = c("Year", "G_Arauco")
catch = df[,c(1,2)]
colnames(se) = c("Year", "Total")
huepo <- list()
huepo$cpue <- cpue
huepo$se <- se
huepo$catch <- catch

#------------------------------------------------------
# Ajuste simple de JABBA a Catch y CPUE con SEs
#-------------------------------------------------------

# Compila JABBA JAGS y objetos de entrada
jbinput = build_jabba(catch=huepo$catch,
                      cpue=huepo$cpue,
                      se=huepo$se,
                      assessment="huepo",
                      scenario = "Simple",
                      model.type = "Schaefer",
                      sigma.est = FALSE,
                      fixed.obsE = 0.01)

# Ajuste JABBA (Valores por defecto - cuidado)
#huepo1 = fit_jabba(jbinput,quickmcmc=TRUE)
huepo1 = fit_jabba(jbinput,
                   save.jabba=TRUE,
                   output.dir=output.dir,
                   quickmcmc = TRUE)
# Make individual plots
jbplot_catch(huepo1)
jbplot_catcherror(huepo1)
jbplot_ppdist(huepo1)
jbplot_residuals(huepo1)
jbplot_cpuefits(huepo1)
jbplot_runstest(huepo1)
jbplot_logfits(huepo1)
jbplot_procdev(huepo1)

# Plot Status Summary 
par(mfrow=c(3,2),mar = c(3.5, 3.5, 0.5, 0.1)) 
jbplot_trj(huepo1,type="B",add=T) 
jbplot_trj(huepo1,type="F",add=T) 
jbplot_trj(huepo1,type="BBmsy",add=T)
jbplot_trj(huepo1,type="FFmsy",add=T)
jbplot_spphase(huepo1,add=T)
jbplot_kobe(huepo1,add=T)


# Try to improve runs test diagnostics by changing the variance settings
jbinput = build_jabba(catch=huepo$catch,
                      cpue=huepo$cpue,
                      se=huepo$se,
                      assessment=assessment,
                      scenario = "S2",
                      model.type = "Schaefer",
                      sigma.est = TRUE,
                      fixed.obsE = 0.1,
                      igamma = c(0.001,0.001),
                      psi.prior = c(1,0.1))
huepo2 = fit_jabba(jbinput,save.jabba=TRUE,output.dir=output.dir)
# Check residual diags
jbplot_cpuefits(huepo2)
jbplot_runstest(huepo2)
jbplot_logfits(huepo2)
# Improved
refinput = jbinput # Note as reference input 


# status summary
par(mfrow=c(3,2),mar = c(3.5, 3.5, 0.5, 0.1))
jbplot_trj(huepo2,type="B",add=T)
jbplot_trj(huepo2,type="F",add=T)
jbplot_trj(huepo2,type="BBmsy",add=T)
jbplot_trj(huepo2,type="FFmsy",add=T)
jbplot_spphase(huepo2,add=T)
jbplot_kobe(huepo2,add=T)

# Write all as png
jabba_plots(jabba=huepo2,output.dir = output.dir)

#------------------------------------------------------
# Estimate shape m as function of Bmsy/K
#-------------------------------------------------------

# Compile JABBA JAGS model and input object
jbinput = build_jabba(catch=huepo$catch,
                      cpue=huepo$cpue,
                      se=NULL,
                      assessment=assessment,
                      scenario = "Est.Shape",
                      model.type = "Pella_m",
                      BmsyK=0.4,
                      sigma.est = TRUE,
                      fixed.obsE = 0.1
                      ,igamma = c(0.001,0.001),
                      psi.prior = c(1,0.1))
huepo3 = fit_jabba(jbinput,save.jabba=TRUE,output.dir=output.dir)

jbplot_ppdist(huepo3) # check shape prior & posterior dist
# Compare
par(mfrow=c(2,2))
jbplot_trj(huepo2,type="BBmsy",add=T)
jbplot_trj(huepo3,type="BBmsy",add=T)
jbplot_kobe(huepo3,add=T)
jbplot_kobe(huepo3,add=T)


jbplot_mcmc(huepo3)
jbplot_residuals(huepo3)
round(huepo3$estimates,3)
round(huepo3$pars,3)

# Organize folders by creating a "retro" subfolder
retro.dir = file.path(output.dir,"retro")
dir.create(retro.dir,showWarnings = F)

# Run hindcasts
huepoM3 = hindcast_jabba(jbinput=jbinput,
                         fit=huepo3,
                         quickmcmc=TRUE,
                         peels = 1:5)

# Retro Analysis Summary plot
jbplot_retro(huepoM3,type = c("B", "F"),xlim = c(),output.dir = retro.dir)
# Zoom-in
mohnsrho = jbplot_retro(huepoM3)

# Save plot and note Mohn's rho statistic
mohnsrho = jbplot_retro(hclamsurM3,as.png = T,single.plots = F,output.dir = retro.dir)
# 
