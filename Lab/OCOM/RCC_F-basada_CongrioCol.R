##############################################################
#  GUIA PRACTICA - ONLY CATCH MODELS - Fase de Proyección
#  FASE DE PROYECCION CON INCERTIDUMBRE Y HCR basada en Mort. Pesca
#  Curso Evaluación y Manejo de Pesquerías Limitadas en Datos
#  Prof. Luis Cubillos - Universidad de Concepción
#  MODELO: OCOM
#  Modelos de Producción Dinámicos
#  B_{t+1} = B_{t} + r B_{t} * (1-B_{t}/k) - C_{t}
##############################################################
rm(list = ls())
library(ggplot2)
library(ggpubr)
library(datalimited2)
# Fase de evaluación de stock ---------------------------------------------
load("~/01Cursos/EvalManPesqLim2024/Lab/OCOM/ocom_m1_congrioco.RData")

# Fase de proyeccion stocastico
#Resumen parámetros
m1$ref_pts
r <- m1$ref_pts[1,4]
k <- m1$ref_pts[2,4]
bmsy <- m1$ref_pts[4,4]
fmsy <- m1$ref_pts[5,4]
Bend <- m1$ref_ts$b[length(m1$ref_ts$b)]
Yend <- m1$ref_ts$catch[length(m1$ref_ts$catch)]
RMS = r*k/4
nsim = 500
nhcr = 4 #numero de reglas de control (se agrega una más)
nyr <- length(m1$ref_ts$year)
nproy = nyr+40 #numero de años proyectados
yrp = seq(2022 + 1, 2022 + nproy+1, 1) #años futuros
B_Bmsy = B = C = Ft = Ct = array(data = NA, dim = c(nsim, nproy+1,nhcr))
#Se seleccionan nsim valores de los parametros viables
rv = kv = b1 = numeric()
for(i in 1:length(m1$krms_draws$r)){
  if(m1$krms_draws$r[i]>m1$ref_pts[1,2]&m1$krms_draws$r[i]<m1$ref_pts[1,6]){
    rv[i] = m1$krms_draws$r[i]
    kv[i] = m1$krms_draws$k[i]
    b1[i] = m1$krms_draws$k[i]
  }
  #if(m1$k_viable[i]>(m1$ref_pts[2,3]/1000)&m1$k_viable[i]<(m1$ref_pts[2,4]/1000)){kv[i] = m1$k_viable[i]}
}
rv = na.omit(rv)
kv = na.omit(kv)
b1 = na.omit(b1)
range(rv)
range(b1)
set.seed(378)
select <- floor(runif(n = nsim,min = 1,max = length(rv)))
plot(m1$krms_draws$r,m1$krms_draws$k)
points(rv[select],kv[select],col="yellow")
legend("topright",legend = c("pares r-k","selección r-k"),pch=c(1,1),col=c(1,"yellow"))

#Error de proceso
sd.proc <- 0
#Error de observación en las capturas efectivas
sd.obs <- 0


for(l in 1:nsim){
  ll <- select[l]
  #r <- m1$r_viable[ll]
  #k <- m1$k_viable[ll]*1000
  r <- rv[ll]
  k <- kv[ll]
  rr <- m1$ref_pts[1,4]
  kk <- m1$ref_pts[2,4]
  Bini <- kk #m1$ref_ts$b[1]
  #Bend <- m1$bt_viable[ll,27]*k
  B1 <-   b1[ll]
  bmsy <- k/2
  fmsy <- r/2
  
  for(j in 1:nhcr){
    #Fase histórica
    C[l,1,j] <- m1$ref_ts$catch[1] # condicionar con error de obs
    B[l,1,j] <- Bini
    Ft[l,1,j] <- C[l,1,j]/B[l,1,j]
    B_Bmsy[l,1,j] <- 2*B[l,1,j]/kk
    for(i in 2:nyr){
      C[l,i,j] = m1$ref_ts$catch[i]
      (B[l,i,j] = max(1,B[l,i-1,j] + rr*B[l,i-1,j]*(1 - B[l,i-1,j]/kk) - C[l,i-1,j]))*exp(rnorm(1,0,sd.proc))
      Ft[l,i,j] = C[l,i,j]/B[l,i,j]
      B_Bmsy[l,i,j] = 2*B[l,i,j]/kk
    }
    
    #Fase proyección
    for(i in (nyr+1):(nproy+1)){
      if(i == (nyr+1)){
        #Proyeccion año 1
        (B[l,i,j] = max(1,B[l,i-1,j] + r*B[l,i-1,j]*(1 - B[l,i-1,j]/k) - C[l,i-1,j]))*exp(rnorm(1,0,sd.proc))
        Brel = B[l,i-1,j]/bmsy
        B_Bmsy[l,i,j]=B[l,i,j]/bmsy
        #APLICACION DE REGLAS
        if(j==1){
          Ft[l,i,j]=0
          #Ct[l,i,j]=0
        }
        if(j==2){
          if(Brel <= 1){
            Ft[l,i,j] = 0.5*fmsy
            if(Brel < 0.5){Ft[l,i,j] =0}
            #Ct[l,i,j] = 0.5*RMS
          }
          else{
            Ft[l,i,2] = fmsy
            #Ct[l,i,2] = RMS
          }
        }
        if(j==3){
          if(Brel < 1){
            Ft[l,i,3] = Brel*fmsy
            #Ct[l,i,3] = Brel*RMS
          }
          else{
            Ft[l,i,3] = fmsy
            #Ct[l,i,3] = RMS
          }
        }
        if(j==4){
          if(Brel < 1){
            Ft[l,i,4] = fmsy*(Brel - 0.5)/0.5
            #Ct[l,i,4] = RMS*(Brel - 0.5)/0.5
            if(Brel <= 0.5){
              Ft[l,i,4] = 0
              #Ct[l,i,4] = 0
            }
          }
          else{
            Ft[l,i,4] = fmsy
            #Ct[l,i,4] = RMS
          }
        }
        C[l,i,j] = (Ft[l,i,j]*B[l,i,j])*exp(rnorm(1,0,sd.obs))
        Ft[l,i,j] = C[l,i,j]/B[l,i,j] #Desempeño de F segun Captura efectiva
        #C[l,i,j] = Ct[l,i,j]*exp(rnorm(1,0,sd.obs))
      }else{
        # a partir del segundo año
        (B[l,i,j] = max(1,B[l,i-1,j] + r*B[l,i-1,j]*(1 - B[l,i-1,j]/k) - C[l,i-1,j]))*exp(rnorm(1,0,sd.proc))
        Brel = B[l,i-1,j]/bmsy
        B_Bmsy[l,i,j]=B[l,i,j]/bmsy
        #APLICACION DE REGLAS
        if(j==1){
          Ft[l,i,j]=0
          #Ct[l,i,j]=0
        }
        if(j==2){
          if(Brel <= 1){
            Ft[l,i,j] = 0.5*fmsy
            if(Brel < 0.5){Ft[l,i,j] =0}
            #Ct[l,i,j] = 0.5*RMS
          }
          else{
            Ft[l,i,2] = fmsy
            #Ct[l,i,2] = RMS
          }
        }
        if(j==3){
          if(Brel < 1){
            Ft[l,i,3] = Brel*fmsy
            #Ct[l,i,3] = Brel*RMS
          }
          else{
            Ft[l,i,3] = fmsy
            #Ct[l,i,3] = RMS
          }
        }
        if(j==4){
          if(Brel < 1){
            Ft[l,i,4] = fmsy*(Brel - 0.5)/0.5
            #Ct[l,i,4] = RMS*(Brel - 0.5)/0.5
            if(Brel <= 0.5){
              Ft[l,i,4] = 0
              #Ct[l,i,4] = 0
            }
          }
          else{
            Ft[l,i,4] = fmsy
            #Ct[l,i,4] = RMS
          }
        }
        C[l,i,j] = (Ft[l,i,j]*B[l,i,j])*exp(rnorm(1,0,sd.obs))
        #C[l,i,j] = (Ct[l,i,j])*exp(rnorm(1,0,sd.obs))
        Ft[l,i,j] = C[l,i,j]/B[l,i,j] #Desempeño de F segun Captura efectiva
      }
    }
    
  }
}


# Graficos ----------------------------------------------------------------

# BIOMASA

db <- NULL
data <- B
nsim = dim(data)[1]
nyear = dim(data)[2]
nhcr = dim(data)[3]
HCR <- c("A","B","C","D")
for(iter in 1:nhcr){
  tmp_db <- cbind(mod = data[,,iter])
  tmp_db <- t(apply(t(tmp_db),1,quantile,c(0.1,0.25,0.5,0.75,0.9),na.rm=TRUE))
  db <- rbind(db,tmp_db)
}

mod <- rep(1:nhcr,each=nyear)
syr <- m1$ref_ts$year[1]
iyrs <- rep(c(syr:(syr+nyear-1)),nhcr)
endyr <- m1$ref_ts$year[length(m1$ref_ts$year)]
colnames(db) <- c("Li","L1","Mediana","L2","Ls")
db_mod <- cbind(as.data.frame(db),Year=iyrs,HCR=HCR[mod])
df <- db_mod
db <- NULL
tail(df)

p1 <- ggplot(data=df,aes(x=Year,y=Mediana,group=HCR))+
  #geom_rect(xmin = 1957,
  #xmax = 2021,
  #ymin = 0, ymax = 60000,
  #fill = "pink", alpha = 0.01)+
  geom_ribbon(aes(ymin = Li,ymax= Ls),fill="grey")+
  #geom_ribbon(aes(ymin = L1,ymax= L2),fill="grey60")+
  geom_line()+
  geom_vline(xintercept = endyr,linetype=3)+
  geom_hline(yintercept = bmsy,col="green")+
  geom_hline(yintercept = 0.5*bmsy,col="red")+
  facet_wrap(~HCR,ncol=4)+
  scale_y_continuous(name="Biomasa")+
  theme_bw()
p1

data <- C
nsim = dim(data)[1]
nyear = dim(data)[2]
nhcr = dim(data)[3]
HCR <- c("A","B","C","D")
for(iter in 1:nhcr){
  tmp_db <- cbind(mod = data[,,iter])
  tmp_db <- t(apply(t(tmp_db),1,quantile,c(0.1,0.25,0.5,0.75,0.9),na.rm=TRUE))
  db <- rbind(db,tmp_db)
}

mod <- rep(1:nhcr,each=nyear)
syr <-m1$ref_ts$year[1]
iyrs <- rep(c(syr:(syr+nyear-1)),nhcr)
colnames(db) <- c("Li","L1","Mediana","L2","Ls")
db_mod <- cbind(as.data.frame(db),Year=iyrs,HCR=HCR[mod])
df <- db_mod
db <- NULL

p2 <- ggplot(data=df,aes(x=Year,y=Mediana,group=HCR))+
  geom_ribbon(aes(ymin = Li,ymax= Ls),fill="grey")+
  #geom_ribbon(aes(ymin = L1,ymax= L2),fill="grey30")+
  geom_line()+
  geom_vline(xintercept = endyr,linetype=3)+
  geom_hline(yintercept = RMS,col="green")+
  facet_wrap(~HCR,ncol=4)+
  scale_y_continuous(name="Captura")+
  theme_bw()
p2

data <- Ft
nsim = dim(data)[1]
nyear = dim(data)[2]
nhcr = dim(data)[3]
HCR <- c("A","B","C","D")
for(iter in 1:nhcr){
  tmp_db <- cbind(mod = data[,,iter])
  tmp_db <- t(apply(t(tmp_db),1,quantile,c(0.1,0.25,0.5,0.75,0.9),na.rm=TRUE))
  db <- rbind(db,tmp_db)
}

mod <- rep(1:nhcr,each=nyear)
syr <-m1$ref_ts$year[1]
iyrs <- rep(c(syr:(syr+nyear-1)),nhcr)
colnames(db) <- c("Li","L1","Mediana","L2","Ls")
db_mod <- cbind(as.data.frame(db),Year=iyrs,HCR=HCR[mod])
df <- db_mod
db <- NULL

p3 <- ggplot(data=df,aes(x=Year,y=Mediana,group=HCR))+
  geom_ribbon(aes(ymin = Li,ymax= Ls),fill="grey")+
  #geom_ribbon(aes(ymin = L1,ymax= L2),fill="grey30")+
  geom_line()+
  geom_vline(xintercept = endyr,linetype=3)+
  geom_hline(yintercept = 0.13,col="green")+
  facet_wrap(~HCR,ncol=4)+
  scale_y_continuous(name="Mortalidad por pesca",limits = c(0,0.5))+
  theme_bw()
p3

data <- B_Bmsy
nsim = dim(data)[1]
nyear = dim(data)[2]
nhcr = dim(data)[3]
HCR <- c("A","B","C","D")
for(iter in 1:nhcr){
  tmp_db <- cbind(mod = data[,,iter])
  tmp_db <- t(apply(t(tmp_db),1,quantile,c(0.1,0.25,0.5,0.75,0.9),na.rm=TRUE))
  db <- rbind(db,tmp_db)
}

mod <- rep(1:nhcr,each=nyear)
syr <-m1$ref_ts$year[1]
iyrs <- rep(c(syr:(syr+nyear-1)),nhcr)
colnames(db) <- c("Li","L1","Mediana","L2","Ls")
db_mod <- cbind(as.data.frame(db),Year=iyrs,HCR=HCR[mod])
df <- db_mod
db <- NULL

p4 <- ggplot(data=df,aes(x=Year,y=Mediana,group=HCR))+
  geom_ribbon(aes(ymin = Li,ymax= Ls),fill="grey")+
  #geom_ribbon(aes(ymin = L1,ymax= L2),fill="grey30")+
  geom_line()+
  geom_vline(xintercept = endyr,linetype=3)+
  geom_hline(yintercept = 1,col="green")+
  geom_hline(yintercept = 0.5,col="red")+
  facet_wrap(~HCR,ncol=4)+
  scale_y_continuous(name="B/Brms",limits = c(0,2.1))+
  theme_bw()
p4

#dir.create("Lab/OCOM/Figs")
ggsave("Lab/OCOM/Figs/Figuras_ocom/Fig_9_Proyeccion_Biomasa.png",plot=p1)
ggsave("Lab/OCOM/Figs/Figuras_ocom/Fig_10_Proyeccion_Capturas.png",plot=p2)
ggsave("Lab/OCOM/Figs/Figuras_ocom/Fig_11_Proyeccion_MortPesca.png",plot=p3)
ggsave("Lab/OCOM/Figs/Figuras_ocom/Fig_12_Proyeccion_razon_B-Bmsy.png",plot=p4)
