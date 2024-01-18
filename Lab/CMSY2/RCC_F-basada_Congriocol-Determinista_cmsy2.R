##############################################################
#  GUIA PRACTICA - ONLY CATCH MODELS - Fase de Proyección
#  FASE DE PROYECCION DETERMINISTA Y HCR basada en Mort. Pesca
#  Curso Evaluación y Manejo de Pesquerías Limitadas en Datos
#  Prof. Luis Cubillos - Universidad de Concepción
#  MODELO: CMS2
#  Modelos de Producción Dinámicos
#  B_{t+1} = B_{t} + r B_{t} * (1-B_{t}/k) - C_{t}
##############################################################



# Fase proyección determinista con Reglas de Control de Captura -----------

# Fase de evaluación de stock ---------------------------------------------
load("~/01Cursos/EvalManPesqLim2024/Lab/CMSY2/CMSY2_congriocol_m2.RData")

m1 = m2

m1$ref_pts
r <- m1$ref_pts[1,2]
k <- m1$ref_pts[2,2]
bmsy <- m1$ref_pts[5,2]
fmsy <- m1$ref_pts[4,2]
Bend <- m1$ref_ts$b[length(m1$ref_ts$b)]
Yend <- m1$ref_ts$catch[length(m1$ref_ts$catch)]
RMS = r*k/4

nhcr = 4 #numero de reglas de control
nproy = 40 #numero de años proyectados
yrp = seq(2021 + 1, 2021 + nproy, 1) #años futuros
B = matrix(data = NA, nrow = nproy+1, ncol = nhcr)
C = matrix(data = NA, nrow = nproy+1, ncol = nhcr)
Ft= matrix(data = NA, nrow = nproy+1, ncol = nhcr)
for(j in 1:nhcr){
  for(i in 1:(nproy+1)){
    if(i == 1){
      #Proyeccion año 1
      (B[i,j] = Bend + r*Bend*(1 - Bend/k) - Yend)*exp(rnorm(1,mean=0,sd=0.1))
      Brel = Bend/bmsy
      #APLICACION DE REGLAS
      if(j==1){
        Ft[i,j]=0
      }
      if(j==2){
        if(Brel <= 1){
          Ft[i,j] = 0.5*fmsy
          if(Brel < 0.5){Ft[i,j] =0}
        }
        else{
          Ft[i,j] = fmsy
          #Ct[l,i,2] = RMS
        }
      }
      if(j==3){
        if(Brel < 1){
          Ft[i,3] = Brel*fmsy
        }
        else{Ft[i,3] = fmsy}
      }
      if(j==4){
        if(Brel < 1){
          Ft[i,4] = fmsy*(Brel - 0.5)/0.5
          if(Brel <= 0.5){
            Ft[i,4] = 0
          }
        }
        else{Ft[i,4] = fmsy}
      }
      C[i,j] = Ft[i,j]*B[i,j]
    }else{
      # a partir del segundo año
      (B[i,j] = max(1,B[i-1,j] + r*B[i-1,j]*(1 - B[i-1,j]/k) - C[i-1,j]))*exp(rnorm(1,mean=0,sd=0.6))
      Brel = B[i-1,j]/bmsy
      #APLICACION DE REGLAS
      if(j==1){
        Ft[i,j]=0
      }
      if(j==2){
        if(Brel <= 1){
          Ft[i,j] = 0.5*fmsy
          if(Brel < 0.5){Ft[i,j] =0}
        }
        else{
          Ft[i,j] = fmsy
          #Ct[l,i,2] = RMS
        }
      }
      if(j==3){
        if(Brel < 1){
          Ft[i,3] = Brel*fmsy
        }
        else{Ft[i,3] = fmsy}
      }
      if(j==4){
        if(Brel < 1){
          Ft[i,4] = fmsy*(Brel - 0.5)/0.5
          if(Brel <= 0.5){
            Ft[i,4] = 0
          }
        }
        else{Ft[i,4] = fmsy}
      }
      C[i,j] = Ft[i,j]*B[i,j]
    }
  }
}

B1 <- c(m1$ref_ts$b,B[1:nproy,1])
B2 <- c(m1$ref_ts$b,B[1:nproy,2])
B3 <- c(m1$ref_ts$b,B[1:nproy,3])
B4 <- c(m1$ref_ts$b,B[1:nproy,4])
Year <- c(m1$ref_ts$year,yrp)
Biom <- data.frame(Year,B1,B2,B3,B4)

#png(filename = "Figuras/Proyeccion_determinista_biomasa.png")
plot(Year,Biom$B1,ty="l",ylim=c(0,35000),ylab="Biomasa",las=1)
lines(Year,Biom$B2,col="brown",lwd=2)
lines(Year,Biom$B3,col="cyan",lwd=2)
lines(Year,Biom$B4,col="blue",lwd=2)
lines(Year,Biom$B1,col="black",lwd=2)
abline(h=bmsy,lty=3,col="green")
abline(h=0.5*bmsy,lty=3,col="red")
legend("topleft",c("F=0","Escala","Rampa","Rampa+veda"),col=c("black","brown","cyan","blue"),lty=c(1,1,1,1),cex=0.6)
#dev.off()

#png(filename = "Figuras/Proyeccion_determinista_razon_b-bmsy.png")
plot(Year,Biom$B1/bmsy,ty="l",cex=0.3,ylim=c(0,2),ylab="B/Bmsy",las=1)
lines(Year,Biom$B2/bmsy,col="brown",lwd=2)
lines(Year,Biom$B3/bmsy,col="cyan",lwd=2)
lines(Year,Biom$B4/bmsy,col="blue",lwd=2)
lines(Year,Biom$B1/bmsy,col=1,lwd=2)
abline(h=1,lty=3,col="green",lwd=2)
abline(h=0.5,lty=3,col="red",lwd=2)
legend("topleft",c("F=0","Escala","Rampa","Rampa+veda"),col=c("black","brown","cyan","blue"),lty=c(1,1,1,1),cex=0.6)
#dev.off()


