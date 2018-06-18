install.packages("googlesheets")
install.packages("RCurl")
install.packages("bitops")
install.packages("dplyr")

library(googlesheets)
library(bitops)
library(RCurl)
library(dplyr)


#####ubicar la hoja dentro tu cuenta de ggole############

gs_ls() #autienticacion de cuenta de google
be <- gs_title("jlemos")#hoja encontrada
gs_ws_ls(be)#
west <- gs_read(ss=be, ws = "datos", skip=0)#hoja de trabajo

dlemos <- as.data.frame(west);dlemos

#original
calif<-dlemos[1:127,12]
x2.test <- shapiro.test(calif)
print(x2.test)

#tranformado 0.1
calif<-dlemos[1:127,12]^0.1
x2.test <- shapiro.test(calif)
print(x2.test)

# Dado un vector dibuja el histograma asociado y la distribución normal

plotn <- function(x,main="Histograma de frecuencias \ny distribución normal",
                  xlab="X",ylab="Densidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  
  hist(x,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="blue")
}
plotn(x,main="Distribución normal")
plotn(x2,main="Distribución uniforme")
#normalidad kolorov smirnov

ks.test(Nombre_Datos$Nombre_Variable, pnorm, mean(Nombre_Datos$Nombre_Variable), sd(Nombre_Datos$Nombre_Variable))

ks.test(dlemos$calificacion, pnorm, mean(dlemos$calificacion), sd(dlemos$calificacion))

ks.test(dlemos$cientificas, pnorm, mean(dlemos$cientificas), sd(dlemos$cientificas))

######## installar (nortest. ) 
library(nortest)
lillie.test(dlemos$calificacion)
#####prueba de correcion Ks
library(agricolae)
data(corn)
attach(corn)
str(corn)
comparison<-kruskal(observation,method,group=TRUE, main="corn")
comparison<-kruskal(observation,method,p.adj="bon",group=FALSE, main="corn")
detach(corn)


####dlemos
attach(dlemos)
str(dlemos)
compa<-kruskal(calificacion,prog,group=TRUE, main="dlemos")
compa<-kruskal(calificacion,prog,p.adj="bon",group=FALSE, main="dlemos")
compa
out<-with(dlemos,Median.test(calificacion,prog,console=FALSE))
out$groups
out$medians
plot(out,ylim=c(0,30),main = "calificaciones")

#######ejemplos####
library(class)
library(e1071)## carga el papel probabilistico

##Leemos los datos (que en este caso est?n en el archivo datos.txt
datos<-read.table("datos.txt",header=T,blank.lines.skip=F)

##Cargamos la funci?n "NORMAL" que es la que nos dir? si los datos siguen distribuci?n normal
source("NORMAL.R")

##Aplicamos la funci?n a nuestros datos
NORMAL(datos)