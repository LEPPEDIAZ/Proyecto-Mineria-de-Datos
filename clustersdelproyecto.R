datos<- todofinal
TodoCompleto<-todofinal[complete.cases(todofinal),]
km<-kmeans(todofinal[,12:13],3)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$CAUFIN))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$CAUFIN))*100
g3<- datos[datos$grupo==3,]
prop.table(table(g3$CAUFIN))*100

plotcluster(todofinal[,1:3],km$cluster)
#silueta
silkm<-silhouette(km$cluster,dist(todofinal[,1:4]))
mean(silkm[,3]) #0.55, no es la mejor partición pero no está mal
#cluster jerarquico
hc<-hclust(dist(todofinal[,11:12])) #Genera el clustering jerárquico de los datos
plot(hc) #Genera el dendograma
rect.hclust(hc,k=3) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=3) #corta el dendograma, determinando el grupo de cada fila
datos$gruposHC<-groups


g1HC<-datos[datos$gruposHC==1,]
g2HC<-datos[datos$gruposHC==2,]
g3HC<-datos[datos$gruposHC==3,]
#cmeans cluster
fcm<-cmeans(todofinal[,12:13],3)
datos$FCGrupos<-fcm$cluster
datos<-cbind(datos,fcm$membership)
silfcm<-silhouette(fcm$cluster,dist(todofinal[,1:4]))
mean(silfcm[,3]) #0.54, no es la mejor partición pero no está mal
#gaussiano para temporada
mc<-Mclust(todofinal[,12:13],3)
plot(mc, what = "classification", main="MClust Classification")
datos$mxGau<-mc$classification
g1MC<-datos[datos$mxGau==1,]
g2MC<-datos[datos$mxGau==2,]
g3MC<-datos[datos$mxGau==3,]
#silueta gaussiano
silmg<-silhouette(mc$classification,dist(todofinal[,12:13]))
mean(silmg[,3])
#gaussiano para departamento
mc<-Mclust(todofinal[,7:9],3)
plot(mc, what = "classification", main="MClust Classification")
datos$mxGau<-mc$classification
g1MC<-datos[datos$mxGau==1,]
g2MC<-datos[datos$mxGau==2,]
g3MC<-datos[datos$mxGau==3,]
#silueta gaussiano
silmg<-silhouette(mc$classification,dist(todofinal[,12:13]))
mean(silmg[,3])
#gaussiano para relaciones interesantes
mc<-Mclust(todofinal[,5:7],3)
plot(mc, what = "classification", main="MClust Classification")
datos$mxGau<-mc$classification
g1MC<-datos[datos$mxGau==1,]
g2MC<-datos[datos$mxGau==2,]
g3MC<-datos[datos$mxGau==3,]
#silueta gaussiano
silmg<-silhouette(mc$classification,dist(todofinal[,5:7]))
mean(silmg[,3])
