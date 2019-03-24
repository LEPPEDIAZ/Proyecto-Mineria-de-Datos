todo <- read.csv("todo.csv")
colnames(todo)<-c("x", "ao","mes","depto","cie","diagnostico","grupoedad","genero","cantidad")
summary(todo)

todo$numero_mes<- as.numeric(todo$mes)
todo$numero_dep<- as.numeric(todo$depto)
todo$numero_cie<- as.numeric(todo$cie)
todo$numero_diag<- as.numeric(todo$diagnostico)
todo$numero_grupoedad<- as.numeric(todo$grupoedad)
todo$numero_gen<- as.numeric(todo$genero)

todo
#histogramas
hist(todo$X)
hist(todo$AÃ.o)
hist(todo$Cantidad.total)
#tablas de normalidad
qqnorm(todo$X)
qqline(todo$X, col = "steelblue", lwd = 2)
qqnorm(todo$AÃ.o)
qqline(todo$AÃ.o, col = "steelblue", lwd = 2)
qqnorm(todo$Cantidad.total)
qqline(todo$Cantidad.total, col = "steelblue", lwd = 2)


#Se muestra la tabla de frecuencia de departamento
departamentofreq <- with(todo, table(Departamento))
margin.table(departamentofreq,1)
#se muestra tabla de frecuencias de grupo de edad
grupoedaad <- with(todo, table(Grupo.de.Edad))
margin.table(grupoedaad, 1)
names(todo)[7] <- paste("grupoedad")
#tabla de frecuencias de mes
mes <- with(todo, table(Mes))
margin.table(mes,1)
#tabla de frecuencias de diagnostico
names(todo)[6] <- paste("diagnostico")
diagnosticofinal <- with(todo, table(diagnostico))
margin.table(diagnosticofinal,1)
#tabla de frecuencias de genero
names(todo)[8] <- paste("genero")
generofinal <- with(todo, table(genero))
margin.table(generofinal,1)
#CLUSTERSlibrary(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el nï¿½mero de clusters ï¿½ptimo
library(factoextra) #Para hacer grï¿½ficos bonitos de clustering
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el n?mero de clusters ?ptimo
library(factoextra) #Para hacer gr?ficos bonitos de clustering
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
#gaussiano
datos<- todo



mc<-Mclust(datos[,9:16],3)
plot(mc, what = "classification", main="MClust Classification")
datos$mxGau<-mc$classification
g1MC<-datos[datos$mxGau==1,]
g2MC<-datos[datos$mxGau==2,]
g3MC<-datos[datos$mxGau==3,]
#silueta gaussiano
silmg<-silhouette(mc$classification,dist(datos[,9:16]))
mean(silmg[,3])
#cluster k-medias
datos<- todo
irisCompleto<-todo[complete.cases(todo),]
todo
todo<- na.omit(todo)
km<-kmeans(todo[,9:15],3)

datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$diagnostico))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$diagnostico))*100
g3<- datos[datos$grupo==3,]
prop.table(table(g3$diagnostico))*100

plotcluster(todo[,9:15],km$cluster) #grafica la ubicaciÃ³n de los clusters
#Error in eigen(m, symmetric = TRUE) : infinite or missing values in 'x'
#c-means
my_data2 <- todo[, c(9, 1, 2, 7, 3, 4, 5, 6, 8)]
fcm<-cmeans(todo[,3:4],7)
datos$FCGrupos<-fcm$cluster
datos<-cbind(datos,fcm$membership)
#jerarquico
hc<-hclust(dist(my_data2[,1:4])) #Genera el clustering jerÃ¡rquico de los datos
#Error: cannot allocate vector of size 2105.6 Gb
plot(hc) #Genera el dendograma
rect.hclust(hc,k=3) #Dibuja el corte de los grupos en el grÃ¡fico
groups<-cutree(hc,k=3) #corta el dendograma, determinando el grupo de cada fila
datos$gruposHC<-groups


#Morbilidad por departamento 
departamentos<- as.vector(todo$depto)
departamentos<-unique(departamentos)
resumen <- aggregate(todo$cantidad,by=list(todo$depto),FUN=length)
cantidad <- as.vector(resumen[,2])
barplot(cantidad, names.arg = departamentos,col =topo.colors(22),main = "morbilidad por departamento",las=2,cex.names = 1)


#Morbilidad por mes
Meses<- as.vector(todo$mes)
Meses<-unique(Meses)
resumen <- aggregate(todo$cantidad,by=list(todo$mes),FUN=length)
cantidad <- as.vector(resumen[,2])
barplot(cantidad, names.arg = Meses,col =topo.colors(6),main = "morbilidad por mes",las=2,cex.names = 1)


#Morbilidad por Genero
Genero<- as.vector(todo$genero)
Genero<-unique(Genero)
resumen <- aggregate(todo$cantidad,by=list(todo$genero),FUN=length)
cantidad <- as.vector(resumen[,2])
barplot(cantidad, names.arg = Genero,col =topo.colors(2),main = "morbilidad por genero",las=2,cex.names = 1)



#Morbilidad por Grupo de edad
Gedad<- as.vector(todo$grupoedad)
Gedad<-unique(Gedad)
resumen <- aggregate(todo$cantidad,by=list(todo$grupoedad),FUN=length)
cantidad <- as.vector(resumen[,2])
barplot(cantidad, names.arg = Gedad,col =topo.colors(17),main = "morbilidad por grupo de edad",las=2,cex.names = 1)

Gedad<- as.vector(todo$numero_grupoedad)
Gedad<-unique(Gedad)
resumen <- aggregate(todo$cantidad,by=list(todo$numero_grupoedad),FUN=length)
cantidad <- as.vector(resumen[,2])
barplot(cantidad, names.arg = Gedad,col =topo.colors(17),main = "morbilidad por grupo de edad",las=2,cex.names = 1)



library(ggplot2) #load ggplot2 library
todo$numero_dep <- factor(todo$numero_dep) # Create a categorical variable
todo$numero_gen <- factor(todo$numero_gen) # Create categorical variable
p <- ggplot(data = todo, aes(x=numero_dep, fill=numero_gen) ) + geom_bar() # Creates stacked bar chart
p <- p + xlab("Departamentos") + ggtitle("Genero por departamento") # Adds title and labels
p


library(ggplot2) #load ggplot2 library
todo$mes <- factor(todo$mes) # Create a categorical variable
todo$grupoedad <- factor(todo$grupoedad) # Create categorical variable
p <- ggplot(data = todo, aes(x=mes, fill=grupoedad) ) + geom_bar() # Creates stacked bar chart
p <- p + xlab("Meses") + ggtitle("grupo de edad por mes") # Adds title and labels
p

unique(todo$mes)


library(ggplot2) #load ggplot2 library
todo$numero_dep <- factor(todo$numero_dep) # Create a categorical variable
todo$grupoedad <- factor(todo$grupoedad) # Create categorical variable
p <- ggplot(data = todo, aes(x=numero_dep, fill=grupoedad) ) + geom_bar() # Creates stacked bar chart
p <- p + xlab("Departamentos") + ggtitle("Grupo de edad por departamento") # Adds title and labels
p

library(gplots)


library(ggplot2) #load ggplot2 library
todo$numero_dep <- factor(todo$numero_dep) # Create a categorical variable
todo$mes <- factor(todo$mes) # Create categorical variable
p <- ggplot(data = todo, aes(x=numero_dep, fill=mes) ) + geom_bar() # Creates stacked bar chart
p <- p + xlab("Departamentos") + ggtitle("mes  por departamento") # Adds title and labels
p



library(ggplot2) #load ggplot2 library
todo$numero_dep <- factor(todo$numero_dep) # Create a categorical variable
todo$numero_mes <- factor(todo$numero_mes) # Create categorical variable
p <- ggplot(data = todo,xlab(as.vector(todo$mes)), aes(x=numero_dep, fill=numero_mes) ) + geom_bar() # Creates stacked bar chart
p <- p + xlab("Departamentos") + ggtitle("Mes por departamento") # Adds title and labels
p

library(reshape2)
library(ggplot2)
ggplot(todo, aes(x = todo$depto, y= todo$cantidad, fill = todo$genero), xlab="Age Group") +
  geom_bar(stat="identity", width=.5, position = "dodge")  


#CIE10 y departamento
library(ggplot2) #load ggplot2 library
todo$numero_dep <- factor(todo$numero_dep) # Create a categorical variable
todo$numero_cie <- factor(todo$numero_cie) # Create categorical variable
p <- ggplot(data = todo, aes(x=numero_dep, fill=numero_cie) ) + geom_bar() # Creates stacked bar chart
p <- p + xlab("Departamentos") + ggtitle("CIE10  por departamento") # Adds title and labels
p

#kmeans clustering para CIE y departamento
#cluster k-medias

todo$pruebacie <- todo$numero_cie
todo$pruebadep <- todo$numero_dep

datos<- todo
todo
todo<- na.omit(todo)
km<-kmeans(todo[,16:17],3)

datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$diagnostico))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$diagnostico))*100
g3<- datos[datos$grupo==3,]
prop.table(table(g3$diagnostico))*100

plotcluster(todo[,16:17],km$cluster) #grafica la ubicaciÃ³n de los clusters
