todo <- read.csv("todo.csv")
summary(todo)
#histogramas
hist(todo$X)
hist(todo$AÃ.o)
hist(todo$Cantidad.total)
#tablas de normalidad
qqnorm(todo$X)
qqline(todo$X, col = "steelblue", lwd = 2)
qqnorm(todo$AÃ.o)
qqline(todo$AÃ.o, col = "steelblue", lwd = 2)
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
library(NbClust) #Para determinar el n�mero de clusters �ptimo
library(factoextra) #Para hacer gr�ficos bonitos de clustering
#gaussiano
mc<-Mclust(datos[,1:4],3)
plot(mc, what = "classification", main="MClust Classification")
datos$mxGau<-mc$classification
g1MC<-datos[datos$mxGau==1,]
g2MC<-datos[datos$mxGau==2,]
g3MC<-datos[datos$mxGau==3,]
#cluster k-medias
datos<- todo
irisCompleto<-todo[complete.cases(todo),]
km<-kmeans(todo[,1:4],3)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$diagnostico))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$diagnostico))*100
g3<- datos[datos$grupo==3,]
prop.table(table(g3$diagnostico))*100

plotcluster(iris[,1:4],km$cluster) #grafica la ubicación de los clusters
#Error in eigen(m, symmetric = TRUE) : infinite or missing values in 'x'
#c-means
my_data2 <- todo[, c(9, 1, 2, 7, 3, 4, 5, 6, 8)]
fcm<-cmeans(todo[,3:4],7)
datos$FCGrupos<-fcm$cluster
datos<-cbind(datos,fcm$membership)
#jerarquico
hc<-hclust(dist(my_data2[,1:4])) #Genera el clustering jerárquico de los datos
#Error: cannot allocate vector of size 2105.6 Gb
plot(hc) #Genera el dendograma
rect.hclust(hc,k=3) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=3) #corta el dendograma, determinando el grupo de cada fila
datos$gruposHC<-groups
#exploratorio
summary(todo)
orden <- order(todo$Cantidad.total, decreasing = T)
head(todo[orden,]$diagnostico,n=1)
#segunda pregunta
library(epiDisplay)
tab1(todo$diagnostico, sort.group = "decreasing", cum.percent = TRUE)
#enfermedad mas repetida
names(which.max(table(todo$diagnostico)))
#departamento mas suministrado
names(which.max(table(todo$Departamento)))
