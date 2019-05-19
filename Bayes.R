library(e1071)
library(caret)
porcentaje<-0.7
datos<-todofinal
set.seed(123)

corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

modelo<-naiveBayes(Temporada~.,data=train)

predBayes<-predict(as.factor(modelo), newdata = test)
table(test$Temporada)
confusionMatrix(predBayes,test$Temporada)
confusionMatrix(as.factor(predBayes),as.factor(test$Temporada))

#bayes ejemplo 2
nb_model = naiveBayes(as.factor(clasificiacion) ~., data=todofinal)
modelPred <- predict(nb_model, todofinal)
modelPred <- as.factor(modelPred)
cMatrix <- table(modelPred, todofinal$Temporada)
plot(cMatrix)
confusionMatrix(cMatrix)
#bayes ejemplo 2
nb_model = naiveBayes(as.factor(clasificiacion) ~., data=todofinal)
modelPred <- predict(nb_model, todofinal)
modelPred <- as.factor(modelPred)
cMatrix <- table(modelPred, todofinal$clasificiacion)
plot(cMatrix)
confusionMatrix(cMatrix)
#bayes ejemplo 3
nb_model = naiveBayes(as.factor(Temporada) ~., data=todofinal)
modelPred <- predict(nb_model, todofinal)
modelPred <- as.factor(modelPred)
cMatrix <- table(modelPred, todofinal$Temporada)
plot(cMatrix)
confusionMatrix(cMatrix)