#arboles de decision
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)

datos <- todofinal

# variable respuesta la clase de la flor
porciento <- 10/100

set.seed(123)

trainRowsNumber<-sample(1:nrow(datos),porciento*nrow(datos))
train<-datos[trainRowsNumber,]
test<-datos[-trainRowsNumber,]
dt_model<-rpart(clasificiacion~.,train,method = "class")
plot(dt_model);text(dt_model)
prp(dt_model)
rpart.plot(dt_model)

head(test)
prediccion <- predict(dt_model, newdata = test[12:13])

#Apply: Para cada fila, determina el nombre de la columna del valor máximo entre los tres valores de una fila
columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
test$prediccion<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción

cfm<-confusionMatrix(test$prediccion,test$clasificiacion)
cfm

#con caret
ct<-trainControl(method = "cv",train[,12:13],number=3, verboseIter=T)
modelorf<-train(clasifiacion~.,data=train,method="rf",trControl = ct)
prediccionRF<-predict(modelorf,newdata = test[,12:13])
testCompleto$predRFCaret<-prediccionRF
cfmCaret <- confusionMatrix(testCompleto$predRFCaret,testCompleto$clasificiacion)

#con random forest
modeloRF1<-randomForest(clasificiacion~.,data=train)
prediccionRF1<-predict(modeloRF1,newdata = test)
testCompleto<-test
testCompleto$predRF<-prediccionRF1
cfmRandomForest <- confusionMatrix(testCompleto$predRF, testCompleto$clasificiacion)