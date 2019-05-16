library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

data<-read.csv("estudioenfermedad.csv")
set.seed(1649)
data$clasificiacion = factor(data$clasificiacion)

training <- sample_frac(data, .7)
testing<- setdiff(data, training)

arbol_1 <- rpart(formula = clasificiacion ~ ., data = training)

rpart.plot(arbol_1)
prediccion_1 <- predict(arbol_1, newdata = testing, type = "class")

confusionMatrix(as.vector(prediccion_1), testing[["clasificiacion"]])
