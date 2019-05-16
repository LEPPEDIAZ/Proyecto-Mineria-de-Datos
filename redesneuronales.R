#redes neuronales
library(caret)
library(nnet)
library(RWeka)
library(neural)
library(dummy)
library(neuralnet)
library(doParallel)

porcentaje<-0.7
datos<-todofinal
set.seed(123)
prueba <- todofinal[,12:13]
prueba[2] <- todofinal[2]
datos<- prueba
corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]
#-------------------------------------------------

# Red Neuronal con caret

#-------------------------------------------------

registerDoParallel(cores = 2)
y <- as.factor(make.names(datos$clasificiacion))
datos$TARGET <-  y
cat("\n## Removing the constants features.\n")
for (f in names(datos)) {
  if (length(unique(datos[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    data[[f]] <- NULL
    test[[f]] <- NULL
  }
}

##### Removing identical features
features_pair <- combn(names(datos), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    datos[[f1]] <- factor(datos[[f1]])
    datos[[f2]] <- factor(datos[[f2]])
    if (all(datos[[f1]] == datos[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(datos), toRemove)
data <- datos[, feature.names]
test <- test[, feature.names[feature.names != 'TARGET']]

inTrain <- createDataPartition(datos$TARGET, p = 3/4)[[1]]
training <- datos[inTrain,]
testing <- datos[-inTrain,]

numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = multiClassSummary, preProcOptions = list(thresh = 0.9, ICAcomp = 3, k = 5))
fit2 <- train(TARGET ~Temporada , data = training, method = 'nnet', preProcess = c('center', 'scale'), trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.001)))
results1 <- predict(fit2, newdata=training)
conf1 <- confusionMatrix(results1, training$TARGET)
results2 <- predict(fit2, newdata=testing)
conf2 <- confusionMatrix(results2, testing$TARGET)
probs <- predict(fit2, newdata=test, type='prob')

#prueba 2
data <- todofinal
prueba <- data[,12:12]
prueba[12] <- data[12]
data<- prueba

# Random sampling
samplesize = 0.70 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )
# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]
## Scale data for neural network
max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

## Fit neural network 
# install library
#install.packages("neuralnet")
library(neuralnet)
# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]
# fit neural network
set.seed(2)
NN = neuralnet(clasificiacion ~ Temporada , trainNN, hidden = 3, linear.output = F ,threshold = 0.01,
               
               stepmax = 1e+05, rep = 1)
# plot neural network
plot(NN)

## Prediction using neural network
predict_testNN = compute(NN, testNN[,c(12:13)])
predict_testNN = (predict_testNN$net.result * (max(data$AdoptionSpeed) - min(data$AdoptionSpeed))) + min(data$AdoptionSpeed)
plot(datatest$clasificiacion, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")
abline(0,1)
# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$clasificiacion - predict_testNN)^2) / nrow(datatest)) ^ 0.5


