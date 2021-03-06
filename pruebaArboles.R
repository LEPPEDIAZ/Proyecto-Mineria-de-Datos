todo <- read.csv("estudioenfermedad.csv")

library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
library(MASS)
library(rpart)
library(caTools)

library(party)

plot(todo$Temporada,todo$clasificiacion)

prueba = lm(clasificiacion ~ MES, data=todo)
summary(prueba)


pruebas = rpart(clasificiacion ~ Temporada+SEXO+PERIODOEDA+EDAD+MES, data=todo)
prp(pruebas)


set.seed(123)
muestra <- sample(1:nrow(todo),0.7*nrow(todo))

train <- todo[muestra,] #70% entrenamiento
test<- todo[-muestra,] #30% prueba


tree = rpart(clasificiacion ~ AÑO + MES + SEXO + GRUPETNICO + EDAD + PERIODOEDA + DEPTORESIDEN + Temporada , data=train)
prp(tree)

tree.pred = predict(tree, newdata=test)
tree.sse = sum((tree.pred - test$clasificiacion)^2)
tree.sse



########################OTRASPRUEBAS
fit <- rpart(clasificiacion~AÑO + MES + SEXO + GRUPETNICO + EDAD + PERIODOEDA  + Temporada, data = train, method = 'class')

fit <- rpart(clasificiacion~.-clasificiacion, data = train, method = 'class')

rpart.plot(fit)

predict_unseen <-predict(fit, test, type = 'class')
table_mat <- table(test$clasificiacion, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

#MAS PRUEBAS -------------------------------------------------------------------
todo <- read.csv("estudioenfermedad.csv")

# Create the input data frame.
input.dat <- todo

# Give the chart file a name.
png(file = "decision_tree.png")

# Create the tree.
output.tree <- ctree(
  clasificiacion ~ AÑO + MES + SEXO + GRUPETNICO + EDAD + PERIODOEDA  + Temporada, 
  data = input.dat)

# Plot the tree.
plot(output.tree)

# Save the file.
dev.off()
