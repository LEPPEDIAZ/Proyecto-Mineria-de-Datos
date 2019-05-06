#Prueba lectura y union 





library(foreign)
View(DQuno)
DQuno <- read.spss("2015i.sav",to.data.frame=TRUE,use.value.labels=FALSE)
DQdos <- read.spss("2015e.sav",to.data.frame=TRUE,use.value.labels=FALSE)
DDSuno <- read.spss("2016i.sav",to.data.frame=TRUE,use.value.labels=FALSE)
DDSdos <- read.spss("2016e.sav",to.data.frame=TRUE,use.value.labels=FALSE)
DDDuno <- read.spss("2017i.sav",to.data.frame=TRUE,use.value.labels=FALSE)
DDDdos <- read.spss("2017e.sav",to.data.frame=TRUE,use.value.labels=FALSE)
View(DQuno)

todo <- rbind(DQuno,DQdos,DDSuno,DDSdos,DDDuno,DDDdos)

library(dplyr)


names(DDDdos)
names(DQuno)
DQdos<- DQdos[,-3]
DQdos<- DQdos[,-10]
DQuno<- DQuno[,-9]

# Write CSV in R
write.csv(todo, file = "Proyecto.csv")

info <- read.csv("Proyecto.csv")


View(todo)

#tienen que leer el csv como "todo". En donde esta la "E" va la letra o string a verificar
#En donde esta el "4", va asi, el numero de la clasificacion
todo$clasificiacion <- ifelse(grepl("E",todo$CAUFIN),"4",todo$clasificiacion)


