todo <- read.csv("todo.csv")
summary(todo)
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
