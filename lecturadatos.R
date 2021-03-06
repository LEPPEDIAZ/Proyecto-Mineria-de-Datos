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

todo <- read.csv("Proyecto.csv")

View(todo)

#tienen que leer el csv como "todo". En donde esta la "E" va la letra o string a verificar
#En donde esta el "4", va asi, el numero de la clasificacion
todo$clasificiacion <- ifelse(grepl("A",todo$CAUFIN),"1",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("B",todo$CAUFIN),"1",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("C",todo$CAUFIN),"2",todo$clasificiacion)
#todo$clasificiacion <- ifelse(grepl("D",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D00",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D01",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D02",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D03",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D04",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D05",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D06",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D07",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D08",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D09",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D10",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D11",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D12",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D13",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D14",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D15",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D16",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D17",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D18",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D19",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D20",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D21",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D22",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D23",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D24",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D25",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D26",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D27",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D28",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D29",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D30",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D31",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D32",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D33",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D34",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D35",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D36",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D37",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D38",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D39",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D40",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D41",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D42",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D43",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D44",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D45",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D46",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D47",todo$CAUFIN),"2",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D48",todo$CAUFIN),"2",todo$clasificiacion)

todo$clasificiacion <- ifelse(grepl("D50",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D51",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D52",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D53",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D54",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D55",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D56",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D57",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D58",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D59",todo$CAUFIN),"3",todo$clasificiacion)

todo$clasificiacion <- ifelse(grepl("D60",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D61",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D62",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D63",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D64",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D65",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D66",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D67",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D68",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D69",todo$CAUFIN),"3",todo$clasificiacion)

todo$clasificiacion <- ifelse(grepl("D70",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D71",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D72",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D73",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D74",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D75",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D76",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D77",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D78",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D79",todo$CAUFIN),"3",todo$clasificiacion)

todo$clasificiacion <- ifelse(grepl("D80",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D81",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D82",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D83",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D84",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D85",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D86",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D87",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D88",todo$CAUFIN),"3",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("D89",todo$CAUFIN),"3",todo$clasificiacion)







#De la D
todo$clasificiacion <- ifelse(grepl("E",todo$CAUFIN),"4",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("F",todo$CAUFIN),"5",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("G",todo$CAUFIN),"6",todo$clasificiacion)
#todo$clasificiacion <- ifelse(grepl("H",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H00",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H01",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H02",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H03",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H04",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H05",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H06",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H07",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H08",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H09",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H10",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H11",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H12",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H13",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H14",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H15",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H16",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H17",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H18",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H19",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H20",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H21",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H22",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H23",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H24",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H25",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H26",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H27",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H28",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H29",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H30",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H31",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H32",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H33",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H34",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H35",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H36",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H37",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H38",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H39",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H40",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H41",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H42",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H43",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H44",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H45",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H46",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H47",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H48",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H50",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H51",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H52",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H53",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H54",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H55",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H56",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H57",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H58",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H59",todo$CAUFIN),"7",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H60",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H61",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H62",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H63",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H64",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H65",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H66",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H67",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H68",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H69",todo$CAUFIN),"8",todo$clasificiacion)

todo$clasificiacion <- ifelse(grepl("H70",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H71",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H72",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H73",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H74",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H75",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H76",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H77",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H78",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H79",todo$CAUFIN),"8",todo$clasificiacion)

todo$clasificiacion <- ifelse(grepl("H80",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H81",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H82",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H83",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H84",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H85",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H86",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H87",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H88",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H89",todo$CAUFIN),"8",todo$clasificiacion)

todo$clasificiacion <- ifelse(grepl("H90",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H91",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H92",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H93",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H94",todo$CAUFIN),"8",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("H95",todo$CAUFIN),"8",todo$clasificiacion)





#De la H







todo$clasificiacion <- ifelse(grepl("I",todo$CAUFIN),"9",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("J",todo$CAUFIN),"10",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("K",todo$CAUFIN),"11",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("L",todo$CAUFIN),"12",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("M",todo$CAUFIN),"13",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("N",todo$CAUFIN),"14",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("O",todo$CAUFIN),"15",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("P",todo$CAUFIN),"16",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("Q",todo$CAUFIN),"17",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("R",todo$CAUFIN),"18",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("S",todo$CAUFIN),"19",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("T",todo$CAUFIN),"19",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("V",todo$CAUFIN),"20",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("Y",todo$CAUFIN),"20",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("Z",todo$CAUFIN),"21",todo$clasificiacion)
todo$clasificiacion <- ifelse(grepl("U",todo$CAUFIN),"22",todo$clasificiacion)


View(todo)

View(todo)
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "1") , ]
View(todo1)
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "2") , ]
View(todo1)
View(todo1)
View(todo1)
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "3") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "4") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "5") , ]
View(todo1)
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "6") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "7") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "8") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "9") , ]
View(todo)
View(todo1)
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "13") , ]
View(todo1)
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "15") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "16") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "17") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "18") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "19") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "20") , ]
View(todo1)
todo1 <- todo3
View(todo1)
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "1") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "2") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "3") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "4") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "5") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "6") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "7") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "8") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "8") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "9") , ]
View(todo1)
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "13") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "15") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "16") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "17") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "18") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "19") , ]
todo1 <- todo1[ which( ! todo1$clasificiacion %in% "20") , ]
View(todo1)

