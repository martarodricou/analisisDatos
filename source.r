
#Carga de datos
datosTitanicTrain <- read.csv('train.csv',stringsAsFactors = FALSE, header = TRUE, strip.white = TRUE)
datosTest <- read.csv('test.csv',stringsAsFactors = FALSE, header = TRUE, strip.white = TRUE)
datosSubmision <- read.csv('gender_submission.csv',stringsAsFactors = FALSE, header = TRUE, strip.white = TRUE)


#Integracion

datosTest["Survived"] <- datosSubmision["Survived"]
head(datosTest)

datosTest = datosTest[, c(1,12,2,3,4,5,6,7,8,9,10,11)]

datosTitanic <- rbind(datosTitanicTrain, datosTest)

head(datosTitanic)
tail(datosTitanic)


#Reducción
datosTitanic <- datosTitanic[, -11]
datosTitanic <- datosTitanic[, -10]
datosTitanic <- datosTitanic[, -9]
datosTitanic <- datosTitanic[, -4]
datosTitanic <- datosTitanic[, -1]

#Seleccion

datosTitanic$alone <- with(datosTitanic, ifelse(SibSp==0 & Parch==0, "yes", "no"))


# Verificamos la estructura del conjunto de datos

str(datosTitanic)
head(datosTitanic)
summary(datosTitanic)
#Descripcion de variables

#Eliminar datos nulos o vacios

colSums(is.na(datosTitanic))
colSums(datosTitanic=="")


datosTitanic$Embarked[datosTitanic$Embarked==""]="Desconocido"


shapiro.test(datosTitanic$Age[!is.na(datosTitanic$Age)])

datosTitanic$Age[is.na(datosTitanic$Age)] <- mean(datosTitanic$Age,na.rm=T)


#Listado de los tipos de datos -> vaolres extremos

unique(datosTitanic$Embarked)
unique(datosTitanic$Survived)
unique(datosTitanic$Pclass)
unique(datosTitanic$Sex)
unique(datosTitanic$SibSp)
unique(datosTitanic$Parch)

boxplot(datosTitanic$Age)
boxplot(datosTitanic$SibSp)

#Normalizacion de datos

embarked <- c("S","C","Q", "Desconocido")
sex <- c("male","female")
alone <- c("yes","no")

datosNorm = datosTitanic

datosNorm$Embarked <- match(datosTitanic$Embarked, embarked)
datosNorm$Sex<- match(datosTitanic$Sex, sex)
datosNorm$alone <- match(datosTitanic$alone, alone)
head(datosNorm)

#funcion para normalizar entre 0 y 1
normalizar <- function(x)
{
  return ((x - min(x)) / (max(x) - min(x)))
}


datosNorm$Age <- normalizar(datosNorm$Age)
datosNorm$SibSp <- normalizar(datosNorm$SibSp)
datosNorm$Parch <- normalizar(datosNorm$Parch)
library(ggplot2)
library(dplyr)


hist(datosTitanic$Age)

library(nortest)
shapiro.test(datosTitanic$Age)


library(DescTools)
shapiro.test(
  BoxCox(datosTitanic$Age, lambda = BoxCoxLambda(datosTitanic$Age))
)
