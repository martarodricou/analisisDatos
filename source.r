


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

##Histograma de edad
hist(datosTitanic$Age)


##Comprobamos la normalidad de los datos
library(nortest)
shapiro.test(datosTitanic$Age)



##Aplicamos la transformación de boxcox para normalizar los datos, pero no conseguimos normalizar los datos
library(DescTools)
shapiro.test(
  BoxCox(datosTitanic$Age, lambda = BoxCoxLambda(datosTitanic$Age))
)




##otro test normalidad en la distribución de la variable
ks.test(datosTitanic$Age, pnorm, mean(datosTitanic$Age), sd(datosTitanic$Age))



######################################################################################################################################

#Repetimos el proceso de transformación de datosTitanic hasta el punto de sustituir los nulos en Age para comprobar si al eliminar los registros nulos la variable tiene una distribución normal
datosTitanic2 <- rbind(datosTitanicTrain, datosTest)


datosTitanic2 <- datosTitanic2[, -11]
datosTitanic2 <- datosTitanic2[, -10]
datosTitanic2 <- datosTitanic2[, -9]
datosTitanic2 <- datosTitanic2[, -4]
datosTitanic2 <- datosTitanic2[, -1]


datosTitanic2$alone <- with(datosTitanic2, ifelse(SibSp==0 & Parch==0, "yes", "no"))

datosTitanic2$Embarked[datosTitanic$Embarked==""]="Desconocido"



##Aplicamos text shapiro a los datos no vacíos
shapiro.test(datosTitanic2$Age[!is.na(datosTitanic$Age)])


##Lo aplicamos tras aplicarle la transformación BoxCox
shapiro.test(
  BoxCox(datosTitanic2$Age[!is.na(datosTitanic2$Age)], lambda = BoxCoxLambda(datosTitanic2$Age[!is.na(datosTitanic2$Age)]))
)



ks.test(datosTitanic$Age, pnorm, mean(datosTitanic$Age), sd(datosTitanic$Age))








#######homocedasticidad sin distribución normal

fligner.test( Age ~ Survived, data = datosNorm )
##menor que 0,05 varianza distinta para el grupo que sobrevive y el que no






wilcox.test(Age ~ Survived, data = datosNorm )
##No se obserban diferencias estadisticamente significativas con la variable Age


chisq.test(table(datosNorm$Survived, datosNorm$Pclass))
##Se observan diferencias

chisq.test(table(datosNorm$Survived, datosNorm$Sex))
##Se observan diferencias


chisq.test(table(datosNorm$Survived, datosNorm$Embarked))
##Se observan diferencias

chisq.test(table(datosNorm$Survived, datosNorm$alone))
##Se observan diferencias


##No se aplica para SibSp ni Parch ya que son variables numéricas y chisq.test se aplica para variables categoricas







##Correlación
cor.test(datosNorm$Survived, datosNorm$Age, method="spearman")
##no están relacionadas


library(corrplot)
corrplot(cor(datosNorm), type="upper", method="ellipse", tl.cex=0.9)

##Gran relación con sex (directamente proporcional), algo de relación con Pclass (inversamente proporcional)




#División de los datos en text y validación
library(rminer)
h <- holdout(datosNorm$Survived, ratio=2/3, mode="stratified")
titanic_train <- datosNorm[h$tr,]
titanic_test <- datosNorm[h$ts,]


print(table(titanic_train$Survived))
print(table(titanic_test$Survived))

##La proporcion de supervivientes se mantiene parecida en los dos conjuntos



##Regresión logística 
modelo1 <- glm(Survived ~ Pclass+Sex+SibSp+Parch+Embarked+Age, data=titanic_train, family="binomial")

summary(modelo1)


pred_log <- predict(modelo1, newdata=titanic_test)


table(titanic_test$Survived, pred_log > 0.5)




##Metodo de clasificacion random forest validación cruzada con 5 folds
train_control <- trainControl(method="cv", number = 5)
modelo_rand_for <- train(Survived~., data = titanic_train, method="rf", trControl = train_control)


titanic_pred <- predict(modelo_rand_for, newdata=titanic_test)


table(titanic_test$Survived, titanic_pred > 0.5)





library(tidyverse)

datosNorm %>%
  gather(Attributes, value, 1:8) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_density(colour="black", alpha=0.5, show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Valores", y="Densidad",
       title="Gráfica de densidad") +
  theme_bw()


##Las variables Age, Parch y SibSp forman picos, por lo tanto se podrán utilizar en la predicción de la supervivencia. Las demás variables están más uniformemente distribuidas y no aportan gran información a la hora de dividir los datos



