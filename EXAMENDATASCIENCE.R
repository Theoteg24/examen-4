setwd('C:/Users/alvar/Desktop/EXAMEN DATASCIENCE') #Para que es esto?

library(dplyr)
library(visdat)
library(naniar)
library(VIM)
library(BBmisc)
library(discretization)
library(smoothmest)
library(readxl)

#0. Carga los datos en R

WHR <- read.csv("C:/Users/alvar/Downloads/world-happiness-report (1).csv")
names(WHR)[1]<-'Country_Name'

WHR_2021 <- read.csv("C:/Users/alvar/Downloads/world-happiness-report-2021 (1).csv")
names(WHR_2021)[1]<-'Country_Name'


#1. Realiza un adecuado discovering de los datos presentados y completa las columnas Dominio y Formato de la pestaña descripciones variables
#1??
str(WHR)
str(WHR_2021)

estad_HWR <- as.data.frame(summary(WHR[,-c(1)]))

distinct(WHR_2021[,c(1,2)])
Paises<- as.vector(distinct(as.data.frame(WHR_2021[,c(1)])))
Regiones <- as.vector(distinct(as.data.frame(WHR_2021[,c(2)])))
estad_WHR_2021 <- as.data.frame(summary(WHR_2021[,-c(1,2)]))

#2. Localiza las ausencias, dibuja su tipología y completa con el método que consideres más adecuado. Justifica tu método

##Trabajamos las ausencias
#WHR
#primero agrego los missing impl?citos
WHR <- tidyr::complete(WHR, Country_Name, year) #tmb esta fill que es si sale un pais y las de abajo no
#Para que hace eso?
View(WHR)
miss_case_summary(WHR)
miss_var_summary(WHR)

gg_miss_var(WHR, facet=year)

sum_ausencias_pais <- WHR %>%
  mutate(var=apply(is.na(WHR)[,-c(1,2)], 1, sum)) %>%
  group_by (Country_Name) %>%
  summarize(sum(var))

sum_ausencias_a?o <- WHR %>%
  mutate(var=apply(is.na(WHR)[,-c(1,2)], 1, sum)) %>%
  group_by (year) %>%
  summarize(sum(var))

vis_miss(WHR)

#las ausencias son de tipo MCAR, por lo que imputar es la mejor opci?n
#en este caso, un sistema de imputaci?n kNN utilizando como variables para
#medir la distancia el pais y el a?o encaja bien

for(i in (3:11)) {WHR <- kNN(WHR, colnames(WHR)[i], dist_var=c("year", "Country_Name"))}
vis_miss(WHR)

#WHR_2021
miss_case_summary(WHR_2021)
miss_var_summary(WHR_2021)

#En este caso solo hay una ausencia. Miramos donde
WHR_2021 %>%
  filter(is.na(Explained.by..Social.support)==TRUE)
#imputamos el valor ausente considerando otros paises que sean cercanos en los 
#tasas de explicaci?n que ofrecen el resto de valores
#KNN
WHR_2021 <- kNN(WHR_2021, "Explained.by..Social.support", k=10, dist_var=c("Explained.by..Log.GDP.per.capita", "Explained.by..Healthy.life.expectancy", "Explained.by..Freedom.to.make.life.choices", "Explained.by..Generosity" , "Explained.by..Perceptions.of.corruption", "Dystopia...residual"))
miss_var_summary(WHR_2021)
#HOTDECK
DF_IMP <- hotdeck (WHR, variable=colnames(WHR)) #Variable mas proxima


#3. Detecta, si exiten, los valores outliers de las variables y realiza trabajo de limpieza con ellos. 

#Empezamos con los outliers
boxplot(as.data.frame(WHR_2021[,-c(1,2)]))
boxplot(as.data.frame(WHR_2021[,-c(1,2,3)]))
boxplot(as.data.frame(WHR_2021[,-c(1,2,3,9)]))
boxplot(as.data.frame(WHR_2021[,c(9)]))

#Como trato los outliers?

#s? corrijo el Ladder.Score, xque tiene pinta que se han movido los decimales
WHR_2021[1,3]<-7.842
boxplot(as.data.frame(WHR_2021[,-c(1,2,9)]))

boxplot(as.data.frame(WHR[,-c(1,2)]))
boxplot(as.data.frame(WHR[,-c(1,2,6)]))
boxplot(as.data.frame(WHR[,c(6)]))

#la esperanza de vida en Hait? ha sido y sigue siendo un drama. Me niego a 
#corregir un dato proporciona tanta informaci?n

#4. Aplica normalización a la variable Log GDP per capita y discretiza con el método CAIM la variable Standard error of ladder score

#normalizo y discretizo
#La normalizacion sirve para redimensionar las variables
WHR_2021 <- cbind(WHR_2021[,1:6], normalize(WHR_2021[,c(7)], method='standardize'), WHR_2021[,8:20])
WHR <- cbind(WHR[,1:3], normalize(WHR[,c(4)], method='standardize'), WHR[,5:11]) #Por que normalizas????
View(WHR_2021)
#La discretización sirve para transformar una variable continua (Muchas clases) en una discreta (Con menos clases)
CAIM <- disc.Topdown(WHR_2021[,c(4,3)], method=1)
discretizacion_CAIM <- as.data.frame(CAIM$Disc.data)
cortes_CAIM <- as.data.frame(CAIM$cutp)
#como funciona lo de method=...?

#5. Anonimiza la consulta de Generosity por país y año aplicando criterio de Laplace con un epsilon de 1,5

#anonimizo consulta
#1 manera mas sencilla
DF_Anonim <- DF %>%
  mutate(var=1:length(DF))
#2 manera
#Media(nada)
Generosity_media <- WHR %>%
  group_by(year, Country_Name) %>%
  summarize(Generosity_media=mean(Generosity))

#Varianza
Generosity_media <- WHR %>%
  group_by(Negative.affect) %>%
  summarize(Generosity_media=var(Generosity))

#Conteo no va
Generosity_media <- WHR %>%
  group_by(year, Country_Name) %>%
  summarize(Generosity_media=count(Generosity))

epsilon=1000
a<-max(WHR$Generosity)
b<-min(WHR$Generosity)
n<-nrow(WHR)
gs <- (b-a)/n #Por que aplica el de la media? Si te piden proporcion, varianza o conteo...

Generosity_media_anon <- cbind(as.data.frame(Generosity_media[,c(1,2)]), round(rdoublex(1949, Generosity_media[,3], gs/epsilon),2))
#Por que 1949 y 2? Ademas, si no es media hay que cambiar tambien "generosity__..."


#6. Genera un repositorio local Git dosde solo guardes los ficheros de las sigueintes tipologías: .R, .ignore y README.md. A continuación, clónalo en un repositorio remoto
