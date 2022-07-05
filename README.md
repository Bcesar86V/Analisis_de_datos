# Diplomado

## Analisis_de_datos

Repositorio analisis de datos 
#Trabajo laboratorio

#Creando data frame
Vivienda<-read.csv("../Escritorio/Artículos/Viviendas.csv", sep=",")

#Dimensiones del data set
dim(Vivienda)

#¿Qué describe cada línea que corresponde a sus dos últimos digitos de su RUT? 
#Una vivienda en la comuna de Nancagua, con un terreno de 118 U^2 y 36 U^2 de construccion, esta avaluada en 2880289 millones de pesos
#Una vivienda en la comuna de Valpraiso, con 46 U^2 de construccion, de la cual se desconoce su terreno, tiene un avaluo de 5278543 millones de pesos.


#Columnas tipo factor
str(Vivienda)
#En total son 6 variables de tipo factor. Si bien es cierto, R solo considera dos de ellas como tipo factor, hay 4 variables que en principio son consideradas como de tipo entero (enaj, act_adq, persona_jur_enaj, persona_jur_ad), 
#pero este tipo de variables son de tipo ficticia. Es decir, se encuentran codificadas, 1= true  0=False.

#Valores NA
my_na<-is.na(Vivienda)
table(my_na)
sum(my_na)
#No hay presencia de NA en la base de datos

#10 comunas con mayor avaluo promedio
install.packages("dplyr")
library(dplyr)
Com_May<-Vivienda%>%group_by(nombre_comuna)%>%summarise(prom=mean(avaluo))%>%arrange(desc(prom))%>%top_n(10)
Com_May

#Obtenga la media, mediana, percentil 25 y 75 para dos comunas a su elección con respecto a la variable terreno. Compare los resultados y comente que se puede decir al respecto de estas comunas.
#He decidido sustituir los valores ceros por NA y luego crear nueva columna
#Donde los NA seran sustituidos por la media de la columna terreno sin los NA
Vivienda$terreno[Vivienda$terreno==0]<-NA
Vivienda$terreno.limpio<-ifelse(is.na(Vivienda$terreno), mean(Vivienda$terreno, na.rm=TRUE), Vivienda$terreno)

Valores<-Vivienda%>%group_by(nombre_comuna)%>%filter(nombre_comuna %in% c("MAIPU", "COLCHANE"))%>%summarise(prom=mean(terreno.limpio), med=median(terreno.limpio), Cuartil1=quantile(terreno.limpio, probs=c(0.25)), Cuartil3=quantile(terreno.limpio, probs=c(0.75)))
Valores 

#Los resultados no arrojan que en general, una vivienda en la comuna de Colchane tiene un terreno mayor a una vivienda en Maipu
#Teniendo asi, el 25% de las viviendas en Colchane, casi el doble del terreno de lo que pudiesen tener el 25% de las viviendas en Maipu


#Genere un dataframe que muestre la cantidad de observaciones, promedio terreno y promedio construcción por comuna y año.
#Primero reemplazare los valores ceros de la columna construccion por NA y luego construire una nueva columna
#Donde los valores NA sean reemplazados por el promedio de la columna construccion sin NA
Viviendas<-read.csv("../Escritorio/Artículos/Viviendas2.csv", sep=";")
Viviendas$construccion[Viviendas$construccion==0]<-NA
Viviendas$terreno[Viviendas$terreno==0]<-NA
Viviendas$terreno.limpio<-ifelse(is.na(Viviendas$terreno), mean(Viviendas$terreno, na.rm=TRUE), Viviendas$terreno)
Viviendas$construccion.limpio<-ifelse(is.na(Viviendas$construccion), mean(Viviendas$construccion, na.rm=TRUE), Viviendas$construccion)
library(tidyr)

Agrupacion<-Viviendas%>%group_by(nombre_comuna, anho2)%>%summarise(prom.terreno=mean(terreno.limpio), media.constr=mean(construccion.limpio))
Agrupacion


#Realice un gráfico de barras de los resultados anteriores (pregunta 7). 
#Etiquete correctamente los ejes x e y. Explique el gráfico
install.packages("ggplot2")
library(ggplot2)
ggplot(data_prome_TC[data_prome_TC$nombre_comuna==c("ARICA","MELIPILLA","VILLARRICA","CONCEPCION"),],aes(x =nombre_comuna, y=prom_terreno))+ 
  geom_bar(stat="identity", fill="darkred", alpha=.6, width=.4)+
  scale_x_discrete(name = "Nombre Comuna")+
  scale_y_continuous(name="promedio m2 terreno")+
  ggtitle(label = "Promedio m2 terreno  por comuna")


#Realice un gráfico que muestre la relación entre avaluo y construcción. 
#¿Es posible ver valor atípicos (outliers)?
library(ggplot2)
Viviendas$avaluo2<-Viviendas$avaluo/1000
plot(avaluo2~construccion.limpio, data=Viviendas, xlab="Construccion", ylab="avaluo vivienda", main="grafico de relacion entre construccion y avaluo")  

#La relacion existente entre construccion y avaluo es directa. Es decir, mientras mayor construccion tenga la vivienda, su avaluo sera mayor
#ademas, en el grafico es posible apreciar la presencia de valores "outliers", estan muy alejados del rango de datos
#aunque para confirmar esto, es mejor aplicar procedimientos analiticos.


#Agregue una nueva columna al dataset que mida la proporción entre avaluo y terreno.¿Qué representa este indicador? 
#Genere un boxplot que permita visualizar este valor por región. 
#¿Cuál región presenta una mayor valor? Nota :Debe asignarle una región a cada comuna, se les adjunta una base como ayuda.

#Como hay terrenos que tienen 0 m2, la división entre un valor y cero se indetermina, estos casos no se contaran más adelante, eliminar.



df$avaluo <- as.numeric(df$avaluo)
df$terreno <- as.numeric(df$terreno)
df$avaluo_terreno <- df$avaluo/df$terreno
df$avaluo_terreno[is.nan(df$avaluo_terreno)] <- 0
df$avaluo_terreno[is.na(df$avaluo_terreno)] <- 0
df$avaluo_terreno[is.infinite(df$avaluo_terreno)] <- 0


#Ahora se cargará la base comunas para generar un merge entre comuna y region.


df_region<- read_xls("C:/Users/dlope/Desktop/Diploma_MAD/Fundamentos/Clases/Clase_1_2/Lab_01/CUT_2018_v04.xls")

#Se pasan todos los caracteres a mayúsculas para lograr juntar las bases


df_region <- mutate_if(df_region,is.character, toupper)

df_region$nombre_comuna <- df_region$Nombre_Comuna



#Remover los acentos del data set para lograr unificar las bases 


df_region$nombre_comuna <- iconv(df_region$nombre_comuna, from = 'UTF-8', to = 'ASCII//TRANSLIT')


#Corregir algunas comunas manuelamente para unificar las bases


df_region[df_region$nombre_comuna== "AISEN",] <- "AYSEN"
df_region[df_region$nombre_comuna== "COIHAIQUE",] <- "COYHAIQUE"  
df_region[df_region$nombre_comuna== "CALERA",] <- "LA CALERA"
df_region[df_region$nombre_comuna== "MARCHIHUE",] <- "MARCHIGUE"
df_region[df_region$nombre_comuna== "O'HIGGINS",] <- "OHIGGINS"
df_region[df_region$nombre_comuna== "PAIGUANO",] <- "PAIHUANO"
df_region[df_region$nombre_comuna== "MOSTAZAL",] <- "SAN FRANCISCO DE MOSTAZAL"
df[df$nombre_comuna== "SANTIAGO OESTE",] <- "SANTIAGO"
df[df$nombre_comuna== "SANTIAGO SUR",] <- "SANTIAGO"
df_region[df_region$nombre_comuna== "TILTIL",] <- "TIL-TIL"
df_region[df_region$nombre_comuna== "TREGUACO",] <- "TREHUACO"
df_region[df_region$nombre_comuna== "LLAILLAY",] <- "LLAY-LLAY"


#La idea es conservar todo lo que está al lado izquierdo y la intersección, por eso se utiliza all.x

db_comparativo<- merge(df, df_region, by="nombre_comuna", all.x = TRUE)


#Revisar nombres de comunas en base general.


table(df$nombre_comuna)


#Revisar nombres de comunas en base de regiones.


table(df_region$nombre_comuna)


#Reviso los datos que no se pudieron juntar, y que tienen NA, cuando no haya ninguno, termino al imputación por regiones.



df_na<-db_comparativo[is.na(db_comparativo$`Nombre Región`)==TRUE,]



#Ahora se genera el box-plot, corroboramos que sea número el que se graficara


str(db_comparativo$avaluo)
str(db_comparativo$avaluo_terreno)


df_avaluo_terreno <- db_comparativo[db_comparativo$avaluo_terreno!=0,]%>%group_by(nombre_comuna)%>%summarise(prom_avaluo_terr=mean(avaluo_terreno))%>%arrange(desc(prom_avaluo_terr))

df_avaluo_terreno[1:10,]


#Se toman algunas regiones, entre las regiones que se seleccionaron, la Región Metropolitana tiene mayor promedio comunal del ratio avaluo sobre m2 de los terrenos, lo que indica es que el suelo es mucho valioso en esta región, pero tambien se aprecia que existen outlier que podrían ser eliminados para tener un mejor comparativo en el gráfico.

db_comparativo$Nombre_Region <- db_comparativo$`Nombre Región`


ggplot(db_comparativo[db_comparativo$Nombre_Region==c("VALPARAÍSO","METROPOLITANA DE SANTIAGO","MAULE","BIOBÍO"),], aes(x=Nombre_Region, y=avaluo_terreno)) + 
  geom_boxplot()

#Se filta la data para ver mejor la visualización. En este caso se aprecia mejor que la Region Metropolitana  es superior a las otras regiones.

db_comparativo <- db_comparativo[db_comparativo$avaluo_terreno<200000 & db_comparativo$avaluo_terreno>0,]

ggplot(db_comparativo[db_comparativo$Nombre_Region==c("VALPARAÍSO","METROPOLITANA DE SANTIAGO","MAULE","BIOBÍO"),], aes(x=Nombre_Region, y=avaluo_terreno)) + 
  geom_boxplot()
