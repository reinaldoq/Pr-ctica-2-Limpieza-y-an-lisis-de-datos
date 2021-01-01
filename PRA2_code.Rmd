---
title: "R Notebook"
output:
  html_notebook: default
  pdf_document: default
---


***
# ELECCIÓN JUEGO DE DATOS
***
Solo en 2012, muerieron más de 17 millones de personas en el mundo por enfermedades cardiovasculares, según datos oficiales de la Organización Mundial de la Salud, convirtiéndose en una de las principales causa de denfunción a nivel mundial. Las enfermedades cardiovasculares son un conjunto del trastorno del corazón y los vasos sanguíneos. 

El data set elegido ha sido "Heart Disease Data Set" obtenidos de distintas fuentes: Cleveland Clinical Foundation, Instituto de Cardiología de Hungria, V.A Centro Médico y la Universidad de Zurich. El motivo por el que elegí este data set, o más concretamente, este campo de estudio que son las enfermedades cardiovasculares, fue porque quería comprobar cómo los avances que ofrece el análisis de datos pueden ser primordiales a la hora de identificar de manera precoz la presencia de enfermedades cardiovasculares en los pacientes. La ventaja de usar la metodología que ofrece el aprendizaje automático es que puede manejar una cantidad de datos ingente y obtenidos de un centenar de fuentes diferentes, lo que nos deja con unos datos variodos a la par que complejos, puesto que los resultados obtenidos de las distintas pruebas o análisis médicos resultan difícil de ser analizados a grande escala.


El conjunto de datos Utilizado cuenta con las siguientes variables:
1. (age) age: age in years
2. (sex) sex (1 = male; 0 = female)
3. (cp) chest pain type
-- Value 1: typical angina
-- Value 2: atypical angina
-- Value 3: non-anginal pain
-- Value 4: asymptomatic
4. (trestbps) resting blood pressure (in mm Hg on admission to the hospital)
5. (chol) serum cholestoral in mg/dl
6. (fbs) (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)
7. (restecg) resting electrocardiographic results
-- Value 0: normal
-- Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)
-- Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria
8. (thalach) maximum heart rate achieved
9. (exang) exercise induced angina (1 = yes; 0 = no)
10. (oldpeak) ST depression induced by exercise relative to rest
11. (slope) the slope of the peak exercise ST segment
-- Value 1: upsloping
-- Value 2: flat
-- Value 3: downsloping
12. (ca) number of major vessels (0-3) colored by flourosopy
13. (thal) 3 = normal; 6 = fixed defect; 7 = reversable defect
14. (goal) (the predicted attribute) diagnosis of heart disease (angiographic disease status)
-- Value 0: < 50% diameter narrowing
-- Value 1: > 50% diameter narrowing
(in any major vessel: attributes 59 through 68 are vessels)

Con este estudio, pretendo lograr un análisis preeliminar de cara a realizar diferentes actividades con ellos en el futuro. En primera instancia, cargaremos y transformaremos los datos. Luego haremos un análisis exploratorio en el que identificaremos tendencias y anomalías. Podremos analizar la correlación que hay entre los distintos síntomas (las distintas variables) que nos permitirán determinar qué datos son los más relevantes a la hora de determinar qué pacientes pueden o están desarrollando una enfermedad cardiovascular. Nos centraremos sobre todo en saber si hay alguna diferencia entre los síntomas o resultados de las pruebas en las mujeres con respecto a los de los hombres.


***
# CARGA DE DATOS
***
```{r}

# cargamos los datos
cleveland <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data',na = "?",stringsAsFactors = FALSE, header = FALSE)
hungarian <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.hungarian.data',na = "?",stringsAsFactors = FALSE, header = FALSE)
switzer <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.switzerland.data',na = "?",stringsAsFactors = FALSE, header = FALSE)
longbeach <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.va.data',na = "?",stringsAsFactors = FALSE, header = FALSE)

# guardamos los distintos archivos en un único data frame y cargamos los nombres
heartData <- rbind(cleveland,hungarian,switzer,longbeach)
names(heartData) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","goal")

# Exportamos las datos 
write.csv(heartData, "heartData_original.csv")

# analizamos los datos que tenemos

# sex: edad en años (continua)
# sex: 1 = hombre, 0 = mujer (categórica)
# cp: (chest pain) tipo de dolor de pecho: 1 = typical angina; 2 = atypical angina; 3 = non-anginal pain; 4 = asymptotic (categórica)
# trestbps: (Resting Blood Pressure) presión arterial en reposo (in mm Hg a la hora de ser ingresado en el hospital) (continua)
# chol: (Serum Cholestrol) colesterol en mg/dL (continua)
# fbs: (Fasting Blood Sugar) compara el azúcar en sangre estando en ayunas > 120 mg/dl (1 = true, 0 = false) (categórica)
# restecg: resultados del electrocardiograma en reposo: 0 = normal; 1 = havingST-T wave abnormality; 2 = left ventricular hyperthrophy (categórica)
# thalach: (Max heart rate achieved) ritmo cardiaco (continua)
# exang: (Exercise induced angina) angina (1 = yes; 0 = no) (categórica)
# oldpeak: depresión del segmento ST inducida por el ejercicio relativo al reposo (continua)
# slope: (Peak exercise ST segment) pendiente del máximo del ejercicio del segmento ST: 1 = upsloping; 2 = flat; 3 = downsloping (categórica)
# ca: número mayor de vasos (continua)
# thal: 3 = normal; 6 = fixed defect; 7 = reversable defect (categórica)
# goal: si el individuo sufre enfermedad cardiovasclar o no: 0 = no; 1,2,3,4 = sí (categórica)

str(heartData)
summary(heartData)
sapply(heartData, class)

```


***
# LIMPIEZA Y TRANSFORMACIÓN DE DATOS
***

Vamos a limpiar y transformar los datos. La verdad es que los datos que hemos descargado ya están bastante procesados: solo contamos con las 14 columnas que se consideran relevantes según los autores del data set en vez de las 75 originales, las variables que estaban como texto (sex, por ejmplo) ya cuentan con valores numéricos. Por lo que sobre todo trataremos los valores nulos del data set además de modificar los valores de una variable.

```{r}

# comprobamos si hay nulos
# hay que tener en cuenta que a la hora de cargar los datos, hemos indicado que lea como valores nulos allí donde aparezca "?"
colSums(is.na(heartData)) 

# comprobamos cuántos objetos no tienen ningún nulo
heartCompleto <- heartData[complete.cases(heartData), ]

# eliminamos la variable ca
heartData <- heartData[,-12]

# eliminamos las filas que tienen valores nulos en trestbps, thalach, exang y oldpeak
heartData <- heartData[!is.na(heartData$trestbps),]
heartData <- heartData[!is.na(heartData$thalach),]
heartData <- heartData[!is.na(heartData$exang),]
heartData <- heartData[!is.na(heartData$oldpeak),]

# eliminamos restecg, chol y fbs
heartData <- heartData[!is.na(heartData$restecg),]
heartData <- heartData[!is.na(heartData$chol),]
heartData <- heartData[!is.na(heartData$fbs),]

# damos un valor a los nulos
heartData$slope[is.na(heartData$slope)] <- 4 # 4 =  categoría para valores desconocidos
heartData$thal[is.na(heartData$thal)] <- 4

# reordenamos las filas
row.names(heartData) <- 1:nrow(heartData)

# los valores 2, 3 y 4 de la columa goal, pasarán a ser 1: enfermo
heartData$goal[heartData$goal == 2] <- 1
heartData$goal[heartData$goal == 3] <- 1
heartData$goal[heartData$goal == 4] <- 1



```

Lo primero que hemos hecho ha sido limpiar los valores nulos. Tenemos un total de 920 observaciones y 1.759 valores nulos de un total de 12.880 valores. Cuando observamos cuántos nulos hay por variable, vemos que hay tres variables que destacan bastante sobre el resto:
- slope: 309 nulos; esto supone un 33% de valores nulos.
- ca: 611 nulos; esto supone un 66% de valores nulos.
- thal: 486 nulos; esto supone un 52% de valores nulos.

Comprobamos cuántos objetos tienen todos los valores y vemos que solo hay 299, por lo que optar por quitar todos los nulos nos quedaría con pocos observaciones para realizar el estudios. Optamos entonces por otras alternativas.
Como la variable ca tiene un 66% de sus valores nulos, la eliminamos del data frame, puesto que no la consideramos determinante para los resultados del estudio.

Vemos que trestbps, thalach, exang y oldpeak tienengit sat valores aparecen como nulos práctimaente en las mismas observaciones. Así pues, también hemos optado por eliminar estos valores nulos. Estos datos representan el 8% de los datos. Después de investigar un poco, pudimos ver que estas variables hacen referencia a los parámetros que se analizan en una prueba de esfuerzo específica que se hace a los pacientes en estas situaciones (Exercise stress test, Harvard Health Publishing 2010) por lo que podemos deducir que estos pacientes no fueron sometidos a estos test.

La variable restcg solo cuanta con dos nulos, por lo que podemos eliminarlos también.

Puesto que fbs es una variable boleana, vamos a eliminar los nulos que contenga.

También eliminamos la vairable chol.

Para slope y thal, que suponían un 33% y un 52% respectivamente. Estos datos ya han sido procesados, incialmente, estos dos variables eran categóricas, teniendo los siguiente valores: 
- slope: downsloping, flat y upsloping
- thal: fixed effect, normal, reversible defect
Pues que estas variables ya están factorizadas en estos datos, vamos a tomar los nulos como si fueran datos desconocidos y les vamos a identificar con 4, como si fuera una categoría más.

Finalmente, en la variable goal, la que determina si el paciente va a tener o no enfermedad cardíaca, vemos que hay cuatro valores, que indican el grado de enfermedad. Nosotros, realmente, lo que queremos es un valor boleano que nos indique si lo está o no, ignorando el grado. Es por eso que los valores 2, 3 y 4 pasarán a ser 1. Se queda entonces que 1 determina aquellos pacientes que sufren una enfermedad cardíaca mientras que 0 determina aquellos que no.

Finalmente, nos quedamos con 740 obervaciones y 13 columnas. Los datos se han reducido en un 20%.

Ahora aplicaremos algunas trasnformaciones adicionales pero necesarias a los datos.

Reemplazamos las variables categoricas por su significado.


```{r}

# Preservamos el dataset sin transformar para anlisis posteriores que requieran variables numericas.

heartData_num <- heartData

# Realizamos la transformacion

heartData$cp <- replace(heartData$cp, heartData$cp =="1", "typical angina")
heartData$cp <- replace(heartData$cp, heartData$cp =="2", "atypical angina")
heartData$cp <- replace(heartData$cp, heartData$cp =="3", "non — anginal pain")
heartData$cp <- replace(heartData$cp, heartData$cp =="4", "asymptotic")
heartData$cp <- as.factor(heartData$cp)
unique(heartData$cp)

heartData$fbs <- replace(heartData$fbs, heartData$fbs =="1", "true")
heartData$fbs <- replace(heartData$fbs, heartData$fbs =="0", "false")
heartData$fbs <- as.factor(heartData$fbs)
unique(heartData$fbs)

heartData$restecg <- replace(heartData$restecg, heartData$restecg =="0", "normal")
heartData$restecg <- replace(heartData$restecg, heartData$restecg =="1", " having ST-T wave abnormality")
heartData$restecg <- replace(heartData$restecg, heartData$restecg =="2", "eft ventricular hyperthrophy")
heartData$restecg <- as.factor(heartData$restecg)
unique(heartData$restecg)

heartData$exang <- replace(heartData$exang, heartData$exang =="1", "Yes")
heartData$exang <- replace(heartData$exang, heartData$exang =="0", "No")
heartData$exang <- as.factor(heartData$exang)
unique(heartData$exang)

heartData$slope <- replace(heartData$slope, heartData$slope =="1", "upsloping")
heartData$slope <- replace(heartData$slope, heartData$slope =="2", "flat")
heartData$slope <- replace(heartData$slope, heartData$slope =="3", "downsloping")
heartData$slope <- as.factor(heartData$slope)
unique(heartData$slope)

heartData$thal <- replace(heartData$thal, heartData$thal =="3", "normal")
heartData$thal <- replace(heartData$thal, heartData$thal =="6", "fixed defect")
heartData$thal <- replace(heartData$thal, heartData$thal =="7", "reversible defect")
heartData$thal <- as.factor(heartData$thal)
unique(heartData$thal)

heartData$goal <- replace(heartData$goal, heartData$goal =="0", "Ausencia")
heartData$goal <- replace(heartData$goal, heartData$goal =="1", "Presencia")
heartData$goal <- as.factor(heartData$goal)
unique(heartData$goal)

heartData$sex <- replace(heartData$sex, heartData$sex =="0", "Male")
heartData$sex <- replace(heartData$sex, heartData$sex =="1", "Female")
heartData$sex <- as.factor(heartData$sex)
unique(heartData$sex)

# Fuente de las categorias https://towardsdatascience.com/heart-disease-prediction-73468d630cfc

```

Ahora nuetro dataset es mas comprensible y las columnas tienen el tipo correcto, factor o double, segun corresponde.

```{r}
library(tidyverse) 

glimpse(heartData)

```


***
# EXPLORACIÓN DE LOS DATOS
***

## Valores atípicos

Vamos a tratar ahora los valores atípicos del data set. Para ello, realizaremos diferentes diagramas de cajas y bigotes para localizar dichos valores en las variables numéricas.

```{r}

require(reshape2)
require(ggplot2)
# dejamos de lado el resto de variables, ya que se tratan de variables categóricas y nos centramos en las numéricas
meltedData <- melt(heartData[-c(2,3,6,7,9,11:13)])

# mostramos los distintos diagramas en un solo plot
p <- ggplot(data = meltedData, aes(x=variable, y=value, fill=variable)) + 
             geom_boxplot()+
             theme_minimal()
p + facet_grid( ~ variable, scales="free")

```

Al analizar el resultado, vemos que no nos encontramos con muchos valores atípicos en ninguna variable a excepción de chol y, en menos medida, trestbps. En este caso, no vamos a tratar estos valores ni imputándolos ni borrándolos de juego de datos, vamos a dejarlos tal cual. Esta decisión se debe a que los valores, aunque atípicos dentro de este juego de datos, no lo son a la hora de analizar las enfermedades cardiovasculares. Es decir, es posible encontrar un paciente que tenga > 400 de colesterol. Quitar estos valores, podría generar una pérdida importante de la información de cara al estudio de estas enfermedades.

Vamos a extraer este juego de datos a csv:
```{r}

write.csv(heartData, "heartData_clean.csv")

```


Ahora que dispoemos de datos limpios con los que trabajar nos disponemos a extraer información relevante sobre el comportamiento de las variables.

```{r}
require(dplyr)

summarise(heartData)

heartData %>%
    group_by(goal) %>%
    summarise(Count = n(),
            age_mean = round(x = mean(age), 2),
            trestbps_mean = round(x = mean(trestbps), 2),
            chol_mean = round(x = mean(chol), 2),
            thalach_mean = round(x = mean(thalach), 2),
            oldpeak_mean = round(x = mean(oldpeak), 2),
            ) %>% t()

```

En la tabla de medias tomamos en consideracion todas las variables continuas que disponemos para explorar hacia que grupo de goal se decantan.

En primer lugar, podemos notar que la muestra se encuentra dividida con bastante equidad entre las personas que presentan enfermedad y las que no.

La edad que a priori pensamos que seria una variable clave no muestra especial inclinacion hacia un grupo u otro.

Son oldpea, thalach y chol las vriables que sí parecían mostrar diferencias entre un grupo y otro.


```{r}

heartData %>%
  ggplot(aes(as.factor(goal), age ,fill = as.factor(goal))) +
    scale_x_discrete("Enfermedad")+
    scale_fill_manual(name = "Enfermedad", labels = c("Ausencia", "Presencia"),values = c("steelblue1", "darkorange1"))+
    geom_boxplot(alpha = 0.65)

```

Para comprobar las sospechas que tenemos sobre que no tenemos diferencias en la edad creamos un boxplot donde corroboramos que efectivamente esa diferencia no es asentuada, solo una pequeña tendencia a la alza en el grupo que presenta enfermedades cardiacas.


```{r}

heartData  %>%
  ggplot(aes(x = as.factor(goal), fill = sex, color = sex)) + geom_bar(show.legend = T) +
  facet_wrap(~sex) +
  theme_minimal() + 
  ylab("Frecuencia") +
  xlab("")

```

Otro insight interesante que podemos obtener a partir del analisis preliminar de los datos es que en la muestra las mujeres parecen tener una diferencia significativa en cuanto a la presencia de enfermedades cardiacas en comparacion a los hombres. Es importante mencionar tambien que para este estudio se tienen menos observaciones de mujeres que de hombres.

***
# ANÁLISIS DE LOS DATOS
***

# Análisi de la normalidad y homogeneidad de la varianza

```{r}

require(gridExtra)

p1 <- ggplot(data=heartData_num, aes(x=age)) +
    geom_density(color="grey33", fill="dodgerblue", alpha=0.4)

p2 <- ggplot(data=heartData_num, aes(x=trestbps)) +
    geom_density(color="grey33", fill="pink1", alpha=0.4)

p3 <- ggplot(data=heartData_num, aes(x=chol)) +
    geom_density(color="grey33", fill="seagreen", alpha=0.4)

p4 <- ggplot(data=heartData_num, aes(x=thalach)) +
    geom_density(color="grey33", fill="red", alpha=0.4)

p5 <- ggplot(data=heartData_num, aes(x=oldpeak)) +
    geom_density(color="grey33", fill="yellow", alpha=0.4)


grid.arrange(p1, p2, p3, p4, p5)

```

```{r}

require(ggpubr)
require(gridExtra)


p6 <-  ggqqplot(heartData_num$age)
p7 <-  ggqqplot(heartData_num$trestbps)
p8 <-  ggqqplot(heartData_num$chol)
p9 <-  ggqqplot(heartData_num$thalach)
p10 <- ggqqplot(heartData_num$oldpeak)

grid.arrange(p6, p7, p8, p9, p10)

```

Como podemos ver por el análisi visual que nos ofrecen los gráficos de densisdad y los gráficos Q-Q, todas las variables numéricas siguen una distribución normal. Si bien se aprecian valores que se desvían, esto no es suficiente para descartar la hipótesis de normalidad.

Para comprobar la homegenidad de las variables, vamos a realizar un análisis anova.
```{r}

modelAnova <- aov(goal ~ oldpeak * thalach , data = heartData_num)
summary(modelAnova)

```

En este caso se comprueba la homegeniedad de las vrianzas de las varibales del modelo.

# Aplicación de pruebas estadísticas y métodos de análisis estadísticos

Vamos a realizar tres métodos de análisis estadísticos. 
En primer lugar, vamos a realizar una matriz de correlación que nos permitirá ver la relación que guardan las variables entre sí.
Luego, realizaremos un modelo de regresión lineal que nos permitirá poder predecir si los pacientes desarrollarán o no una enfermedad cardiovascular. y comprobaremos su eficiencia.
En tercer lugar, pondremos en práctica un árbol de decisión. Este nos permitirá ver qué variables son las más representatativas a la hora de determinar la presencia de una enfermedad en el paciente.
Finalmente, haremos un K-means.

## Correlación

Vamos a usar tres funciones diferentes: - rcorr: nos devolverá una lista con los siguentes elementos: - r: la matriz de correlación - n: una matriz que nos muestra el número de observaciones usadas para analizar cada par de variables - p: los p-valores correspondientes al los niveles de significación de correlación

corrplot: nos va a permitir visualizar gráficamente la matriz de correlaciones:
las correlaciones positivas aparecen representadas en azul y las correlaciones negativas en rojo; las insignificantes aparecen en blanco. La intensidad del color y el tamaño del círculo son proporcionales a los coeficientes de correlación.

```{r}

require(corrplot)

# usamos la función corrplot para la visualización de la matriz de relaciones
# antes necesitamos tener nuestro data ser como una matriz de correlaciones, usamos la función cor
heartData_cor = cor(heartData_num,method = c("spearman"))
corrplot(heartData_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 60)

```

Podemos observar que no hay mucha correlación entre las variables. Thalach tiende a mostrar una correlación negativa con el resto de variables. La mayor correlación la encontramos entre goal y cp, goal y exang y goal y oldpeak.


## Regresión lineal

Vamos a realizar un modelo de regresión lineal que nos permitirá predecir si un paciente tendrá o no una enfermedad cardiovascular. Para realizar dicho modelo, usaremos las variables que más correlación guardan entre ellas.
```{r}

attach(heartData)

modelLineal <- lm(goal~age+sex+cp+oldpeak+exang+thalach,data = heartData_num,family = binomial)
summary(modelLineal)

```

Comprobamos la eficiencia del modelo.
```{r}

predicciones <- ifelse(test = modelLineal$fitted.values > 0.5, yes = "Yes", no = "No")
matriz_confusion <- table(modelLineal$model$goal , predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

```
En el 0: el modelo acierta 290 de 357
En el 1: el modelo acierta 306 de 383

Un acierto del 80 %.

Ahora, podríamos usar este modelo para predecir si el paciente tendrá o no una enfermedad cardiovascular:
```{r}

#▲ creamos un nuevo data frame con las caracterísitcas de un paciente imaginario
newdata <- data.frame(
    age = 61,
    sex = 1,
    cp = 2,
    oldpeak = 3.5,
    exang = 1,
    thalach = 147)

# Predecir si tendrá una enfermedad o no
predict(modelLineal, newdata)

```

Según nuestro modelo, la probabilidad de que el paciente caiga enfermo es del 80,50 %.


# Árbol de decisión

Para crear el árbol de decisión, primero tenemos que separar lo datos en train y test, con una proporción 70:30.
```{r}

require(caTools)

# vamos a poner la seed
set.seed(120)
# usamos la función sample.split para separar los valores. El SplitRatio nos permite separar los valores en 70-30.
sampleValue = sample.split(heartData_num,SplitRatio=0.7) 
heartTrain = subset(heartData_num, sampleValue == TRUE)
heartTest = subset(heartData_num, sampleValue == FALSE)

as.data.frame(table(heartTrain$goal))
as.data.frame(table(heartTest$goal))

```

Comprobamos que los datos están bien distribuídos: 
Hay un total de 740 observaciones: 
    0 → 357 observaciones (48,25 %)
    1 → 383 observaciones (51,75 %)

En la variable heartTrain tenemos 512 observaciones, lo que suponene 69,18 % de los datos:
    0 → 251 observaciones (49,02 %)
    1 → 261 observaciones (50,98 %)

En la variable heartTest tenemos 228 observaciones, lo que suponene 30,82 % de los datos:
    0 → 106 observaciones (46,50 %)
    1 → 122 observaciones (53,50 %)

Los datos están, por lo tanto, bien divididos. Podemos pasar a crear el modelo:
```{r}

require(rpart)
require(rpart.plot)

modelR <- rpart(formula = goal ~ ., data = heartTrain)
rpart.rules(modelR, style = "tallw", cover = TRUE)

```

```{r}

modelR

```

Lo visualizamos gráficamente;
```{r}

require(rattle)

fancyRpartPlot(modelR, main = "Árbol de decición", caption = " ", palettes = "Oranges", type = 3)

```

Podemos sacar algunas conclusiones: la variable exang es la más significativa a la hora de decidir en qué grupo estará cada observación. Por lo tnato, la primera partición se hará en función de exang, dependiendo de si es 0 o 1. La regla que cubre el 40 % de los casos, siendo así la más relevante, determina si un paciente tendrá o no una enfermedad cardíaca basándose en esta primera partición de exang, que para este caso, será igual a 0 y en los valores de thal, chol y oldpeak. Esta regla nos dice que la probabilidad de que las muestras que cumplen esas condiciones comentandas tenga una enfermedad cardiaca es del 15 %. Por lo tanto, el 85 % no sufrirá una enfermedad cardíaca. Como segunda más relevante, tenemos la contraparte del valor de exang. Cuando este es 1, y además el cp está por encima de 0.83, entonces el paciente tendrá una enfermedad cardíaca. Esta regla cubre el 33 % de los casos. La probablidad de que, cumpliendo con estos parámetros, se sufra una enfermedad cardíaca es del 88 %. Con estas dos reglas, tendríamos identificadas casi tres cuartas partes de todas las observaciones. También contamos con otras reglas para casos más excepcionales. Por ejemplo, en el caso de la primera regla, en las situaciones en las que thal está por encima de 0.5, tenemos otra regla que recoge el 10 % de los casos y se fija en el valor de cp. Contamos con otras reglas que cubrirían los casos restantes, solo en torno a un 4 % de las muestras. La regla que determina con más seguridad, 92 %, que el paciente va a sufrir una enfermedad cardíaca, es aquella en la que exang tiene valor de 1, cp está por debajo de 0.83 y el thalach por debajo de 0.42. Sin emabargo, esta regla solo cubre el 3 % de los casos.

# K-means

Para poder generar un modelo que sea capaz de predecir la presencia o no de enfermedad debemos en primer lugar disponer del dataset sin la variable objetivo.

```{r}
library(cluster)
library(dplyr)

# Creamos un dataset unicamente con las variables numericas
df_kmeans <- select_if(heartData, is.numeric)

# Exportamos el dataset
write.csv(df_kmeans, "heartData_num_kmenas.csv")

```

Como inicialmente no conocemos el número óptimo de clústers, probamos con varios valores, aunque esperamos que el valor óptimo sea 5 ya que es las clases en las que originalmente viene dividido el dataset.

```{r}
d <- daisy(df_kmeans) 
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9,10))
{
  fit           <- kmeans(df_kmeans, i)
  y_cluster     <- fit$cluster
  sk            <- silhouette(y_cluster, d)
  resultados[i] <- mean(sk[,3])
}
```


Mostramos en un gráfica los valores de las siluetas media de cada prueba para comprobar el mejor numero de cluster.

```{r message= FALSE, warning=FALSE}
plot(2:10,resultados[2:10],type="o",col="blue",pch=0,xlab="Número de clusters",ylab="Silueta")
```
Contrario a nuestras espectativas el mejor valor obtenido es k=2 y no k=5 como nos planteamos incialmente.

Aplicamos analisis adicionales para determinar el k a usar.

En primer lugar el método elbow:
```{r message= FALSE, warning=FALSE}
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9,10))
{
  fit           <- kmeans(df_kmeans, i)
  resultados[i] <- fit$tot.withinss
}
plot(2:10,resultados[2:10],type="o",col="blue",pch=0,xlab="Número de clusters",ylab="tot.tot.withinss")
```
En este caso la curva comienza a estabilizarse en en 4 con los que k=4 seria el numero optimo segun este metodo.

Ahora aplicamos la función kmeansruns del paquete fpc que ejecuta el algoritmo kmeans con un conjunto de valores, para después seleccionar el valor del número de clústers que mejor funcione de acuerdo a dos criterios: la silueta media ("asw") y Calinski-Harabasz ("ch").

```{r message= FALSE, warning=FALSE}
library(fpc)
fit_ch  <- kmeansruns(df_kmeans, krange = 1:10, criterion = "ch") 
fit_asw <- kmeansruns(df_kmeans, krange = 1:10, criterion = "asw") 
```

Podemos comprobar el valor con el que se ha obtenido el mejor resultado y también mostrar el resultado obtenido para todos los valores de k usando ambos criterios


```{r message= FALSE, warning=FALSE}
fit_ch$bestk
fit_asw$bestk

plot(1:10,fit_ch$crit,type="o",col="blue",pch=0,xlab="Número de clústers",ylab="Criterio Calinski-Harabasz")
plot(1:10,fit_asw$crit,type="o",col="blue",pch=0,xlab="Número de clústers",ylab="Criterio silueta media")

```

Los resultados apuntan que en este caso el numero de clusters adecuado es k=2 que concuerda con nuestras categorias de presencia ausencia, lo que nos confirma que la decision de agrupar la variable goal en estas dos categorias fue acertada inicalmente.

```{r message= FALSE, warning=FALSE}
k2 <- kmeans(df_kmeans, centers = 2)
k2
str(k2)

```

El análisis de kmeans nos permite observar que exister dos clusters claramente diferenciados que logran explicar el 59.9% de la varianza total del modelo. 

Procedemos a representarlo gráficamente para tener una mejor comprensión de la clsificación. 


```{r message= FALSE, warning=FALSE}

library(factoextra)

fviz_cluster(k2, data = df_kmeans, ellipse.type = "euclid", repel = FALSE, star.plot = TRUE )

```

En el grafico podemos observar dos clusters diferenciados, esto nos da pistas sobre las variables que nos permita predecir adecuadamente la presencia o ausencia de enfermedad. 


## Conclusiones

Como se ha visto se ha visto inicialmente hemos procesado y limpiado un dataset que contiene data real y extensa sobre enfermdades cardiacas, hemos realizado un proceso de elmincación de datos ausentes y consideración de valores extremos. 

Posteriormente condujimos un análisis exploratorio de los datos que nos permitió tener una primera impresión de cómo se comportarían las variables y lo que podríamos esperar al conducir pruebas estadísticas. 

En cuanto a las pruebas aplicadas, partimos de un regresión lineal que resulta útil para conocer la relación que existe entre las variables del modelo, luego elaboramos un árbol de decisión que nos permitió generar reglas de comportamiento de las variables y así poder conocer las condiciones que suceden en los distintos casos de la variables obetivo. 

Finalmente, aplicamos un análisis de k-means donde obtuvimos una agrupación de las variables del modelos en función del efecto que tiene sobre la ausencia o presencia de enfermedad.

Todos los analisis, desde el exploratorio al estadistico, permitieron tener una caracterización y comprensión profunda de las variables que conforman el dataset de estudio. Podemos afirmar, entonces, que sí hemos sido capaces de responder al problema con el que partíamos en un principio que consistía en saber si podríamos determinar las enfermedades cardiovasculares en los pacientes.





