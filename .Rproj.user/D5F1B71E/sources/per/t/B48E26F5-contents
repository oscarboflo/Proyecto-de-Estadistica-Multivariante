# Leyendo los Datos -------------------------------------------------------

library(readxl)
library(tidyverse)
library(knitr)
library("fdth")
library(plotly)
library(corrr)
library(ggplot2)
library(grid)
library(gridExtra)
library(GPArotation)
library(psych)
library(e1071)
library(mfv)
library(highcharter)
library(ca)

datos = read_excel("Grupo 1.xlsx", col_names = T)
#View(datos)

# Estadistica Descriptiva -------------------------------------------------

# Variables Cualitativas -----------

# Tablas de Frecuencia

#Tabla de frecuencia del Operador
tfrecuencia_operador <- table(datos$Operador)
#tfrecuencia_operador

kable(tfrecuencia_operador, caption = "Tabla de Frecuencia del Operador",col.names = c("Operador", "Frecuencia"),
      digits = 1, format.args=list(decimal.mark=","))


# Digrama de Barras para el Operador

ggplot(datos, aes(x=Operador, fill=as.factor(Operador))) +  geom_bar( ) +  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")

#Tabla de frecuencia del Tipo
tfrecuencia_tipo <- table(datos$TIPO)

kable(tfrecuencia_tipo, caption = "Tabla de Frecuencia del Tipo",col.names = c("Tipo", "Frecuencia"),
      digits = 1, format.args=list(decimal.mark=","))

# Diagrama de Barras para el Tipo

ggplot(datos, aes(x=TIPO, fill=as.factor(TIPO))) +  geom_bar( ) +  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")

#Tabla de frecuencia de la Especie
tfrecuencia_especie <- table(datos$ESPECIE)

kable(tfrecuencia_especie, caption = "Tabla de Frecuencia de la Especie",col.names = c("Especie", "Frecuencia"),
      digits = 1, format.args=list(decimal.mark=","))

# Diagrama de Barras para la Especie

ggplot(datos, aes(x=ESPECIE, fill=as.factor(ESPECIE))) +  geom_bar( ) +  scale_fill_brewer(palette = "Set3") +
  theme(legend.position="none")

#Tabla de frecuencia de los Meses
datos <- datos %>%
  mutate(mes = case_when(mes == 1 ~ 'Enero',
                         mes == 2 ~ 'Febrero',
                         mes == 3 ~ 'Marzo',
                         mes == 4 ~ 'Abril',
                         mes == 5 ~ 'Mayo',
                         mes == 6 ~ 'Junio',
                         mes == 7 ~ 'Julio',
                         mes == 8 ~ 'Agosto',
                         mes == 9 ~ 'Septiembre',
                         mes == 10 ~ 'Octubre',
                         mes == 11 ~ 'Noviembre',
                         mes == 12 ~ 'Diciembre'
  )) %>%
  mutate(mes = as_factor(mes))


tfrecuencia_mes <- table(datos$mes)

kable(tfrecuencia_mes, caption = "Tabla de Frecuencia del Mes",col.names = c("Mes", "Frecuencia"),
      digits = 1, format.args=list(decimal.mark=","))


# Diagrama de barras para el mes
ggplot(datos, aes(x=mes, fill=as.factor(mes))) +  geom_bar( ) +  scale_fill_brewer(palette = "Set3") +
  theme(legend.position="none")

# Variables cuantitivas ----------------

# Tablas de frecuencia para Nbatch
tfrecuencia_nbatch <- fdt(datos$NBatch)

kable(tfrecuencia_nbatch, caption = "Tabla de Frecuencia del NBatch",
      digits = 2, format.args=list(decimal.mark=","))

# Histograma del Nbatch
plot(tfrecuencia_nbatch, type = "fh", col="lightyellow", xlab = "Numero de proceso por lote", ylab = "Frecuencias", main = "Numero de procesos por Lote")

#Medias de tendencia central y Dispersion
summary(datos$NBatch)


# Tablas de frecuencia para CapLinea
tfrecuencia_CapLinea <- fdt(datos$CapLinea)

kable(tfrecuencia_CapLinea, caption = "Tabla de Frecuencia del CapLinea",
      digits = 2, format.args=list(decimal.mark=","))

# Histograma del CapLinea
plot(tfrecuencia_CapLinea, type = "fh", col="green", xlab = "Capacidad de la maquina", ylab = "Frecuencias", main = "Capacidad de la maquina")

#Medias de tendencia central y Dispersion
summary(datos$CapLinea)



# Tablas de frecuencia para Hmezcla
tfrecuencia_Hmezcla <- fdt(datos$HdMezcladora)
kable(tfrecuencia_Hmezcla, caption = "Tabla de Frecuencia del Hmezcla",
      digits = 2, format.args=list(decimal.mark=","))

# Histograma del Hmezcla
plot(tfrecuencia_Hmezcla, type = "fh", col="lightblue", xlab = "Horas de mezcla por producto", ylab = "Frecuencias", main = "Horas de mezcla por producto")

#Medias de tendencia central y Dispersion
summary(datos$HdMezcladora)


# Tablas de frecuencia para minOrd
tfrecuencia_minOrd <- fdt(datos$minOrd)
kable(tfrecuencia_minOrd, caption = "Tabla de Frecuencia del minOrd",
      digits = 2, format.args=list(decimal.mark=","))

# Histograma del minOrd
plot(tfrecuencia_minOrd, type = "fh", col="pink", xlab = "Numero de proceso por lote", ylab = "Frecuencias", main = "Numero de procesos por Lote")

#Medias de tendencia central y Dispersion
summary(datos$minOrd)


#Diagrama de Cajas de TempAc1, TempAc2, TemExp

fig <- plot_ly(type = 'box')

fig <- fig %>% add_boxplot(y = datos$TempAc1, name = "Tempartura Aire Acondicionado 1", boxpoints = FALSE,
                           marker = list(color = 'rgb(9,56,125)'),
                           line = list(color = 'rgb(9,56,125)'))
fig <- fig %>% add_boxplot(y = datos$TempAc2, name = "Tempartura Aire Acondicionado 2", boxpoints = 'suspectedoutliers',
                           marker = list(color = 'rgb(8,81,156)',
                                         outliercolor = 'rgba(219, 64, 82, 0.6)',
                                         line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                                                     outlierwidth = 2)),
                           line = list(color = 'rgb(8,81,156)'))
fig <- fig %>% add_boxplot(y = datos$TemExp, name = "Temperatura Expandido", boxpoints = 'outliers',
                           marker = list(color = 'rgb(107,174,214)'),
                           line = list(color = 'rgb(107,174,214)'))
fig <- fig %>% layout(title = "Diagrama de cajas")

fig

# Estadistica Bivariante --------------------------------------------------

# Matriz de correlacion de forma grafica
r.datos <- cor(datos[,13:16])
cor.plot(r.datos, numbers = TRUE)

#Matriz de varianza y covarianza
matriz_va_cova <- cov(datos[,c('TempAc1','TempAc2','TemExp')])

kable(matriz_va_cova, caption = "Matriz de Varianza y Covarianza",col.names = c("Temp. Ac1", "Temp. Ac2","Temp Exp"),
      digits = 1, format.args=list(decimal.mark=","))

# Grafico de correlacion con un modelo de regresion lineal

# Verifico que variables puedo tomar para hacer mi modelo de regresion lineal a traves de esta grafico
pairs(datos[,11:16], panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")})

fit1 <- lm(data = datos, TempAc1 ~ TempAc2)
summary(fit1)


#Grafico de la regresion lineal
library(dplyr)
library(broom)
fit1 <- lm(data = datos, TempAc1 ~ TempAc2)
modelo1 <- augment(fit1) %>% arrange(TempAc2)

hc <- datos %>% hchart('scatter', hcaes(x = TempAc2, y = TempAc1)) %>%
  hc_add_series(modelo1, type = "line", hcaes(x = TempAc2, y = .fitted), name = "Fit", id = "fit")
  
hc

#fit2 <- lm(data = datos, TempAc2 ~ TempAc1)
#summary(fit2)


#ggplot(datos, aes(x=TempAc1, y =TempAc2 ))+geom_point() + geom_smooth(method = "lm", colour = "Red")





# Estadistica Inferencial -------------------------------------------------

# Bondad de ajuste -------------------------------

# Supuestos: Muestras grandes, el tamaño de la muestra n es mayor o igual que 30

#Pruebas de Hipotesis
# H0: u < 50
# H1: u >= 50

media_tempac1 = mean(datos$TempAc1)
mediana_tempac1 = median(datos$TempAc1)
varianza_tempac1 = var(datos$TempAc1)
sd_tempac1 = sd(datos$TempAc1)
n_tempac1 = length(datos$TempAc1)
err_estandarmedia_tempac1 = sd_tempac1/sqrt(n_tempac1)
sesgo_tempac1 = skewness(datos$TempAc1)
moda_tempac1 = mfv(datos$TempAc1)

test_chisquare3 <- chisq.test(datos$TempAc1)
test_chisquare3

# Conclusion: Ya que el valor de p = 0.001948 , rechazamos la hipotesis nula que nos dice que la media de la Temparatura
# del aire acondicionado 1 es menor a 50



# Pruebas de Hipótesis e Intervalos de Confianza (media,proporción, varianza) -----------------------------------


#Prueba de Hipotesis 1:
# Supuestos: Muestras grandes, el tamaño de la muestra n es mayor o igual que 30
# H0: u < 50
# H1: u >= 50

media_tempac1 = mean(datos$TempAc1)
mediana_tempac1 = median(datos$TempAc1)
varianza_tempac1 = var(datos$TempAc1)
sd_tempac1 = sd(datos$TempAc1)
n_tempac1 = length(datos$TempAc1)
err_estandarmedia_tempac1 = sd_tempac1/sqrt(n_tempac1)
sesgo_tempac1 = skewness(datos$TempAc1)
moda_tempac1 = mfv(datos$TempAc1)

valorp_tempac1 <- chisq.test(datos$TempAc1)
valorp_tempac1
# Conclusion: Ya que el valor de p = 0.001948 , rechazamos la hipotesis nula que nos dice que la media es menor a 50

(Z_tempac1 = (media_tempac1 - 50)/err_estandarmedia_tempac1)

(ic_tempac1 = media_tempac1 + c(-1,1) * Z_tempac1 * err_estandarmedia_tempac1)  

#Conclusion: Con un intervalo de confianza del 95% podemos asegurar que la media de la Temperatura del Aire
# acondicionado 1 se va a encontrar entre [50.00000, 77.20841]


#Prueba de Hipotesis 2:
# Supuestos: Muestras grandes, el tamaño de la muestra n es mayor o igual que 30
# H0: u < 50
# H1: u >= 50

media_tempac2 = mean(datos$TempAc2)
mediana_tempac2 = median(datos$TempAc2)
varianza_tempac2 = var(datos$TempAc2)
sd_tempac2 = sd(datos$TempAc2)
n_tempac2 = length(datos$TempAc2)
err_estandarmedia_tempac2 = sd_tempac2/sqrt(n_tempac2)
sesgo_tempac2 = skewness(datos$TempAc2)
moda_tempac2 = mfv(datos$TempAc2)

valorp_tempac2 <- chisq.test(datos$TempAc2)
valorp_tempac2
# Conclusion: Ya que el valor de p = 2.2e-16 , rechazamos la hipotesis nula que nos dice que la media es menor a 50

Z_tempac2 = (media_tempac2 - 50)/err_estandarmedia_tempac2

(ic_tempac2 = media_tempac2 + c(-1,1) * Z_tempac2 * err_estandarmedia_tempac2)  

#Conclusion: Con un intervalo de confianza del 95% podemos asegurar que la media de la Temperatura del Aire
# acondicionado 2 se va a encontrar entre [50.00000, 68.79845]


# Analis de contingencia ------------------

#Analisis de contingencia 1
(tabl1 =table(datos$TIPO,datos$ESPECIE))#Tabla de contingencia

#Prueba de hipotesis
#H0: La cantidad de especies de tipo relacionado es mayor al de tipo comercial
#H1: La cantidad de especies de tipo relacionado NO mayor al de tipo comercial

test_chisquaretabl1 <- chisq.test(datos$TIPO,datos$ESPECIE)
test_chisquaretabl1

#Conclusion: Ya que el valor de p = 2.2e-16, rechazamos la H0 que nos dice que La cantidad de especies de tipo 
# relacional es mayor al de tipo comercial.

# Analisis de contingencia 2 

(tabl2 =table(datos$TIPO,datos$Operador))#Tabla de contingencia

#Prueba de hipotesis
#H0: La cantidad de ventas de tipo relacional con respecto a los operadores  es mayor al de tipo comercial
#H1: La cantidad de ventas de tipo relacional con respecto a los operadores NO es mayor al de tipo comercial

test_chisquaretabl2 <- chisq.test(datos$TIPO,datos$Operador)
test_chisquaretabl2

#Conclusion: Ya que el valor de p = 0.0417,  rechazamos la H0 que nos dice que La cantidad de ventas de tipo 
# comercial con respecto a los operadores NO es mayor al de tipo relacionado


# Analisis Multivariante --------------------------------------------------

# ANALISIS DE COMPONENTES PRINCIPALES --------------

# BIPLOT SIN ROTACION
library(factoextra)

datos2 = datos[,c(14,15,16,18)]

acp <- prcomp(datos2, scale. = TRUE)

# BIPLOT SIN ROTACION

fviz_pca_biplot(acp, repel = TRUE)

# BIPLOT CON ROTACION VARIMAX

biplot.psych(fa(datos2, nfactors=2, fm="pa", rotate="varimax"), main=paste("Biplot con rotación","varimax"), col=c(2,3,4),pch=c(21,18))

# Porcentaje de varianza explicada

fviz_screeplot(acp,addlabels=TRUE, ylim=c(0,70))

# Grafico de correlacion

library(corrplot)

corrplot(cor(datos2),        
         method = "shade", 
         type = "full",    
         diag = TRUE,      
         tl.col = "black", 
         bg = "white",     
         title = "",       
         col = NULL)       

# ANALISIS FACTORIAL --------------

# BIPLOT SIN ROTACION

biplot.psych(fa(datos2, nfactors=2, fm="pa", rotate="none"), main=paste("Biplot sin rotación"), col=c(2,3,4),pch=c(21,18))

# BIPLOT CON ROTACION QUARTIMAX

biplot.psych(fa(datos2, nfactors=2, fm="pa", rotate="Promax"), main=paste("Biplot con rotación","quartimax"), col=c(2,3,4),pch=c(21,18))

# CIRCULO UNITARIO

R=cor(datos2)

fit.pa.none=fa(R,nfactors=2,fm="pa", rotate="none",n.obs=220)

fit.ml.none=fa(R,nfactors=2, fm="ml",rotate="none",n.obs = 220)

corvar.pa.none<-fit.pa.none$loadings[,1:2]

p1<-plot(-1:1,-1:1,type="n",asp=1,xlab="F1", ylab="F2")+
  abline(h=0, v=0, lty=2, col=8)+
  symbols(0,0,1,inches=F,add=T)+
  symbols(0,0,sqrt(.5),inches=F,add=T)+
  arrows(0,0,corvar.pa.none[,1], corvar.pa.none[,2],length=.1)+
  text(corvar.pa.none[,1],corvar.pa.none[,2],colnames(datos2),pos=4, offset=.6, col=2, font=2)

# TEST DE BARLETT

bartlett.test(datos2)

# Con un nivel de significancia del 5%, ya que nuestro p-value > 0.05, decimos que no existe homogeneidad entre
# los datos

# TEST KMO

KMO(datos2)

# Ya que los valores de mis variables son mayores a 0.5, podemos decir que si se puede utilizar el Analisis Factorial

# DETERMINANTE DE LA MATRIZ DE CORRELACION
R=cor(datos2)
det(R)

# Ya que el valor de la matriz no es cercano a 0, decimos que no hay multicolinealidad entre las variables

# ESCALADO MULTIDIMENSIONAL --------------------------------
library(ape)
library(ade4)
library(vegan)
library(geometry)
library(permute)
library(lattice)
library(FD)
library(MASS)

# DIAGRAMA DE SHEPPARD

datos3 = datos[1:6,c(11,13,14,15,16,18)]
em<-isoMDS(as.dist(datos3))

shep<-Shepard(as.dist(datos3),em$points)
plot(shep,pch=20,cex=0.4)
lines(shep$x,shep$y,type = "S")

# VALOR DEL STRESS

(em$stress)

# PORCENTAJE DE VARIANZA EXPLICADA

fviz_screeplot(prcomp(datos3, scale. = TRUE),addlabels=TRUE, ylim=c(0,70))

# ANALISIS DE CORRESPONDENCIA --------------------------------

datos4 = datos[,c(36,38,39,40,41)]
acm <- mjca(datos4)

acm_s_col <- summary(acm)$columns

coord <- data.frame(acm_s_col[,5], acm_s_col[,8])


variables <- data.frame(acm_s_col$name)

variables$GRUPO[str_sub(variables$acm_s_col.name,1,9) == 'Operador:'] <- 'Operador'
variables$GRUPO[str_sub(variables$acm_s_col.name,1,8) == 'ESPECIE:'] <- 'Especie'
variables$GRUPO[str_sub(variables$acm_s_col.name,1,5) == 'TIPO:'] <- 'Tipo'
variables$GRUPO[str_sub(variables$acm_s_col.name,1,13) == 'PRESENTACION:'] <- 'Presentación'
variables$GRUPO[str_sub(variables$acm_s_col.name,1,8) == 'PROCESO:'] <- 'Proceso'


unlist_categorias <- unlist(str_split(variables$acm_s_col.name,":"))
n <- 1:(length(unlist_categorias)/2)
categorias <- unlist_categorias[2*n]

df_coordenadas <- data.frame(coord, categorias, variables$GRUPO)

highchart_plot <- highchart() %>%
  hc_title(text = 'Análisis de Correspondencia Múltiple') %>%
  hc_add_series(df_coordenadas, type = 'scatter', hcaes(x = acm_s_col...5., y = acm_s_col...8., name = categorias, group = variables$GRUPO),
                dataLabels = list(format = "{point.name}", enabled = TRUE),
                tooltip = list(pointFormat = "{point.name}")) %>%
  hc_xAxis(title = list(text = 'd1'), plotLines = list(list(value=0,color='lightblue',width=2,zIndex=4))) %>%
  hc_yAxis(title = list(text = 'd2'), plotLines = list(list(value=0,color='lightblue',width=2,zIndex=4)))

highchart_plot

# ANALISIS DISCRIMINANTE ----------

library(tidyverse)
library(MASS)
datos5 = datos[,c(28,29,31,38)]
modelo = lda(ESPECIE~CantKgProd+Granulometria+PorcAgua, data = datos5)
#modelo

lda.data <- cbind(datos5, predict(modelo)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = ESPECIE))
