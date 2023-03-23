library(shiny)
library(highcharter)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(readxl)
library(data.table)
#install.packages("shinyWidgets")
library(shinyWidgets)
library(shinythemes)
library(ca)
library(factoextra)
library(hpackedbubble)
library(FactoMineR)
library(factoextra)
library(ggplot2)
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
library(dplyr)
library(broom)
library(ape)
library(ade4)
library(vegan)
library(geometry)
library(permute)
library(lattice)
library(FD)
library(MASS)
library(ca)
library(highcharter)
library(philentropy)
library(philentropy)
library(tidyverse)
library(MASS)

#install.packages("hpackedbubble")
#install.packages("shinyWidgets")
datos = read_excel("Grupo 1.xlsx", col_names = T)
datos2 = datos[,c(14,15,16,18)]
datos3 = datos[1:6,c(11,13,14,15,16,18)]
datos4 = datos[,c(36,38,39,40,41)]
datos5 = datos[,c(28,29,31,38)]
datos6 <- readxl::read_excel("matrix.xlsx")
df <- data.frame(datos6)

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


#img = tags$a(href="https://www.espol.edu.ec/",tags$img(src="https://i.ibb.co/GcCXysm/FCNM.png",height="45", width="200"))


ui <- dashboardPage(skin="green",
                    #NOMBRE DEL PANEL
                    dashboardHeader(title = "Proyecto"),
                    ## Contenido Sidebar, PANEL IZQUIERDO 
                    dashboardSidebar(
                      sidebarMenu(
                        
                        menuItem("PRESENTACION DE LA BASE",tabName ="prese-base",icon =icon("house")),
                        menuItem("ANÁLISIS DESCRIPTIVO", tabName = "analis-descrip",icon = icon("bar-chart"),
                                 menuSubItem("VARIABLES CUALITATIVAS", tabName = "var-cual"),
                                 menuSubItem("VARIABLES CUANTITATIVAS", tabName = "var-cuanti")),
                        menuItem("ANALISIS BIVARIANTE",tabName = "analis-biva",icon=icon("chart-line")),
                        menuItem("ESTADISTICA INFERENCIAL", tabName = "est-infe",icon = icon("table"),
                                 menuSubItem("BONDAD DE AJUSTE", tabName = "bond-ajust"),
                                 menuSubItem("P. HIPOTESIS E I. CONFIANZA", tabName = "hi-ic"),
                                 menuSubItem("ANALISIS DE CONTINGENCIA", tabName = "analis-contin")), 
                        menuItem("ESTADISTICA MULTIVARIANTE", tabName = "est-multi",icon = icon("diagram-project"),
                             menuSubItem("ACC", tabName = "acc"),
                             menuSubItem("ACP", tabName = "acp"),
                             menuSubItem("AF", tabName = "af"),
                             menuSubItem("MDS", tabName = "mds"),
                             menuSubItem("AC", tabName = "ac"),
                             menuSubItem("ACG", tabName = "acg"),
                             menuSubItem("AD", tabName = "ad")))),
                    #CUERPO
                    #CONTENIDO DE LA PRESENTACION DE LA BASE -----------------
                    dashboardBody(tabItems( 
                      tabItem(tabName = "prese-base",h2("PRESENTACION DE LA BASE",align="center"),
                                                  fluidPage(
                                                    navbarPage(tabPanel("ENBLANCO"),tabPanel("Visualizacion de los datos",
                                                                                             dataTableOutput("tabla1"))))),
                      # CONTENIDO DE LAS VARIABLES CUALITATIVAS --------------
                      tabItem(tabName = "var-cual",
                              h2("ANALISIS DESCRIPTIVOS VARIABLES DE LAS VARIABLES CUALITATIVAS",align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("Operador",
                                           fluidRow(box(div(tags$img(src = "https://i.postimg.cc/Dzk7JnCG/Tabla-de-Frecuencia-del-Operador.png", height = "400px", width="400"), style="text-align: center;")),
                                                    box(plotOutput("barchart1"),width = 6))),
                                           tabPanel("TIPO",
                                                    fluidRow(box(div(tags$img(src = "https://i.postimg.cc/8PR2KMw7/image-2022-11-06-222939866.png", height = "400px", width="400"), style="text-align: center;")),
                                                             box(plotOutput("barchart2"),width = 6))),
                                           tabPanel("ESPECIE",
                                                    fluidRow(box(div(tags$img(src = "https://i.postimg.cc/FsrpvSNd/image-2022-11-06-223428549.png", height = "400px", width="400"), style="text-align: center;")),
                                                             box(plotOutput("barchart3"),width = 6))),
                                           tabPanel("MES",
                                                    fluidRow(box(div(tags$img(src = "https://i.postimg.cc/3rcNr5HX/image-2022-11-06-223856161.png", height = "400px", width="400"), style="text-align: center;")),
                                                             box(plotOutput("barchart4"),width = 6)))))),
                      # CONTENIDO DE LAS VARIABLES CUANTITATIVAS---------
                      tabItem(tabName = "var-cuanti",
                              h2("ANALISIS DESCRIPTIVOS VARIABLES DE LAS VARIABLES CUANTITATIVAS",align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("NBATCH",
                                                    fluidRow(box(div(tags$img(src = "https://i.postimg.cc/WbnkNkzy/Tabla-de-Frecuencia-del-NBatch.png", height = "400px", width="400"), style="text-align: center;")),
                                                             box(plotOutput("hist1"),width = 6),
                                                             box(verbatimTextOutput("summary1"),width=6))),
                                           tabPanel("CAPLINEA",
                                                    fluidRow(box(div(tags$img(src = "https://i.postimg.cc/6pCjMzpP/Tabla-de-Frecuencia-del-Caplina.png", height = "400px", width="400"), style="text-align: center;")),
                                                             box(plotOutput("hist2"),width = 6),
                                                             box(verbatimTextOutput("summary2"),width=6))),
                                           tabPanel("HMEZCLA",
                                                    fluidRow(box(div(tags$img(src = "https://i.postimg.cc/gJ2gkWH9/Tabla-de-Frecuencia-de-Hmezcla.png", height = "400px", width="400"), style="text-align: center;")),
                                                             box(plotOutput("hist3"),width = 6),
                                                             box(verbatimTextOutput("summary3"),width=6))),
                                           tabPanel("MINORD",
                                                    fluidRow(box(div(tags$img(src = "https://i.postimg.cc/4yrP5hkZ/Tabla-de-Frecuencia-del-min-Ord.png", height = "400px", width="400"), style="text-align: center;")),
                                                             box(plotOutput("hist4"),width = 6),
                                                             box(verbatimTextOutput("summary4"),width=6)))))),
                      # CONTENIDO DEL ANALISIS BIVARIADO --------------
                      tabItem(tabName = "analis-biva",
                              h2("ANALISIS BIVARIANTES",align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("MATRIZ DE CORRELACION",plotOutput("matcov")),
                                           tabPanel("MATRIZ DE VARIANZA Y COVARIANZA",
                                                    div(tags$img(src = "https://i.postimg.cc/L863jkYj/Matriz-de-Varianza-y-Covarianza.png", height = "700px", width="900"), style="text-align: center;")),
                                           tabPanel("DIAGRAMA DE CAJAS ", plotlyOutput("boxplot")),
                                           tabPanel("MODELO DE REGRESION LINEAL",
                                                    fluidRow(box(plotOutput("plotpairs"),width = 6),
                                                             box(verbatimTextOutput("summary5"),width=6))),
                                           tabPanel("GRAFICO DE LA REGRESION LINEAL",highchartOutput("hc_linearmodel"))))),
                      #CONTENIDO DE LA ESTADISTICA INFERENCIAL----------
                      # BONDAD DE AJUSTE
                      tabItem(tabName = "bond-ajust",
                              h2("ANALISIS INFERENCIALES",align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("TABLA DE LA BONDAD DE AJUSTE",div(tags$img(src = "https://i.postimg.cc/VNy2sX0H/Bondad-de-ajsute-tempac1-version2.png", height = "700px", width="1200"), style="text-align: center;"))))),
                      # PRUEBAS DE HIPOTESIS E INTERVALOS DE CONFIANZA
                      tabItem(tabName = "hi-ic",
                              h2("ANALISIS INFERENCIALES",align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("TABLA P. HIPOTESIS E I. CONFIANZA TEMPAC1",div(tags$img(src = "https://i.postimg.cc/JnSkS72J/PH-e-IC-Tempac1-version-2.png", height = "700px", width="1200"), style="text-align: center;")),
                                           tabPanel("TABLA P. HIPOTESIS E I. CONFIANZA TEMPAC2",div(tags$img(src = "https://i.postimg.cc/m2YH6sNz/PH-e-IC-Temp-Ac2-version-2.png", height = "700px", width="1200"), style="text-align: center;"))))),
                      # ANALISIS DE CONTINGENCIA
                      tabItem(tabName = "analis-contin",
                              h2("ANALISIS INFERENCIALES",align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("TABLA DE CONTINGENCIA TIPO Y ESPECIE",div(tags$img(src = "https://i.postimg.cc/8P6X5QRX/Tabla-de-contingencia-1.png", height = "500px", width="1600"), style="text-align: center;")),
                                           tabPanel("PRUEBA DE HIPOTESIS TIPO Y ESPECIE",div(tags$img(src = "https://i.postimg.cc/rmsQ2vyJ/PH-Analisis-de-contingencia-1.png", height = "700px", width="1400"), style="text-align: center;")),
                                           tabPanel("TABLA DE CONTINGENCIA TIPO Y OPERADOR",div(tags$img(src = "https://i.postimg.cc/yY0qGSbN/Tabla-de-contingencia-2.png", height = "500px", width="1400"), style="text-align: center;")),
                                           tabPanel("PRUEBA DE HIPOTESIS TIPO Y OPERADOR",div(tags$img(src = "https://i.postimg.cc/wxhM9ktn/PH-Analisis-de-contingencia-2-version-2.png", height = "700px", width="1400"), style="text-align: center;"))))),
                      # CONTENIDO MULTIVARIANTE ------------------
                      # ANALISIS DE CORRELACION CANONICA
                      tabItem(tabName = "acc",
                              h2("ANALISIS DE CORRELACION CANONICA",align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("MATRIZ DE CORRELACION DE LOS DATOS",div(tags$img(src = "https://i.postimg.cc/k5RDBm5B/image-2023-01-16-200450332.png", height = "700", width="1600"), style="text-align: center;")),
                                           tabPanel("CORRELACION ENTRE LAS VARIABLES CANONICAS",div(tags$img(src = "https://i.postimg.cc/cHsqPRP0/image-2023-01-16-200615590.png", height = "400", width="1600"), style="text-align: center;")),
                                           tabPanel("COMBINACIONES LINEALES",
                                                    fluidRow(column(10,div(tags$img(src = "https://i.postimg.cc/T1JJFmMM/image-2023-01-16-200816740.png", height = "200", width="1650"), style="text-align: center;")),
                                                             column(10,div(tags$img(src = "https://i.postimg.cc/5tHdJ92k/image-2023-01-16-201254525.png", height = "200px", width="1650"), style="text-align: center;"))))))),
                      # ANALISIS DE COMPONENTES PRINCIPALES
                      tabItem(tabName = "acp",
                              h2("ANALISIS DE COMPONENTES PRINCIPALES",align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("BIPLOT SIN ROTACION",plotOutput("biplotsin")),
                                           tabPanel("BIPLOT CON ROTACION VARIMAX",plotOutput("biplotcon")),
                                           tabPanel("PORCENTAJE DE VARIANZA EXPLICADA",plotOutput("porcentexp")),
                                           tabPanel("MATRIZ DE CORRELACION",plotOutput("matcov2"))))),
                      # ANALISIS FACTORIAL
                      tabItem(tabName = "af",
                              h2("ANALISIS FACTORIAL",align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("BIPLOT SIN ROTACION",plotOutput("biplotsin2")),
                                           tabPanel("BIPLOT CON ROTACION QUARTIMAX",plotOutput("biplotcon2")),
                                           tabPanel("CIRCULO UNITARIO",plotOutput("cirunit")),
                                           tabPanel("TEST BARLETT-KMO-DETERMINANTE",
                                                    fluidRow(box(verbatimTextOutput("barlett"),width = 4),
                                                             box(verbatimTextOutput("kmo"),width = 4),
                                                             box(verbatimTextOutput("determinante"),width = 4)))))),
                      # ESCALADO MULTIDIMENSIONAL 
                      tabItem(tabName = "mds",
                              h2("ESCALADO MULTIDIMENSIONAL",align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("DIAGRAMA DE SHEPPARD-VALOR DEL STRESS",
                                                    fluidRow(box(plotOutput("sheppard"),width = 6),
                                                             box(verbatimTextOutput("stress"),width = 6))),
                                           tabPanel("PORCENTAJE DE VARIANZA EXPLICADA",plotOutput("porcentexp2"))))),
                      # ANALISIS DE CORRESPONDENCIA
                      tabItem(tabName = "ac",
                              h2("ANALISIS DE CORRESPONDENCIA",align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("ANALISIS DE CORRESPONDENCIA",highchartOutput("acm"))))),
                      # ANALISIS DE CONGLOMERADOS
                      tabItem(tabName = "acg",
                              h2("ANALISIS DE CONGLOMERADOS",align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("MÉTODO DEL VECINO MÁS PRÓXIMO",plotOutput("vecprox")),
                                           tabPanel("MÉTODO DEL PROMEDIO",plotOutput("metprom")),
                                           tabPanel("MÉTODO DE WARD",plotOutput("metward")),
                                           tabPanel("CLUSTER DENDOGRAM",plotOutput("cluster"))))),
                      # ANALISIS DE DISCRIMINANTE
                      tabItem(tabName = "ad",
                              h2("ANALISIS DE DISCRIMINANTE",align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("GRÁFICO DEL ANÁLISIS DISCRIMINANTE",plotOutput("adiscri")))))
                      
                      
                      )))
                                                    
                                           
                      
                       
                                                    
                                         
                      
                      
                      
                      
                                                             



server <- function(input, output) {
  output$tabla1 <- renderDataTable({
    datos
    
  })
  output$barchart1 <- renderPlot({
    #Diagrama de barras segun el operador
    ggplot(datos, aes(x=Operador, fill=Operador)) +  geom_bar( ) +  scale_fill_brewer(palette = "Set1") +
      theme(legend.position="right")
    
  })
  output$barchart2 <- renderPlot({
    #Diagrama de barras segun el operador
    ggplot(datos, aes(x=TIPO, fill=TIPO)) +  geom_bar( ) +  scale_fill_brewer(palette = "Set1") +
      theme(legend.position="right")
    
  })
  output$barchart3 <- renderPlot({
  ggplot(datos, aes(x=ESPECIE, fill=ESPECIE)) +  geom_bar( ) +  scale_fill_brewer(palette = "Set3") +
    theme(legend.position="right")
    
  })
  output$barchart4 <- renderPlot({
  ggplot(datos, aes(x=mes, fill=mes)) +  geom_bar( ) +  scale_fill_brewer(palette = "Set3") +
    theme(legend.position="right")
  })
  
  output$hist1 <- renderPlot({
  # Histograma del Nbatch
    tfrecuencia_nbatch <- fdt(datos$NBatch)
  plot(tfrecuencia_nbatch, type = "fh", col="lightyellow", xlab = "Numero de proceso por lote", ylab = "Frecuencias", main = "Numero de procesos por Lote")
  })
  
  output$summary1 <- renderPrint({
    summary(datos$NBatch)
  })
  
  output$hist2 <- renderPlot({
    # Histograma del CapLinea
    tfrecuencia_CapLinea <- fdt(datos$CapLinea)
    plot(tfrecuencia_CapLinea, type = "fh", col="green", xlab = "Capacidad de la maquina", ylab = "Frecuencias", main = "Capacidad de la maquina")
  })
  
  output$summary2 <- renderPrint({
    summary(datos$CapLinea)
  })
  
  output$hist3 <- renderPlot({
    # Histograma del CapLinea
    tfrecuencia_Hmezcla <- fdt(datos$HdMezcladora)
    plot(tfrecuencia_Hmezcla, type = "fh", col="lightblue", xlab = "Humedad de la mezcla por producto", ylab = "Frecuencias", main = "Humedad de la mezcla por producto")
  })
  
  output$summary3 <- renderPrint({
    summary(datos$HdMezcladora)
  })
  
  output$hist4 <- renderPlot({
    # Histograma del CapLinea
    tfrecuencia_minOrd <- fdt(datos$minOrd)
    plot(tfrecuencia_minOrd, type = "fh", col="pink", xlab = "Numero de minutos por orden", ylab = "Frecuencias", main = "Numero de minutos por orden")
  })
  
  output$summary4 <- renderPrint({
    summary(datos$minOrd)
  })
  
  output$matcov <- renderPlot({
    # Matriz de correlacion de forma grafica
    r.datos <- cor(datos[,13:16])
    cor.plot(r.datos, numbers = TRUE)
  })
  output$plotpairs <- renderPlot({
    pairs(datos[,11:16], panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")})
  })
  
  output$summary5 <- renderPrint({
  fit1 <- lm(data = datos, TempAc1 ~ TempAc2)
  summary(fit1)
  })
  
  output$hc_linearmodel <- renderHighchart({
  fit1 <- lm(data = datos, TempAc1 ~ TempAc2)
  modelo1 <- augment(fit1) %>% arrange(TempAc2)
  
  hc <- datos %>% hchart('scatter', hcaes(x = TempAc2, y = TempAc1)) %>%
    hc_add_series(modelo1, type = "line", hcaes(x = TempAc2, y = .fitted), name = "Fit", id = "fit")
  
  hc
  })
  output$boxplot <- renderPlotly({
    fig <- plot_ly(type = 'box')
    
    fig <- fig %>% add_boxplot(y = datos$TempAc1, name = "Temperatura de acondicionamiento 1", boxpoints = FALSE,
                               marker = list(color = 'rgb(9,56,125)'),
                               line = list(color = 'rgb(9,56,125)'))
    fig <- fig %>% add_boxplot(y = datos$TempAc2, name = "Temperatura de acondicionamiento 2", boxpoints = 'suspectedoutliers',
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
  })
  output$biplotsin <- renderPlot({
    acp <- prcomp(datos2, scale. = TRUE)
    # BIPLOT SIN ROTACION
    fviz_pca_biplot(acp, repel = TRUE)
  })
  output$biplotcon <- renderPlot({
    # BIPLOT CON ROTACION VARIMAX
    biplot.psych(fa(datos2, nfactors=2, fm="pa", rotate="varimax"), main=paste("Biplot con rotación","varimax"), col=c(2,3,4),pch=c(21,18))
  })
  output$porcentexp <- renderPlot({
    # Porcentaje de varianza explicada
    fviz_screeplot(acp,addlabels=TRUE, ylim=c(0,70))
  })
  output$matcov2 <- renderPlot({
    # Matriz de correlacion de forma grafica
    r.datos2 <- cor(datos2)
    cor.plot(r.datos2, numbers = TRUE)
  })
  output$biplotsin2 <- renderPlot({
    biplot.psych(fa(datos2, nfactors=2, fm="pa", rotate="none"), main=paste("Biplot sin rotación"), col=c(2,3,4),pch=c(21,18))
  })
  output$biplotcon2 <- renderPlot({
    # BIPLOT CON ROTACION QUARTIMAX
    biplot.psych(fa(datos2, nfactors=2, fm="pa", rotate="Promax"), main=paste("Biplot con rotación","quartimax"), col=c(2,3,4),pch=c(21,18))
  })
  output$cirunit <- renderPlot({
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
    p1
  })
  output$barlett <- renderPrint({
    bartlett.test(datos2)
  })
  output$kmo <- renderPrint({
    KMO(datos2)
  })
  output$determinante <- renderPrint({
    R=cor(datos2)
    det(R)
  })
  output$sheppard <- renderPlot({
    em<-isoMDS(as.dist(datos3))
    
    shep<-Shepard(as.dist(datos3),em$points)
    plot(shep,pch=20,cex=0.4)
    lines(shep$x,shep$y,type = "S")
  })
  output$stress <- renderPrint({
    (em$stress)
  })
  output$porcentexp2 <- renderPlot({
    # Porcentaje de varianza explicada
    fviz_screeplot(prcomp(datos3, scale. = TRUE),addlabels=TRUE, ylim=c(0,70))
  })
  output$acm <- renderHighchart({
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
  })
  output$vecprox <- renderPlot({
    dimnames(df)=list(seq(1:28),seq(1:28))
    y<- as.dist(df)
    cl <- hclust(y,method="single")
    plot(cl,hang = -1,main="Dendograma: método del vecino más próximo",xlab="Objetos",ylab="Distancia")
    abline(h=4.5,lty=2)
  })
  output$metprom <- renderPlot({
    y<- as.dist(df)
    cl <- hclust(y,method="average")
    plot(cl,hang = -1,main="Dendograma: método del promedio",xlab="Objetos",ylab="Distancia")
  })
  output$metward <- renderPlot({
    y<- as.dist(df)
    cl <- hclust(y,method="ward")
    plot(cl,hang = -1,main="Dendograma: método de Ward",xlab="Objetos",ylab="Distancia")
  })
  output$cluster <- renderPlot({
    #distancia maxima entre dos componentes de X eY
    dd <-dist(df,method = "maximum")
    #distacia de manhattan
    dd <-dist(df,method = "manhattan")
    #distancia de canberra
    dd <-dist(df,method = "canberra")
    #distancia de minkowski
    dd <-dist(df,method = "minkowski")
    #distancia euclidiana
    dd <-dist(df)
    #conformacion de los conglomerados
    cl <- hclust(dd)
    plot(cl,hang=-1)
  })
  output$adiscri <- renderPlot({
    modelo = lda(ESPECIE~CantKgProd+Granulometria+PorcAgua, data = datos5)
    #modelo
    
    lda.data <- cbind(datos5, predict(modelo)$x)
    ggplot(lda.data, aes(LD1, LD2)) +
      geom_point(aes(color = ESPECIE))
  })
  
  
  
}

shinyApp(ui = ui, server = server)
