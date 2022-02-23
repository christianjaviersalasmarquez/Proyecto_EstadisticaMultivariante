options(encoding = 'UTF-8')
library(shiny)
library(shinydashboard)
library(readr)
library(haven)
library(DT)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(plotly)
library(agricolae)
library(haven) 
library(corrplot)
library(corrr) 
library(highcharter)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(GPArotation)
library(psych)
library(nFactors)
library(knitr)
library(reshape2)
library(foreign)
library(ca)
library(stringr)
library(FactoMineR)
library(ade4)
library(FactoClass)
library(factoextra)
library(missMDA)
library(ggpubr)
library(tidyr)
####
introPrincipal <- h3(style="text-align: justify;", "El presente trabajo investigativo forma parte de la materia Estadística Multivariante de la Escuela Superior Politécnica del Litoral, el cual representa una compilación de los conocimientos adquiridos durante el curso. Este trabajo, se titula “Análisis Multivariante de las características de los Tests Psicométricos”, el cual tiene como objetivo la aplicación los conocimientos obtenidos durante el curso. Así mismo, se busca develar las técnicas multivariantes aprendidas, de modo que este trabajo investigativo pueda aportar a toma decisiones en el campo de la psicología validándose del análisis que daremos a este trabajo.
En nuestra sociedad la salud mental ha sido uno de los temas con mayor crecimiento de interés en las últimas décadas, de modo que las técnicas de psicología clínica se afianzan de la estadística para obtener resultados que se correlacionen para su debida interpretación y toma de decisión con dichos resultados. Este trabajo está estructurado por un análisis descriptivo seguido de un análisis multivariante. Se utilizan las diversas técnicas multivariantes y se presentan las respectivas interpretaciones para que este trabajo no se vuelva solo un conglomerado de ecuaciones y cálculos.
Las variables de respuesta del estudio surgen de un grupo de individuos de las zonas aledañas del cantón Milagro. Los datos serán tratados estadísticamente para investigar las variables explicativas (dependientes a cada observación). Por último, se presentarán los anexos, bibliografías y fuentes que se han utilizado en este trabajo académico.")


integrantes <- h3("Christian Salas M., John Borbor M., Pedro Goya C.")


objetivo <- h3(style="text-align: justify;","Examinar los atributos psicométricos de múltiples test psicológicos y comparar los resultados obtenidos mediante la utilización de las técnicas multivariantes adecuadas.")

objetivos_secundarios <- h3(style="text-align: justify;", "*Comparar y analizar la información obtenida del Mini-Mental Test Examination (MMSE) en un cuestionario de 30 ítems dicotómicos de datos binarios.
*Medir la velocidad del pensamiento cognitivo y respuestas del funcionamiento ejecutivo del cerebro a través de un Trail Making Test (TMT).
*Medir y analizar las variables dentro de un cuestionario de apoyo social.
*Comparar, analizar y correlacionar la información del Geriatric Depression Scale (GDS) en el test de síntomas depresivos, y así obtener información de interés para nuestro estudio.
*Contrastar los resultados obtenidos con las diversas metodologías de la Teoría de la Respuesta al Ítem (TRI) y compararlos con los obtenidos en el Análisis Factorial tradicional.
*Aplicar técnicas Biplot que sean coherentes con las características de los datos de este estudio, y usar la representación gráfica con un Dashboard de modo que la descripción del proyecto y su análisis queden cohesionados.")


t1 <- p(style="text-align: justify;","Prueba xi cuadrado para la tabla de contingencia utilizando chisq.test(tabla). Valor p es 0.002. Por lo tanto dado que el valor p es
menor que 0.05, rechazamos la hipótesis nula. No hay independencia estocástica, es decir la información
sobre la ocurrencia de uno modifica la probabilidad de que ocurra el otro. i.e. existe dependencia entre el
género y si tiene depresión o no. En este caso, a partir de la tabla vemos que el género femenino tiene más
prevalencia de sufrir depresión.") 

t2 <- p(style="text-align: justify;","El valor p es de 0.3. Dado el nivel de significancia alpha = 0.05, no rechazamos la hipótesis nula. Es decir,
existe independencia entre el grupo etáreo y si el examinado tiene depresión o no.")

conclusion <- h3(style="text-align: justify;", "  El método Biplot ha demostrado ser una herramienta gráfica pertinente y útil para el analisis de los datos derivados de las aplicaciones del Mini mental State Examination (MMSE). La representacion grafica en este trabajo en base a test psicometricos tuvo una enorme ventaja ante las presentaciones de analisis descriptivos. En el analisis de datos multivariantes no solo se inciden las caracteristicas de las tecnicas que se usan, sino que observar como influye la manera en la que los datos. Como pudimos observar, los patrones de las respuestas tienen cierta relacion con el invididuo que contesta un item de los diversos test. Esto extiende como se considera una respuesta binaria en un test hacia el campo de otras ciencias como la psicometria" ) 

#Data
options(warn = -1)
#datos <- read_sav("BASE-FINAL.sav")
datos <- read.spss("BASE-FINAL.sav", to.data.frame = TRUE, use.value.labels = FALSE)
df <- datos
##
encuesta <- datos[,1:18]
MMST <- datos[,19:50]
TMT <- datos[,51:54]
UPPS <- datos[,55:105]
GDS <- datos[,109:124]
TEST_SENTIMIENTOS <- datos[,126:138]
#Data Separada
genero = datos$Género #categórica
edad = datos$Edad #Cuantitativa
estudios = datos$Estudios #categórica
ocupacion = datos$Ocupación
residencia = datos$Residencia
atencion = datos$Atención
discapacidad = datos$Discapacidad
jubilado = datos$Jubilado
satisfaccion = datos$Satisfacción
percepcion = datos$Percepción
deprimido = datos$Deprimido
memoria = datos$Memoria
fuma = datos$Fuma
alcohol = datos$Alcohol
enfermedad = datos$Enfermedad
demencia = datos$Demencia
expuesto = datos$Expuesto
MM1 = datos$MM1
MM2 = datos$MM2
MM3 = datos$MM3
MM4 = datos$MM4
MM5 = datos$MM5
MM6 = datos$MM6
MM7 = datos$MM7
MM8 = datos$MM8
MM9 = datos$MM9
MM10 = datos$MM10
MM11 = datos$MM11
MM12 = datos$MM12
MM13 = datos$MM13
MM14 = datos$MM14
MM15 = datos$MM15
MM16 = datos$MM16
MM17 = datos$MM17
MM18 = datos$MM18
MM19 = datos$MM19
MM20 = datos$MM20
MM21 = datos$MM21
MM22 = datos$MM22
MM23 = datos$MM23
MM24 = datos$MM24
MM25 = datos$MM25
MM26 = datos$MM26
MM27 = datos$MM27
MM28 = datos$MM28
MM29 = datos$MM29
MM30 = datos$MM30
calificacion = datos$Calificación 
notaGDS <- datos$Calificacióndp
resulltadoMME <- datos$RMM
#Cuantitativa
RMM = datos$RMM
##Area de trabajo
#Descriptivas
hist.edades <- plot_ly(x = edad ,type = "histogram",histnorm = "probability", name = "Distribución de la edad de los pacientes")
hist.nota.MM <- plot_ly(x = calificacion ,type = "histogram",histnorm = "probability", name = "Distribución de las calificaciones del test Mini Mental Examination")
hist.nota.TMT <- plot_ly(x = TMT ,type = "histogram",histnorm = "probability", name = "Distribución de la edad de los pacientes")
hist.UPPS <- plot_ly(x = edad ,type = "histogram",histnorm = "probability", name = "Distribución de la edad de los pacientes")
hist.GDS <- plot_ly(x = notaGDS ,type = "histogram",histnorm = "probability", name = "Resultado de Depresión Geriatriaca")
#
residencia <- as.character(residencia)
residencia[residencia == '1'] <- 'Rural'
residencia[residencia == '2'] <- 'Urbano'
hist.residencia <- plot_ly(x = residencia,type = "histogram",histnorm = "probability", name = "Residencia de los encuestados")
genero <- as.character(genero)
genero[genero == '1'] <- 'Masculino'
genero[genero == '2'] <- 'Femenino'
hist.sexo <- plot_ly(x = genero,type = "histogram",histnorm = "probability", name = "Comparacion de Genero")
educacion <- as.character(estudios)
educacion[educacion == '1'] <- 'Primaria'
educacion[educacion == '2'] <- 'Secundaria'
educacion[educacion == '3'] <- 'Universitaria/Superior'
hist.estudios <- plot_ly(x = educacion,type = "histogram",histnorm = "probability", name = "Nivel de estudios alcanzados")
deprimido <- as.character(deprimido)
deprimido[deprimido == '1'] <- 'Si'
deprimido[deprimido == '0'] <- 'No'
hist.deprimido <- plot_ly(x = deprimido,type = "histogram",histnorm = "probability", name = "Perecepción sobre su estado de animo")
demencia <- as.character(demencia)
demencia[demencia == '1'] <- 'Si'
demencia[demencia== '0'] <- 'No'
demencia[demencia== '2'] <- 'Blanco'
hist.antecedentes.demencia <- plot_ly(x = demencia,type = "histogram",histnorm = "probability", name = "Antecedentes familiares de demencia")
resulltadoMME <- as.character(resulltadoMME)
resulltadoMME[resulltadoMME  == '1'] <- 'Posee rasgos de demencia'
resulltadoMME[resulltadoMME  == '0'] <- 'No posee rasgos de demencia'
hist.resultad1 <- plot_ly(x =  resulltadoMME ,type = "histogram",histnorm = "probability", name = "Resultados MME")
# AF
o_temporal = MM1+MM2+MM3+MM4+MM5 # Puntaje para Orientación Temporal
o_espacial = MM6+MM7+MM8+MM9+MM10 # Puntaje para Orientación Espacial
fijacion = MM11+MM12+MM13
atencion_calculo = MM14+MM15+MM16+MM17+MM18
memoria = MM19+MM20+MM21
nominacion = MM22+MM23
repeticion = MM24
comprension = MM25+MM26+MM27
lectura = MM28
escritura = MM29
copia = MM30
##

#Multivariante ACM

datos$RangoEdades[datos$Edad>=40 & datos$Edad < 65] = 'Edad entre 40 y 64'
datos$RangoEdades[datos$Edad>=65 & datos$Edad < 80] = 'Edad entre 65 y 79'
datos$RangoEdades[datos$Edad>=80] = 'Edad mayor o igual a 80'

genero = datos$Género #categórica
edad = datos$Edad #Cuantitativa
estudios = datos$Estudios #categórica
estado=datos$Estado
ocupacion = datos$Ocupación
residencia = datos$Residencia
atencion = datos$Atención
discapacidad = datos$Discapacidad
jubilado = datos$Jubilado
satisfaccion = datos$Satisfacción
percepcion = datos$Percepción
deprimido = datos$Deprimido
memoria = datos$Memoria
fuma = datos$Fuma
alcohol = datos$Alcohol
enfermedad = datos$Enfermedad
demencia = datos$Demencia
expuesto = datos$Expuesto

rango_edades = datos$RangoEdades

PerfilExaminado <- data.frame(genero, rango_edades, estudios,estado, ocupacion, residencia, atencion, discapacidad, jubilado, satisfaccion, percepcion, deprimido, memoria, fuma, alcohol, enfermedad, demencia, expuesto)

colnames(PerfilExaminado) <- c("Género","Rango de Edad","Estudios","Estado Civil","Ocupación","Residencia","Atención","Discapacidad","Jubilado","Satisfacción Salud","Percepción de la Edad","Deprimido","Memoria","Fuma","Alcohol","Enfermedad","Antecedente Familiar Demencia","Expuesto a Tóxicos")


library(ca)
library(stringr)
library(FactoMineR)
library(ade4)
library(FactoClass)
library(factoextra)
library(missMDA)

acm <- mjca(PerfilExaminado)


acm_summary <- summary(acm)

acm_summary_columns <- acm_summary$columns


coordenadas <- data.frame(acm_summary_columns[,5], acm_summary_columns[,8])/1000




df_variables <- data.frame(acm_summary_columns$name)


df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,2) == 'Gé'] <- 'Género'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,5) == 'Rango'] <- 'Rango.de.Edad'
#df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,26) == 'Rango.de.Edad:Edad entre 4'] <- 'Edad entre 40 y 64'
#df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,26) == 'Rango.de.Edad:Edad entre 6'] <- 'Edad entre 65 y 79'
#df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,20) == 'Rango.de.Edad:Edad m'] <- 'Edad mayor o igual a 80'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,4) == 'Estu'] <- 'Estudios'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,4) == 'Esta'] <- 'Estado.Civil'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,2) == 'Oc'] <- 'Ocupación'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,2) == 'Re'] <- 'Residencia'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,2) == 'At'] <- 'Atención'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,2) == 'Di'] <- 'Discapacidad'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,2) == 'Ju'] <- 'Jubilado'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,2) == 'Sa'] <- 'Satisfacción.Salud'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,3) == 'Per'] <- 'Percepción.de.la.Edad'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,3) == 'Dep'] <- 'Deprimido'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,3) == 'Mem'] <- 'Memoria'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,3) == 'Fum'] <- 'Fuma'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,3) == 'Alc'] <- 'Alcohol'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,3) == 'Enf'] <- 'Enfermedad'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,3) == 'Ant'] <- 'Antecedente.Familiar.Demencia'
df_variables$Grupo[str_sub(df_variables$acm_summary_columns.name,1,3) == 'Exp'] <- 'Expuesto.a.Tóxicos'






w <- str_split(df_variables$acm_summary_columns.name,":")

vector_unlist <- unlist(w)

n = 1:58

nombre_rotulos <- vector_unlist[2*n]
nombre_rotulos <- sub("-",":",nombre_rotulos, fixed=TRUE)
nombre_rotulos <- sub("-",":",nombre_rotulos, fixed=TRUE)

#nombre_rotulos
#str(coordenadas)
#str(nombre_rotulos)
#str(df_variables$Grupo)


coordenadas <- data.frame(coordenadas, nombre_rotulos, df_variables$Grupo)
#coordenadas
coordenadas <- coordenadas[coordenadas$nombre_rotulos != 'Blanco',]
#coordenadas




library(highcharter)
library(dplyr)



hc <- highchart()%>%
  hc_title(text = "Perfil del Examinado")%>%
  hc_subtitle(text="Análisis de Correspondencia Múltiple")%>%
  hc_subtitle(text="")%>%
  hc_subtitle(text="")%>%
  hc_add_series(coordenadas, type='scatter',hcaes(x=acm_summary_columns...5., y=acm_summary_columns...8., name=nombre_rotulos, group=df_variables.Grupo),
                dataLabels=list(format="{point.name}",enabled=TRUE), 
                tooltip=list(pointFormat="{point.name}"))%>%
  
  hc_xAxis(
    title = list(text = "Dimensión 1"),
    plotLines = list(list(value=0, color = 'lightgreen',width=2,zIndex=4,label=list(text="",style=list(color='lightblue', fontWeight='bold')))))%>%
  
  hc_yAxis(
    title = list(text = "Dimensión 2"),
    plotLines = list(list(value=0, color = 'lightgreen',width=2,zIndex=4,label=list(text="",style=list(color='lightblue', fontWeight='bold')))))



###### 
#TRI
library(mirt)
library(shiny)
library(WrightMap)

datos_tri <- data.frame(MM1,MM2,MM3,MM4,MM5,MM6,MM7,MM8,MM9,MM10,MM11,MM12,MM13,MM14,MM15,MM16,MM17,MM18,MM19,MM20,MM21,MM22,MM23,MM24,MM25,MM26,MM27,MM28,MM29)

modelo_tri <- mirt(datos_tri, 1, itemtype = '2PL')

latentes <- as.vector(fscores(modelo_tri))
#latentes

calificacion_MMSE <- apply(datos_tri, 1, sum)

medidas <- data.frame(edad, calificacion_MMSE, latentes)

tri_cci <- plot(modelo_tri, type = 'trace')

tri_iitl <- plot(modelo_tri, type = 'infotrace')

modelo_tri_coef <- coef(modelo_tri, simplify = T, IRTpars = T)

#tri_wright <-wrightMap(latentes, modelo_tri_coef$items[,2])
#############

# AF
library(ggcorrplot)

o_temporal = MM1+MM2+MM3+MM4+MM5 # Puntaje para Orientación Temporal
o_espacial = MM6+MM7+MM8+MM9+MM10 # Puntaje para Orientación Espacial
fijacion = MM11+MM12+MM13
atencion_calculo = MM14+MM15+MM16+MM17+MM18
memoria = MM19+MM20+MM21
nominacion = MM22+MM23
repeticion = MM24
comprension = MM25+MM26+MM27
lectura = MM28
escritura = MM29
copia = MM30

df_apartados_MMSE = data.frame(o_temporal, o_espacial, fijacion, atencion_calculo, memoria, nominacion, repeticion, comprension, lectura, escritura, copia)

df_apartados_MMSE_omit = na.omit(df_apartados_MMSE)

r.datos <- cor(df_apartados_MMSE_omit)

#grafCorr <- cor.plot(r.datos, numbers = TRUE)

grafCorr <- ggcorrplot(r.datos)












#Header 
header <- dashboardHeader(title="PROYECTO")
#Sidebar

sidebar <- dashboardSidebar(
  sidebarMenu(menuItem(text = "Introducción", tabName = "intro"),
              menuItem(text = "Datos", tabName = "data",
                       menuSubItem(text = "Encuesta Socio Economica",tabName = "formulario"),
                       menuSubItem(text = "Test Mini Mental",tabName = "dataMMT"),
                       menuSubItem(text = "Trail Making Test ",tabName = "dataTMT"),
                       menuSubItem(text = "Test Escala de Impulsividad ",tabName = "dataUPPS"),
                       menuSubItem(text = "Test Sintomas Depresivos ",tabName = "dataGDS")),
              
              menuItem(text = "Inferencial", tabName = "inferencial"),
              
              menuItem(text = "Estadística Descriptiva", tabName = "descriptivos",
                       menuSubItem(text = "Resultados de la encuesta socio economica",tabName = "resultados1"),
                       menuSubItem(text = "Resultados de los Test Psicometricos",tabName = "notas")),
              
              menuItem(text = "Estadística Multivariante", tabName = "multivariante",
                       menuSubItem(text = "Mini Mental Test Examination",tabName = "perfilMME"),
                       menuSubItem(text = "Correlación entre variables MME",tabName = "corMME"), 
                       menuSubItem(text="Teoría de Respuesta al Item", tabName = "TRI")),
              
              menuItem(text = "Conclusiones", tabName = "conclusion")
  )
)

#Cuerpo
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "intro", box(title = strong("Introducción"),introPrincipal), box(title=strong("Integrantes"), integrantes), box(title=strong("Objetivo General"), objetivo), box(title=strong("Objetivos Secundarios"),objetivos_secundarios)),
    tabItem(tabName = "inferencial", box(title = strong("Género vs Deprimido"),t1), box(title=strong("Grupo Etáreo vs Deprimido"), t2)),
    tabItem(tabName = "conclusion", box(title = strong("Conclusiones"),conclusion)),
    tabItem(tabName = "formulario" , fluidRow(column(DT::dataTableOutput("encuesta"),width = 12))),
    tabItem(tabName = "dataMMT" , fluidRow(column(DT::dataTableOutput("MMST"),width = 12))),
    tabItem(tabName = "dataTMT" , fluidRow(column(DT::dataTableOutput("TMT"),width = 12))),
    tabItem(tabName = "dataUPPS" , fluidRow(column(DT::dataTableOutput("UPPS"),width = 12))),
    tabItem(tabName = "dataGDS" , fluidRow(column(DT::dataTableOutput("GDS"),width = 12))),
    tabItem(tabName = "resultados1",
            fluidRow( column(width = 3,box(title = "Edades", plotlyOutput("hist.edades"),width = 12)), 
                      column(width = 3,box(title = "Sexo",   plotlyOutput("hist.sexo"),width = 12)),
                      column(width = 3,box(title = "Estudios",   plotlyOutput("hist.estudios"),width = 12)),
                      column(width = 3,box(title = "Residencia",   plotlyOutput("hist.residencia"),width = 12)), 
                      column(width = 3,box(title = "Percepción de su estado de animo",   plotlyOutput("hist.deprimido"),width = 12))
            )),
    
    tabItem(tabName = "notas",
            fluidRow( column(width = 3,box(title = "Resultados Mini Mental test", plotlyOutput("hist.nota.MM"),width = 12)),
                      column(width = 3,box(title = "Resultados del MME", plotlyOutput("hist.resultad1"),width = 12)),
                      column(width = 3,box(title = "Resultados Trail Making Test", plotlyOutput("hist.nota.TMT"),width = 12)),
                      column(width = 3,box(title = "Antecedentes familiares de demencia", plotlyOutput("hist.antecedentes.demencia"),width = 12)),
                      column(width = 3,box(title = "Resultados Cuestionario Impulsividad", plotlyOutput("hist.UPPS"),width = 12)),
                      column(width = 3,box(title = "Resultados Geriatric Depression Test", plotlyOutput("hist.GDS"),width = 12))
                      
                      
                      
            )),
    
    
    
    tabItem(tabName = "perfilMME", 
            fluidRow( column(width = 12, box(title = strong("ACM Perfil del Examinado"), highchartOutput("hcontainer") ,width = 12))),
            
            tabItem(tabName = "corMME",
                    fluidRow( column(width = 12, box(title = strong("Correlaciones entre variables del Mini Mental Test"), plotOutput("grafCorr") ,width = 12))
                    )
            ),
            
            tabItem(tabName = "TRI",
                    fluidRow( column(width = 12, box(title = strong("CCI Teoría de Respuesta al Item MMSE"), plotOutput("tri_cci") ,width = 12))
                    )
            )
            
    )
  ))

#UI
ui <- dashboardPage(title= "Dashboard", skin= "purple",
                    
                    header = header,
                    sidebar = sidebar,
                    body = body
)

#Servidor
server <- function(input, output) {
  output$encuesta <- renderDataTable(encuesta)
  output$MMST <- renderDataTable(MMST)
  output$TMT  <- renderDataTable(TMT)
  output$UPPS <- renderDataTable(UPPS)
  output$GDS <- renderDataTable(GDS)
  output$hist.edades  <- renderPlotly(hist.edades)
  output$hist.sexo  <- renderPlotly(hist.sexo)
  output$hist.estudios  <- renderPlotly(hist.estudios)
  output$hist.residencia  <- renderPlotly(hist.residencia) 
  output$hist.deprimido  <- renderPlotly(hist.deprimido)
  output$hist.antecedentes.demencia  <- renderPlotly(hist.antecedentes.demencia)
  output$hist.resultad1  <- renderPlotly(hist.resultad1)
  output$hist.nota.MM  <- renderPlotly(hist.nota.MM)
  output$hist.nota.TMT  <- renderPlotly(hist.nota.TMT)
  output$hist.UPPS  <- renderPlotly(hist.UPPS)
  output$hist.GDS <- renderPlotly(hist.GDS)
  output$hcontainer <- renderHighchart(hc)
  
  output$grafCorr  <- renderPlot(grafCorr) 
  
  output$tri_cci <- renderPlot(tri_cci)
}
#App0
shinyApp(ui, server)