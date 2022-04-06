if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(agricolae)) install.packages("agricolae", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(corrr)) install.packages("corrr", repos = "http://cran.us.r-project.org")
if(!require(highcharter)) install.packages("highcharter", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(GPArotation)) install.packages("GPArotation", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(nFactors)) install.packages("nFactors", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")
if(!require(ca)) install.packages("ca", repos = "http://cran.us.r-project.org")
if(!require(FactoMineR)) install.packages("FactoMineR", repos = "http://cran.us.r-project.org")
if(!require(ade4)) install.packages("ade4", repos = "http://cran.us.r-project.org")
if(!require(FactoClass)) install.packages("FactoClass", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(missMDA)) install.packages("missMDA", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(mirt)) install.packages("mirt", repos = "http://cran.us.r-project.org")
if(!require(WrightMap)) install.packages("WrightMap", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")


#Data
options(warn = -1)
#datos <- read_sav("BASE-FINAL.sav")
datos <- read.spss("BASE-FINAL.sav", to.data.frame = TRUE, use.value.labels = FALSE)
df <- datos

# Secciones del dataset
encuesta <- datos[,1:18]
MMST <- datos[,19:50]
TMT <- datos[,51:54]
UPPS <- datos[,55:105]
GDS <- datos[,109:124]
TEST_SENTIMIENTOS <- datos[,126:138]

# Variables
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

# Estadística Descriptiva
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
