options(encoding = 'UTF-8')

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



####
introPrincipal <- h3(style="text-align: justify;", "El presente trabajo investigativo forma parte de la materia Estadística Multivariante de la Escuela Superior Politécnica del Litoral, el cual representa una compilación de los conocimientos adquiridos durante el curso. Este trabajo, se titula “Análisis Multivariante de las características de los Tests Psicométricos”, el cual tiene como objetivo la aplicación los conocimientos obtenidos durante el curso. Así mismo, se busca develar las técnicas multivariantes aprendidas, de modo que este trabajo investigativo pueda aportar a toma decisiones en el campo de la psicología validándose del análisis que daremos a este trabajo.
En nuestra sociedad la salud mental ha sido uno de los temas con mayor crecimiento de interés en las últimas décadas, de modo que las técnicas de psicología clínica se afianzan de la estadística para obtener resultados que se correlacionen para su debida interpretación y toma de decisión con dichos resultados. Este trabajo está estructurado por un análisis descriptivo seguido de un análisis multivariante. Se utilizan las diversas técnicas multivariantes y se presentan las respectivas interpretaciones para que este trabajo no se vuelva solo un conglomerado de ecuaciones y cálculos. Los datos serán tratados estadísticamente para investigar las variables explicativas (dependientes a cada observación). Por último, se presentarán los anexos, bibliografías y fuentes que se han utilizado en este trabajo académico.")


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

conclusion <- h3(style="text-align: justify;", " Las técnicas multivariantes utilizadas han demostrado ser unas herramientas útiles para el analisis de los datos derivados de las aplicaciones del Mini mental State Examination (MMSE). La representacion grafica en este trabajo en base a test psicometricos tuvo una enorme ventaja ante las presentaciones de analisis descriptivos. En el analisis de datos multivariantes no solo se inciden las caracteristicas de las tecnicas que se usan, sino que observar como influye la manera en la que los datos. Como pudimos observar, los patrones de las respuestas tienen cierta relacion con el invididuo que contesta un item de los diversos test. Esto extiende como se considera una respuesta binaria en un test hacia el campo de otras ciencias como la psicometria" ) 






#Header 
header <- dashboardHeader(title="Multivariante")
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

#Body
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
            fluidRow( column(width = 12, box(title = strong("ACM Perfil del Examinado"), highchartOutput("hcontainer") ,width = 12)))),
            
    tabItem(tabName = "corMME",
                    fluidRow( column(width = 12, box(title = strong("Correlaciones entre variables del Mini Mental Test"), plotOutput("grafCorr") ,width = 12))
                    )
            ),
            
    tabItem(tabName = "TRI",
                    fluidRow( column(width = 12, box(title = strong("CCI Teoría de Respuesta al Item MMSE"), plotOutput("tri_cci") ,width = 12))
                    )
            )
  ))

#UI
ui <- dashboardPage(title= "Dashboard", skin= "purple",
                    header = header,
                    sidebar = sidebar,
                    body = body)

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