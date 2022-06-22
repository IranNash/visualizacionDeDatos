#################################################################
#                     Gráfico de Dispersión                     #
#################################################################

#Librerias utilizadas
library(tidyverse)
library(khroma)

#Obtención de la base de datos.
materna <- read_csv("https://raw.githubusercontent.com/IranNash/visualizacionDeDatos/main/materna.csv")
#Selección de las variables a utilizar
datos<-materna %>% 
  #Generación la variable nivel de desarrollo utilizando ifelse
  mutate(desarrollo = ifelse(idh < as.numeric(quantile(materna$idh, probs = 0.25, na.rm = TRUE)), "Bajo", 
        ifelse(idh < as.numeric(quantile(materna$idh, probs = 0.75, na.rm = TRUE)), "Medio", "Alto"))) %>% 
  #Ordenamos el factor ingresos por niveles
  mutate(desarrollo = factor(desarrollo, levels = c("Alto","Medio","Bajo"))) %>% 
  #Seleccionamos las variables de interés
  select(mortality, adolescent, desarrollo) %>% 
  #omitimos datos faltantes
  na.omit()

#Cargamos las variables x y y en el gráfico
grafico_base<-ggplot(data = datos, aes(x = mortality, y = adolescent))+
  #Definimos el tamaño de la burbuja y definimos una clase, damos transparencia de 0.5
  geom_point(aes(colour = desarrollo), size = 2)+
  #Agregamos uan linea de regresión
  geom_smooth(formula = "y~x", method = "lm", colour = "gray30", se = FALSE, size = 0.2)+
  #Damos título a la leyenda
  guides(colour=guide_legend(title="Nivel de desarrollo", override.aes = list(size=3)))+
  #Ponemos nombre a los ejes, titulos y notas al pie.
  labs(x = "Ratio de mortalidad materna", 
       y = "Tasa de natalidad adolescente",
       title = "Natalidad adolescente y la mortalidad materna",
       subtitle = "Relación según el nivel de desarrollo del país.",
       caption = " Nota: La información refleja el cálculo de los indicadores entre año 2015 y 2020.
       Elaboración propia con datos del banco mundial, programa de las naciones unidas para
       el desarrollo y la base de datos de polı́ticas globales de aborto, OMS.")+
  scale_colour_highcontrast()

#Definimos Lineas y tipografías del gráfico
theme_personalizado <- theme_minimal()+
  theme(axis.line = element_line(size = 0.2, colour = "black", linetype=1),
        plot.title = element_text(family = "Noto Serif Semibold", size = 22),
        plot.subtitle = element_text(family = "DejaVu Serif", margin = margin(0,0,10,0), size = 15),
        plot.caption = element_text(family = "DejaVu Serif", margin = margin(10,0,0,0)),
        strip.text.x = element_text(family = "DejaVu Serif", size = 8),
        axis.title =  element_text(family = "DejaVu Serif", size = 11),
        axis.text =   element_text(family = "DejaVu Serif", size = 9),
        legend.text = element_text(family = "DejaVu Serif"),
        legend.title = element_text(family = "DejaVu Serif"),
        legend.position = "top", legend.box = "horizontal")

#Agregamos texto a la imagen y la ubicamos en el plano
grafico_texto<-grafico_base +  
    geom_text(x = 1100, y = 75, 
              label = "Hay una relación positiva entre 
natalidad adolescente y RMM",
              family = "DejaVu Serif", stat = "unique",
              size = 4, hjust = "center")+
  #se ajusta la escala para incrementos de 200
  scale_x_continuous(breaks = seq(0,1400,200))+
  #agregamos una flecha curva
  annotate(geom = "curve", x = 1200, y = 100, xend = 1000, yend = 150, 
    curvature = .3, arrow = arrow(length = unit(1, "mm"))) +
  theme_personalizado

print(grafico_texto) 


ggsave(grafico_texto, filename  = "disper1.eps", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

#Se genera el gráfico de facetas a partir del gráfico base
grafico_facetas<-grafico_base + 
  theme_personalizado+
  #Separamos facetas por niveles de ingreso.
  facet_wrap(~desarrollo)+
  #Agregamos las lineas de los ejes omitidas en el primer gráfico
  theme(axis.line = element_line(size = 0.2, colour = "black", linetype=1),
        legend.position = "none",
        panel.spacing = unit(2, "lines"))
print(grafico_facetas) 

ggsave(grafico_facetas, filename  = "disper2.eps", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

#################################################################
#                     Gráfico de Burbujas                       #
#################################################################

#Librerias utilizadas
library(tidyverse)
library(khroma)

ilecdmx <-read.csv("https://raw.githubusercontent.com/IranNash/visualizacionDeDatos/main/ilecdmx.csv")
#Seleccionamos las variables de interes
datos<-ilecdmx %>% select(año, edad, naborto, nivel_edu) %>%
  #Generamos la nueva variable
  mutate(grupos_edad = case_when(
    edad < 18 ~ "menos de 18",
    edad < 30 ~ "18-29",
    edad < 40 ~ "30-39",
    edad < 50 ~ "40-49",
    edad >=50 ~ "50 y más")) %>% 
  na.omit() %>%
  #Le damos a la variable año la propiedad de ser un factor
  mutate(año = as.factor(año)) %>% 
  #Recodificamos la variable nivel_edu
  mutate(nivel_edu = recode(nivel_edu, "SIN ACCESO A LA EDUCACION FORMAL" = "SIN EDUCACION")) %>% 
  #ordenamos los niveles del factor nivel_edu
  mutate(nivel_edu = factor(nivel_edu, levels = c("SIN EDUCACION","PRIMARIA","SECUNDARIA",
                                    "PREPARATORIA","LICENCIATURA","MAESTRIA", "DOCTORADO" ))) %>% 
  #ordenamos los niveles del factor grupos_edad
  mutate(grupos_edad = factor(grupos_edad, 
                              levels = c("menos de 18", "18-29","30-39","40-49", "50 y más"))) 
#Agrupamos las variables grupos_edad,  nivel_edu, año y generamos el total de ILE
datos2 <- datos %>% group_by(grupos_edad,  nivel_edu, año) %>% summarise(total = sum(naborto))

thema_personalizado <- theme_minimal() +
  theme(plot.title = element_text(family = "Helvetica Inserat LT Std", size = 17),
        plot.subtitle = element_text(family = "CMU Classical Serif", margin = margin(0,0,20,0), size = 13),
        axis.text = element_text(family = "Helvetica World"),
        plot.caption = element_text(family = "Helvetica World"),
        axis.title =  element_text(family = "Helvetica World", size = 9),
        strip.text.x = element_text(family = "Helvetica World", size = 10),
        legend.title = element_text(family = "Helvetica World", size = 9),
        legend.text  = element_text(family = "Helvetica World", size = 8))

#Cargamos la vase del gráfico donde en x queremos los años y en y el nivel educativo
grafico_base<-ggplot(data = datos2, aes(x = año, y = nivel_edu, color = nivel_edu))+
  #Definimos el tamaño de la burbuja por el total de ILE, le damos transparencia con alpha
  geom_point(aes(size = total), alpha = 0.9)+
  #Dado que el color es meramente orientativo omitimos la leyenda de color
  theme(legend.position = "none")+
  guides(color="none")+
  #Definimos los rangos de la burbuja y el titulo de la leyenda
  scale_size(range=c(1 , 7), breaks=c(1,100, 300, 600), name = "Número de abortos")+
  #Se generan facetas por el rango de edad
  facet_wrap(~grupos_edad, ncol = 5)+
  #Definimos etiquetas
  labs(x = "", y = "", 
       title = "Interrupciones legales de embarazo en la CDMX por nivel educativo",
       subtitle = "Cálculo por rangos de edad con datos del 2016 al 2021",
       caption = "Elaboracion propia con datos del portal de datos abiertos de la CDMX
       disponible en: https://datos.cdmx.gob.mx/dataset/interrupcion-legal-del-embarazo")+
  #Se agregan tipografías
  thema_personalizado+
  #Se ajusta el ángulo del las etiquetas del eje x, el espacio entre facetas y la posición de la leyenda
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.spacing = unit(0.4, "cm", data = NULL),
        legend.position = "bottom")+
  #Definimos la paleta de color
  khroma::scale_colour_buda(discrete = TRUE)
  #scale_color_manual(values = colorblind_pal)
print(grafico_base)

ggsave(grafico_base, filename  = "burbujas1.eps", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

#Gráfico 2

#Gereramos una variable dicotómica donde 0 muestra si no tuvo abortos previo y 1 en otro caso
datos3 <-datos %>% mutate(aborto = ifelse(naborto == 0, 0, 1)) %>% 
  #agrupamos por edad y nivel educativo
  group_by(edad, nivel_edu) %>%
  #Calculamos el total y el promedio de abortos para estos dos grupos
  summarise(total = sum(aborto, na.rm = TRUE)) %>%
  #Calculamos la proporción de abortos para cada grupo
  mutate(paborto = total / sum(total)) %>% 
  #omitimos datos faltantes
  na.omit() 

#Se seleccionan las variables de interes
grafico_base <-ggplot(data = datos3, aes(x = edad, y = paborto))+
  #El tamaño de la burbuja se asigna según el total de abortos previos
  geom_point(aes(size = total, colour = nivel_edu), alpha = 0.8)+
  #Se asigna la escala de las burbujas así como los rangos
  scale_size(range=c(1 , 7), breaks=c(1, 10, 50, 200))+
  #Se ajusta la escala del eje x
  scale_x_continuous(breaks = seq(10,55,5))+
  #Se asignan las etiquetas
  labs(x = "Edad", y = "Proporción de abortos",
       title = "Proporción de abortos previos a la ILE según la edad en la CDMX",
       subtitle = "Distribución por nivel Educativo de la paciente, total del 2016 - 2021",
       caption = "Nota: La proporción de abortos se calcula con el numero de mujeres que tuvieron 
       almenos un aborto previo entre el total de mujeres registradas para ese grupo de edad.
       Elaboracion propia con datos del portal de datos abiertos de la CDMX
       disponible en: https://datos.cdmx.gob.mx/dataset/interrupcion-legal-del-embarazo")+
  #asignamos tipografías
  thema_personalizado +
  #Se posicionan y se le da titulo a las leyendas
  theme(legend.position = "right")+
  guides(color=guide_legend(title="Nivel educativo", override.aes = list(size=4)))+
  guides(size=guide_legend(title="Número de abortos"))+
  #Se asigna el color
  scale_color_hawaii(discrete = TRUE)+
  #Se coloca un texto que genere una narrativa
  geom_text(x = 36, y = 0.78,
            label = "Las pacientes entre 20 y 30 años
con secundaria y preparatoria son las que han
tenido más abortos previos a la ILE en la clínica.",
            family = "DejaVu Serif", stat = "unique",size = 2.5, hjust = "center")+
  #Se agregan flechas
  annotate(geom = "curve", x = 28, y = 0.72, xend = 22, yend = 0.6, 
           curvature = .3, arrow = arrow(length = unit(1, "mm")))+
  annotate(geom = "curve", x = 35, y = 0.7, xend = 30, yend = 0.5, 
           curvature = -.3, arrow = arrow(length = unit(1, "mm")))

print(grafico_base)

ggsave(grafico_base, filename  = "burbujas2.eps", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

#################################################################
#                     Gráfico de Barras                         #
#################################################################


educacion <- read.csv("https://raw.githubusercontent.com/IranNash/visualizacionDeDatos/main/educacion.csv")

#Definimos las tipografías
theme_personalizado<-theme_minimal()+
  theme(plot.title = element_text(family = "DejaVu Serif", size = 20),
        plot.subtitle = element_text(family = "CMU Classical Serif", 
                                     margin = margin(0,0,20,0), size = 17),
        axis.text = element_text(family = "DejaVu Serif"),
        plot.caption = element_text(family = "DejaVu Serif"),
        axis.title =  element_text(family = "DejaVu Serif", size = 9),
        strip.text.x = element_text(family = "DejaVu Serif", size = 10))


grafico_base<-educacion %>% 
  #ordenamos las categorías de la variable nivel_educativo
  mutate(Nivel_educativo = factor(Nivel_educativo, levels = c("Preescolar", "Primaria", "Secundaria", "Media superior", "Superior"))) %>% 
  #Seleccionamos el ciclo escolar 2020-2021
  filter(ciclo == "2020-2021") %>% 
  #agrupamos por nivel educativo y se calcula para cada nivel el total de escuelas
  group_by(Nivel_educativo) %>% summarise(Total = sum(Escuelas)) %>% 
  #en el eje x colocamos el nivel educativo y en el eje y el total 
  ggplot(aes(x = Nivel_educativo, y = Total, fill=Nivel_educativo)) +
  #Utilizamos la geometría para el gráfico de barras, damos un pequeño contorno a la barra
  geom_bar(stat = "identity", color = "black", size = 0.1)+
  #Quitamos el eje vertical
  scale_y_continuous(breaks=NULL)+
  #Agregamos la paleta de color
  scale_fill_batlowK(discrete = TRUE)+
  #agregamos la etiqueta arriba de la barra
  geom_text(aes(label = Total, family = "DejaVu Serif", size = 2 ), vjust = -0.2)+
  #agregamos las anotaciones del gráfico
  labs(x = "", y = "Total de escuelas",
       title = "Total de Escuelas a nivel nacional",
       subtitle = "Total de Escuelas durante el ciclo 2020-2021 por nivel educativo",
       caption = "Elaboracion propia con datos del INEGI
       disponible en: https://www.inegi.org.mx/temas/educacion//#Tabulados")+
  theme_personalizado+
  #quitamos leyenda y fondo del gráfico
  theme(legend.position = "none",
        panel.grid.major = element_blank())


print(grafico_base)

ggsave(grafico_base, filename  = "barras1.pdf", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

grafico_facetas<-educacion %>% 
  mutate(Nivel_educativo = factor(Nivel_educativo, levels = c("Preescolar", "Primaria", "Secundaria", "Media superior", "Superior"))) %>% 
  group_by(Nivel_educativo, ciclo) %>% summarise(Total = sum(Escuelas)) %>% 
  #Para esta estrategia se visualiza en el eje x el ciclo escolar y en el y la frecuencia
  ggplot(aes(x = ciclo, y = Total, fill = ciclo)) +
  geom_bar(stat = "identity", color = "black", size = 0.1)+
  #Se estratifica por nivel educativo
  facet_wrap(~Nivel_educativo, ncol = 5) +
  theme_personalizado+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #ajustamos las enumeraciones del eje vertical
  scale_y_continuous(breaks = seq(0,100000,10000),
                     labels=function(n){format(n, scientific = FALSE)})+
  #Agregamos el color
  khroma::scale_fill_oslo(discrete = TRUE, reverse = TRUE)+
  #Agregamos las anotaciones
  labs(x = "", y = "Total de escuelas",
       title = "Evolución de la cantidad de escuelas a nivel Nacional",
       subtitle = "Evolución por nivel educativo del ciclo 2000-2001 al 2020-2021",
       caption = "Elaboracion propia con datos del INEGI
       disponible en: https://www.inegi.org.mx/temas/educacion//#Tabulados")
print(grafico_facetas)

ggsave(grafico_facetas, filename  = "barras2.pdf", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

theme_personalizado<-theme_minimal()+
  theme(legend.position = "top",
        plot.title = element_text(family = "DejaVu Serif", size = 17),
        plot.subtitle = element_text(family = "CMU Classical Serif", 
                                     margin = margin(0,0,20,0), size = 14),
        axis.text = element_text(family = "DejaVu Serif"),
        plot.caption = element_text(family = "DejaVu Serif"),
        axis.title =  element_text(family = "DejaVu Serif", size = 9),
        strip.text.x = element_text(family = "DejaVu Serif", size = 5),
        legend.text = element_text(family = "DejaVu Serif"),
        legend.title = element_text(family = "DejaVu Serif"),
        axis.text.x = element_blank())

grafico_facetas<-educacion %>%
  #Hacemos un filtro para solo tomar el nivel superior
  filter(Nivel_educativo == "Superior") %>% 
  #agrupamos por nivel educativo, ciclo y se calcula el total de escuelas
  group_by(Entidad_federativa, ciclo) %>% summarise(Total = sum(Escuelas)) %>% 
  ggplot(aes(x = ciclo, y = Total, fill = ciclo)) +
  geom_bar(stat = "identity", color = "black", size = 0.1)+
  #se quita el eje vertical
  scale_y_continuous(breaks=NULL)+
  #Se hacen la sfacetas y se asignan 4 columas para el renderizado
  facet_wrap(~ Entidad_federativa , ncol = 8) +
  #se asigna una paleta de colores
  scale_fill_lapaz(discrete = TRUE)+
  geom_text(aes(label = Total, family = "DejaVu Serif"), size = 1.8, vjust = -0.2)+
  labs(x = "", y = "Total de escuelas",
       title = "Evolución de la cantidad de escuelas de educación superior",
       subtitle = "Evolución por Entidad Federativa del ciclo 2000-2001 al ciclo 2020-2021",
       caption = "Elaboracion propia con datos del INEGI
       disponible en: https://www.inegi.org.mx/temas/educacion//#Tabulados")+
  guides(fill=guide_legend(title="Ciclo Escolar", override.aes = list(size=0.2)))+
  theme_personalizado+
  #se elimina el fondo
  theme(panel.grid.major = element_blank())
print(grafico_facetas)

ggsave(grafico_facetas, filename  = "barras3.pdf", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

#################################################################
#                     Histograma y Densidad                     #
#################################################################

#Librerias utilizadas
library(tidyverse)
library(khroma)
library(ggExtra)

estratos <- read_csv("https://raw.githubusercontent.com/IranNash/visualizacionDeDatos/main/estratos.csv")

#Seleccionamos el municipio de Xalapa y eliminamos NA
xalapa <-estratos %>% filter(NOM_MUN == "Xalapa") %>% na.omit()
#Calculamos el número de intervalos del histograma
k = 1 + log(181, base = 2)

#Aplicamos los parámetros para la tipografía
theme_personalizado<-theme_minimal()+
  theme(legend.position = "top",
        plot.title = element_text(family = "DejaVu Serif", size = 17),
        plot.subtitle = element_text(family = "CMU Classical Serif", 
                                     margin = margin(0,0,20,0), size = 14),
        axis.text = element_text(family = "DejaVu Serif"),
        plot.caption = element_text(family = "DejaVu Serif"),
        legend.text = element_text(family = "DejaVu Serif"),
        legend.title = element_text(family = "DejaVu Serif"),
        axis.title =  element_text(family = "DejaVu Serif", size = 9))

#Graficaremos la variable GRAPROES
grafico_base <-ggplot(data = xalapa, mapping = aes(x = GRAPROES)) +
  #creamos un instrograma con el número de barras guardado en "k", color gray60
  geom_histogram(bins = round(k), col = "black", fill = "gray60") + 
  labs(x = "Promedio de años de Escolaridad",
       y = "Frecuencia",
       title = "Distribución de los años de escolaridad en Xalapa",
       subtitle = "Distribución por Área Geoestadística Básica",
       caption = "Elaboración propia con datos del
       censo de población y vivienda 2020")+
  theme_personalizado
print(grafico_base)

ggsave(grafico_base, filename  = "histograma1.pdf", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

#Calculamos ahora los años de escolaridad por cuartiles

xalapa <-xalapa %>% mutate(escolaridad = case_when(
  GRAPROES < 9 ~ "menos de 6",
  GRAPROES < 12 ~ "6 a 9",
  GRAPROES >=12 ~ "9 o más")) %>% 
  mutate(escolaridad = factor(escolaridad, 
        levels = c("menos de 6", "6 a 9", "9 o más"))) %>% na.omit() 

#Se va a mapear la variable VPH_INTER (viviendas sin internet) segmentadas por escolaridad.
grafico_base<-ggplot(xalapa, aes(x = VPH_INTER, fill = escolaridad))+
  #Se agrega la instrucción y = ..density.. para indicar que la frecuencia de y es la densidad.
  geom_density(aes(y = ..density..),col = "black", alpha = 0.7, size = 0.2, adjust = 1)+
  #agregamos la paletta de colores
  scale_fill_highcontrast()+
  labs(x = "Porcentaje de viviendas sin internet",
       y = "Densidad",
       title = "Distribución de las viviendas habitadas sin internet",
       subtitle = "Distribución por nivel de escolaridad.",
       caption = "Elaboración propia con datos del
       censo de población y vivienda 2020")+
  theme_personalizado +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(title="Promedio de años de escolaridad"))

print(grafico_base)
ggsave(grafico_base, filename  = "histograma2.pdf", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

#mapeamos un gráfico de dispersión de la población sin internet contra la población
#que esta inscrita al INSABI
grafico_base <-ggplot(xalapa, aes(x = VPH_INTER, y = PDER_SEGP, colour = escolaridad)) +
  #Mapeamos los puntos
  geom_point()+
  #agregamos la paleta de color
  scale_color_highcontrast()+
  labs(x = "Porcentaje de viviendas sin internet",
       y = "Porcentaje de la población afiliada al INSABI",
       title = "Relación en la población afiliada al INSABI y su acceso a internet",
       subtitle = "Distribución por nivel de escolaridad.",
       caption = "Elaboración propia con datos del
       censo de población y vivienda 2020")+
  theme_personalizado +
  guides(colour=guide_legend(title="Promedio de años de escolaridad", override.aes = list(size=3)))

#Agregamos el histográma sobre el eje "y". Confirmamos qel mapeo por grupos y que se asigna color.
grafico_marginal<-ggMarginal(grafico_base, groupColour = TRUE, groupFill = TRUE, 
           margins = "y", type = "histogram", alpha = 0.3)

print(grafico_marginal)

ggsave(grafico_marginal, filename  = "histograma3.pdf", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

#################################################################
#                     Gráfico de cajas y alambres               #
#################################################################

#Librerias utilizadas
library(tidyverse)
library(khroma)

#Obtención de la base de datos.
materna <- read_csv("https://raw.githubusercontent.com/IranNash/visualizacionDeDatos/main/materna.csv")
#Selección de las variables a utilizar
datos<-materna %>% 
  #Generación la variable nivel de desarrollo utilizando ifelse
  mutate(desarrollo = ifelse(idh < as.numeric(quantile(materna$idh, probs = 0.25, na.rm = TRUE)), "Bajo", 
                             ifelse(idh < as.numeric(quantile(materna$idh, probs = 0.75, na.rm = TRUE)), "Medio", "Alto"))) %>% 
  #Ordenamos el factor ingresos por niveles
  mutate(desarrollo = factor(desarrollo, levels = c("Alto","Medio","Bajo"))) %>% na.omit()

#Aplicamos los parámetros para la tipografía
thema_personalizado<-theme_minimal()+
  theme(legend.position = "top",
        plot.title = element_text(family = "DejaVu Serif", size = 17),
        plot.subtitle = element_text(family = "CMU Classical Serif", 
                                     margin = margin(0,0,20,0), size = 14),
        axis.text = element_text(family = "DejaVu Serif"),
        plot.caption = element_text(family = "DejaVu Serif", margin = margin(20,0,0,0)),
        axis.title =  element_text(family = "DejaVu Serif", size = 9))

grafico_base<-ggplot(datos, aes(x = desarrollo, y = adolescent)) +
  geom_boxplot()+
  labs(x = "Niveles de desarollo",
       y = "Tasa de natalidad adolescente",
       title = "Distribución de la tasa de natalidad adolescente entre 2015-2020",
       subtitle = "Distribución por niveles de desarrollo basado en IDH",
       caption = "Nota: La información refleja el cálculo de los indicadores entre año 2015 y 2020.
       Elaboración propia con datos del banco mundial, programa de las naciones unidas para
       el desarrollo y la base de datos de polı́ticas globales de aborto, OMS.")+
  thema_personalizado

print(grafico_base)

ggsave(grafico_base, filename  = "boxplot1.pdf", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

hist(datos$adolescent)
shapiro.test(datos$adolescent)
car::leveneTest(datos$adolescent, datos$desarrollo)
bartlett.test(datos$adolescent ~ datos$desarrollo)

kruskal.test(adolescent ~ desarrollo, datos)

#boxplt más histograma
datos <-datos %>% mutate(aborto = recode(aborto, "0" = "Sin aborto", "1" = "Con aborto"))
#Calculamos el numero de barras del histograma
k = 1 + log(nrow(datos), base = 2)

#agregamos tipografías de la leyenda
thema_personalizado <- thema_personalizado + 
  theme(legend.text = element_text(family = "DejaVu Serif"),
        legend.title = element_text(family = "DejaVu Serif"))

grafico_base<-ggplot(datos, aes(x = gini, fill = aborto)) +
  #Damos la scarcaterísticas deseabkes al histograma
  geom_histogram(bins = round(k), col = "black", fill = "gray60", alpha = 0.6)+
  #Definimos con width el ancho de la caja
  geom_boxplot(width = 20)+
  labs(x = "Coeficiente de Gini",
       y = "Frecuencia",
       title = "Distribución del coeficiente del Gini a nivel mundial",
       subtitle = "Comparativo de las politicas de aborto",
       caption = "Nota: La información refleja el cálculo de los indicadores entre año 2010-2017.
       Elaboración propia con datos del banco mundial, programa de las naciones unidas para
       el desarrollo y la base de datos de polı́ticas globales de aborto, OMS.")+
  scale_fill_vibrant()+
  thema_personalizado +
  #Agregamos la escala vertical manualmente para evitar negativos
  scale_y_continuous(breaks=seq(0, 30, 5)) +
  guides(fill=guide_legend(title="Políticas sobre el aborto")) 

print(grafico_base)

ggsave(grafico_base, filename  = "boxplot2.pdf", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

#################################################################
#                     Gráfico de lineas                         #
#################################################################

#Agregamos las librerías
library("tidyverse")
library("khroma")

#Cargamos la base de datos desde el repositorio de ourworldindata.org
covid = read.csv("https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true")

#Personalizamos las tipografías
theme_personalizado<-theme_minimal()+
  theme(plot.title = element_text(family = "DejaVu Serif", size = 17),
        plot.subtitle = element_text(family = "CMU Classical Serif", 
                                     margin = margin(0,0,20,0), size = 14),
        axis.text = element_text(family = "DejaVu Serif"),
        plot.caption = element_text(family = "DejaVu Serif"),
        legend.text = element_text(family = "DejaVu Serif"),
        legend.title = element_text(family = "DejaVu Serif"),
        axis.title =  element_text(family = "DejaVu Serif", size = 9))

#Damos formato de fecha
covid$date <- as.Date(covid$date,format = "%Y-%m-%d")

#Filtramos solo los páises pertenecientes a norteamérica a partir del 1 de enero del 2022
grafico_base <-covid %>% filter(location == "United States" | location == "Canada" | 
  location == "Mexico", date >= "2022-01-01") %>% 
  #seleccionamos solo la fecha, el país y las muertes por covid
  select(date, location, new_deaths_smoothed) %>% 
  ggplot(aes(x = date, y = new_deaths_smoothed, col = location)) + 
  geom_line() +
  labs(x = "",
       y = "Nuevas muertes diarias",
       title = "Muertes por Covid-19 en América del Norte ",
       subtitle = "Datos diarios reportados",
       caption = "Nota: Los datos son una aproximación suavisada de los datos reportados.
       Elaboración propia con datos de https://ourworldindata.org/coronavirus.")+
  scale_colour_highcontrast()+
  thema_personalizado +
  theme(legend.position = "right")+
  guides(colour=guide_legend(title="País",  override.aes = list(size=3))) +
  #agregamos una linea a partir del 1 de febrero del 2022
  geom_vline(xintercept = as.Date("2022-02-01"), linetype = 2)+
  #Agregamos un texto para generar una narrativa
  geom_text(aes(x = as.Date("2022-05-01"), y = 2000,
            label = "A partir del mes de febrero
Se observa una caída curva
de muertes por covid-19."),
            family = "DejaVu Serif", stat = "unique",
            size = 3.5, hjust = "center", colour = "black")+
  #Se agregan flechas
  annotate(geom = "curve", x = as.Date("2022-03-25"), y = 2000, 
           xend = as.Date("2022-02-05"), yend = 1800, 
           curvature = .3, arrow = arrow(length = unit(1, "mm")))
  
print(grafico_base)

ggsave(grafico_base, filename  = "lineas.pdf", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

#################################################################
#                           Heatmap                             #
#################################################################
#librerías
library("tidyverse")
library("khroma")

#Cargamos los datos de dengue
dengue <- read.csv("https://raw.githubusercontent.com/IranNash/visualizacionDeDatos/main/dengue.csv")

#Ordenamos el factor Mes para que aparezcan en orden en la visualización
dengue = dengue %>% mutate(Mes = factor(Mes, levels = c("enero", "febrero", "marzo", "abril",
                                                        "mayo", "junio", "julio", "agosto",
                                                        "septiembre", "octubre", "noviembre", 
                                                        "diciembre")))

grafico_base<-ggplot(dengue, aes(x = Año, y = Mes, fill = fhd_ng)) +
  #Con geom_tile creamos la geometría
  geom_tile()+
  #establecemos los años en el eje en una secuencia de 2010 al 2019
  scale_x_continuous(breaks = seq(2010, 2019, 1))+
  labs(x = "", y = "",
       title = "Casos mensuales de dengue no grave en Veracruz",
       subtitle = "Cáculo a partir de los boletines epidemiológicos semanales",
       caption = "Elaboracion propia con datos de los boletines epidemiológicos
       de la Dirección Nacional de Epidemiología disponible en:
       https://www.gob.mx/salud/acciones-y-programas/historico-boletin-epidemiologico") +
  theme_personalizado +
  guides(fill = guide_legend(title="Casos no graves",  override.aes = list(size=3)))+
  scale_fill_acton()
print(grafico_base)

ggsave(grafico_base, filename  = "calor.pdf", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")



#################################################################
#                              Cartograma                       #
#################################################################

library("sf", lib.loc="/usr/local/lib/R/site-library")  # manejo de informacion geografica 
#Alternativamente se calga simplemente como library(sf)
library(tidyverse)
library(khroma)

#Personalizamos las tipografías
theme_personalizado<-theme_minimal()+
  theme(plot.title = element_text(family = "DejaVu Serif", size = 17),
        plot.subtitle = element_text(family = "CMU Classical Serif", 
                                     margin = margin(0,0,20,0), size = 14),
        axis.text = element_text(family = "DejaVu Serif"),
        plot.caption = element_text(family = "DejaVu Serif"),
        legend.text = element_text(family = "DejaVu Serif"),
        legend.title = element_text(family = "DejaVu Serif"),
        axis.title =  element_text(family = "DejaVu Serif", size = 9))

#Cargamos el shapefile de información municipal de veracruz
ver<- st_read("~/Descargas/Marco Geoestadistico/30mun.shp") #Colocar la ruta donde se encuentra el archivo
#Cargamos los delitos en Veracruz en 2021
delitos <- read.csv("https://raw.githubusercontent.com/IranNash/visualizacionDeDatos/main/delitos.csv")
#Creamos la variable Total de delitos sumando los delitos mensuales
delitos$total = rowSums(delitos[,10:21])
#Calculamos el total de Robos para cada municipio en 2021
Robos <-delitos %>% filter(Tipo_de_delito == "Robo") %>% group_by(CVEMUN) %>% summarise(Total = sum(total))
#Cargamos la base de datos del censo de población para veracruz
censo <- read.csv("https://raw.githubusercontent.com/IranNash/visualizacionDeDatos/main/censo_ver.csv")
#Seleccionamos solo el indentificador del municipio y la población total
censo <- censo %>% select(POBTOT, CVEMUN)
#creamos la variable incidencia delictiva
Robos <- Robos %>% inner_join(censo, by = "CVEMUN") %>% mutate(incidencia = Total*1000/POBTOT)

#Unimos las bases de datos
muni<-sp::merge(ver,      #Shapefile
                Robos,             #Datos a combinar
                by.x="CVEGEO",   #Clave del shapefile
                by.y="CVEMUN",    #Clave de la base nueva
                duplicateGeoms=T,
                all.x=F)

#Cargamos la proyección
new = st_crs(4326)
#Asignamos proyección a la base muncipal nacional
muni = st_transform(muni, new)

grafico_base<-ggplot(muni) + 
  #Agregamos la geometría y asignamos incidencia como la variable a mapear
  geom_sf(aes(fill = incidencia), color = "black", size = 0.15)+
  #Paleta de colores
  scale_fill_YlOrBr()+
  labs(title = "Incidencia de Robos en el Estado de Veracruz 2021",
       subtitle = "Calculo de Robos por cada mil habitantes",
       caption = "Elaboración propia con datos del SESNSP y de
       Censo de Población y Vivienda 2020")+
  theme_personalizado +
  guides(colour = guide_legend(title="Incidencia de Robos"))

print(grafico_base)

ggsave(grafico_base, filename  = "mapa.pdf", device = cairo_pdf, 
       dpi = 300, 
       width = 8.58, height = 5.54, units = "in")

