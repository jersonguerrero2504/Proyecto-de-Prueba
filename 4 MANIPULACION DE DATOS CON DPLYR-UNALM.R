#----------------------------------#
#  MANIPULACIÓN DE DATOS CON DPLYR #
#     Mg. Jesús Salinas Flores     #
#     jsalinas@lamolina.edu.pe     #
#----------------------------------#

rm(list = ls())
graphics.off()
cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

options(digits = 3)
options(scipen = 999)

# Paquetes
library(pacman)
p_load(dplyr, ggplot2, datos, tidyverse)

tidyverse_packages()

#---------------------------#
# Caso de Estudio 1: paises #
#---------------------------#

#-----------------------------------------#
#### I. Trabajando con todos los datos ####
#-----------------------------------------#

# 1.1 Describiendo los datos -----------------------------------
library(datos)

data(package = "datos")

paises

View(paises)

str(paises)

# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	1704 obs. of  6 variables:
# $ pais             : Factor w/ 142 levels "Afganistán","Albania",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ continente       : Factor w/ 5 levels "África","Américas",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ anio             : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
# $ esperanza_de_vida: num  28.8 30.3 32 34 36.1 ...
# $ poblacion        : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
# $ pib_per_capita   : num  779 821 853 836 740 ...

paises2 <- as.data.frame(paises)
# Nota.- En Excel *.xls *.xlsx leídos como tibble

str(paises2)
paises2


# Comparación
# data.frame es histórico y flexible
# tibble es moderno, predecible y pedagógicamente superior
# 👉 Para enseñanza, análisis exploratorio y ciencia de datos:
#    se recomienda tibble
# 👉 Para compatibilidad con código antiguo:
#    se recomienda data.frame


#----------------------------------------#
#### II. Data wrangling              #####
#        Transformación y mapeo de datos #
#----------------------------------------#

attach(paises)

abs(round(log(sqrt(0.5)), 2))

x <- sqrt(0.5)
y <- log(x)
z <- round(y, 2)
w <- abs(z)
w

0.5 %>% sqrt() %>% log() %>% round(2) %>% abs()

0.5 %>% sqrt %>% log %>% round(2) %>% abs

sqrt(0.5) %>% log %>% round(2) %>% abs


0.5 %>% sqrt %>% round %>% 2 %>% abs

attach(paises)

round(prop.table(table(continente)), 3) * 100
continente

library(dplyr)
continente %>% table() %>% prop.table()*100 %>% round(3)
continente %>% table %>% prop.table %>% round %>% 3 # Es un error

#________________________
# ________ \\|// ________
# ________( o o ) _______
# ___oo0____(_)____Ooo___
#      Ejercicio   1     #
# Obtenga la esperanza de vida promedio con 2 decimales



# Funciones en dplyr
# dplyr cuenta con 5 grupos diferentes de funciones: 
# - Funciones de selección/filtro
# - Funciones de manipulación 
# - Funciones de agrupación
# - Funciones de resumen
# - Funciones de combinación

# 2.1 Funciones de Selección / Filtro --------------------------

# Filtrando registros con filter() 
filter(paises, anio == 1957) 

filter(paises, anio == 1957) -> b   # 1
b <- filter(paises, anio == 1957)   # 2
b = filter(paises, anio == 1957)    # 3
filter(paises, anio == 1957) =  b   # 4
filter(paises, anio == 1957) <- b   # 5

a = 5
a <- 5
5 -> a
5 = a
5 == a

str(b)
write.csv(b, "paises1957.csv")

filter(paises, anio == 1957)

# Usando pipe
paises %>% filter(anio == 1957)

# Filtrando para un país y un año 
paises %>% filter(pais == "China", anio == 2002)   # & y

paises %>% filter(pais == "China" & anio == 2002)

paises %>% filter(pais == "Chile" | pais == "Perú")  # | or

paises %>% filter(pais == "Chile" | pais == "Perú")  %>% View

paises %>% filter(pais == "Chile" & pais == "Perú")  # ¿? error

paises %>% filter(pais == "Perú", anio == 1997 |
                    anio == 2002 | anio == 2007)

paises %>% filter(pais == "Perú", 
                  anio %in% c(1997, 2002, 2007, 2026))

paises %>% filter(pais == "Perú")

# Ordenando observaciones con arrange()
paises %>% arrange(esperanza_de_vida) %>% 
           View # por defecto ascendente, de - a +

paises %>% arrange(desc(esperanza_de_vida)) # De + a -

paises %>% arrange(-esperanza_de_vida) # equivalente al anterior

# Filtrando y ordenando 
paises %>% filter(anio == 1957) %>% arrange(poblacion)
paises %>% filter(anio == 1957) %>% arrange(-poblacion)


#________________________
# ________ \\|// ________
# ________( o o ) _______
# ___oo0____(_)____Ooo___
#      Ejercicio   2     #
# Muestre la información de Asia del año 1987 ordenada
# según el PIB
# En Asia en el año 1987, ¿qué país tenía el mayor PBI?


# Seleccionando columnas con select() 
paises[, c(1, 3, 5)]
paises[, c("pais", "anio", "poblacion")]

paises %>% select(pais, anio, poblacion)

paises %>% select(anio, pais, poblacion)

paises %>% select(pais, esperanza_de_vida, everything())

paises %>% select(starts_with("p"))

paises %>% select(contains("p"))

paises %>% select_if(is_double)

paises %>% select_if(is.factor)

# Retener registros de una determinada posición con slice()
paises %>% filter(anio == 2007, continente == "Europa") %>%
  arrange(-esperanza_de_vida)

paises %>% filter(anio == 2007, continente == "Europa") %>%
  arrange(-esperanza_de_vida) %>% slice(1:4)

paises %>% slice_sample(n = 10)

dim(paises)
paises %>% slice_sample(prop = 0.10) 

paises %>% slice_sample(prop = 0.10) %>% count()


# 2.2 Funciones de manipulación --------------------------------

# Usando mutate() y transmute() para cambiar o crear una columna 
paises %>% 
  mutate(esperanza_de_vida = 12 * esperanza_de_vida) %>% View

paises %>% 
  mutate(esperanza_de_vida_meses = 12 * esperanza_de_vida) %>%
  View

paises %>% 
  mutate(esperanza_de_vida = 12 * esperanza_de_vida) %>% View

paises %>% 
  transmute(esperanza_de_vida = 12 * esperanza_de_vida) %>% View

# Renombrando columnas con colnames() 
paises3 <- paises
colnames(paises3)
colnames(paises3)[1] <- "Pais"

paises2 <- paises %>% rename(Pais = pais, Año = anio,
                             PBI = pib_per_capita)

paises2


#-----------------------------------#
#### III. Visualización de datos ####
#-----------------------------------#

# Comparando población y PBI per cápita 
paises_1952 <- paises %>% filter(anio == 1952)

paises_1952

ggplot(paises_1952) + aes(x = poblacion, y = pib_per_capita) +
  geom_point() + 
  labs(title = "PBI vs poblacion en 1952")

# ¿Se puede juntar líneas anteriores en un sólo código?
paises %>% filter(anio == 1952) %>%
ggplot() + aes(x = poblacion, y = pib_per_capita) +
  geom_point() + 
  labs(title = "PBI vs poblacion en 1952")

# Cambiando los ejes X e Y a una escala logarítmica 
# Usando escala logarítmica
# log10(10)=1;log10(100)=2; log10(1000)=3; log10(10000)=4
# Opción 1
paises %>% filter(anio == 1952) %>%
  ggplot() + aes(x = poblacion, y = pib_per_capita) +
  geom_point() + scale_x_log10() + scale_y_log10()

# Opción 2
paises %>% filter(anio == 1952) %>%
  ggplot() + aes(x = log10(poblacion), y = log10(pib_per_capita)) +
  geom_point()

# Presente un gráfico animado que muestre la evolución de la
# esperanza de vida en la India (P)
paises %>% filter(pais == "India") %>%
  ggplot()  + aes(x = anio, y = esperanza_de_vida) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(1952,2007,5)) +
  transition_reveal(anio)

# Elaborar un gráfico animado donde se vea como ha ido
# el PBI en Perú a través de los años




# Añadiendo color al gráfico de dispersión 
paises %>% filter(anio == 1952) %>%
  ggplot() + aes(x = poblacion, y = esperanza_de_vida,
                 color = continente) +
  geom_point() + scale_x_log10()

# Creando un subgráfico para cada continente 
paises %>% filter(anio == 1952) %>%
  ggplot() + aes(x = poblacion, y = esperanza_de_vida) +
  geom_point() +
  scale_x_log10() + facet_wrap(~ continente)

# ¿Cómo modificas el código anterior para que los colores
# cambien según el continente?


#________________________
# ________ \\|// ________
# ________( o o ) _______
# ___oo0____(_)____Ooo___
#      Ejercicio   3     #
# Presente un gráfico que compare la población y
# PBI per cápita en América para cada año



# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   4     #
# Presente un gráfico que compare el pbi y la esperanza de vida
# por continente, color diferente por país y el tamaño del
# punto cambie según la población



# PIB per cápita vs. esperanza vida para 2007 para países que 
# empiezan con P en América
library(datos)
library(ggrepel)
library(stringr)
paises %>% filter(anio == 2007, continente == "Américas",
                  str_detect(pais, "^P")) %>%
  ggplot() + aes(x = pib_per_capita, 
                 y = esperanza_de_vida, 
                 label = pais) + 
  geom_point(aes(size = poblacion)) +
  labs(title = 'Países de América con "P", 2007') +
  theme_classic() -> r1
r1

r1 + geom_text()
r1 + geom_text_repel()
r1 + geom_label()
r1 + geom_label_repel()

# Diferentes colores
r1 + geom_text_repel(aes(col = pais))
r1 + geom_label_repel(aes(fill = pais), col = "blue") 
r1 + geom_label_repel(aes(fill = pais), col = "blue") +
     guides(fill = "none")

# guides() te permite decidir qué guía mostrar y cómo para cada
# estética, de forma selectiva.


# Gráfico Animado
# Color diferente por país
# El tamaño del punto cambia según la población
library(gganimate)
ggplot(paises) + aes(x = pib_per_capita, 
                     y = esperanza_de_vida,
                     color = pais, size = poblacion) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() + 
  facet_wrap( ~ continente, nrow = 3) +  # vars(continente)
  # Animando el gráfico:
  labs(title = 'Año: {frame_time}',  # Interpolación
       x = 'PBI per cápita',
       y = 'Esperanza de vida') +
  transition_time(anio)   



# 2.3 Funciones de agrupación y resumen ------------------------

paises %>% mean(esperanza_de_vida)  # ¿?

# Resumiendo con summarize()
paises %>% summarize(mean(esperanza_de_vida))

mean(paises$esperanza_de_vida) 

# Añadiendo una etiqueta
paises %>% 
  summarize(esperanza_de_vida_media = mean(esperanza_de_vida))

# Resumiendo la esperanza de vida en 1957 
paises %>% filter(anio == 1957) %>%
  summarize(esperanza_de_vida_media = mean(esperanza_de_vida))

# Resumiendo con varios indicadores en 1952 
paises %>% filter(anio == 1952) %>%
  summarize(esperanza_de_vida_media = mean(esperanza_de_vida),
    pib_per_capita_maximo = max(pib_per_capita))

# Resumiendo por año con group_by() 
paises %>% group_by(anio) %>%
  summarize(esperanza_de_vida_media = mean(esperanza_de_vida))

paises %>% group_by(anio) %>%
  summarize(esperanza_de_vida_media = mean(esperanza_de_vida)) %>%
  ggplot() + aes(x = anio, y = esperanza_de_vida_media) + 
  geom_point()  + geom_line() +
  scale_x_continuous(breaks = seq(1952, 2007, 5))

# # # Agrupando por continente y por año
paises %>% group_by(continente, anio) %>%
  summarize(edv = mean(esperanza_de_vida)) %>% View

paises %>% group_by(continente, anio) %>%
  summarize(edv = mean(esperanza_de_vida)) %>%
  ggplot() + 
  aes(x = anio, y = edv, color = continente) + 
  geom_line() +
  facet_wrap(~ continente, scales = "fixed") + 
  theme(legend.position = "none") +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = seq(1952, 2007, 5))

paises %>% group_by(continente, anio) %>%
  summarize(edv = mean(esperanza_de_vida)) %>%
  ggplot() + 
  aes(x = anio, y = edv, color = continente) + 
  geom_line() +
  facet_wrap(~ continente, scales = "free") + 
  theme(legend.position = "none") +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = seq(1952, 2007, 5))



# Visualizando la esperanza de vida media 
paises %>%
  group_by(anio) %>%
  summarize(esperanza_de_vida_media = mean(esperanza_de_vida)) %>%
  ggplot() + 
  aes(x = anio, y = esperanza_de_vida_media) +
  geom_point() + geom_line() + 
  expand_limits(y = 0) +
  scale_y_continuous(breaks = seq(0, 70, 10)) + theme_bw()

# + expand_limits(y = 0)
# ylim(0,70)



# Comparando la esperanza de vida media y el PBI medio 
# por continente en 2007
paises %>% filter(anio == 2007) %>% group_by(continente) %>%
  summarize(esperanza_de_vida_media = mean(esperanza_de_vida),
    pib_per_capita_medio = mean(pib_per_capita)) %>%
  ggplot() + aes(x = pib_per_capita_medio, 
                 y = esperanza_de_vida_media,
                 color = continente) + geom_point()

# Contando registros con count() y summarise(n())
paises %>% filter(anio == 2007) %>%
           group_by(continente) %>% count()

paises %>% filter(anio == 2007) %>%
  group_by(continente) %>% summarize(n())


paises %>% filter(anio == 2007) %>%
  group_by(continente) %>% summarize(Número_Países = n())

# Número de países por continente en 2007 
paises %>% filter(anio == 2007) %>%
  group_by(continente) %>% summarize(n = n()) %>%
  ggplot() + aes(x = continente, y = n) + 
  geom_bar(stat = "identity")  # stat = "count"

paises %>% filter(anio == 2007) %>%
  group_by(continente) %>% summarize(n = n()) %>%
  ggplot() + aes(x = continente, y = n) + 
  geom_col() # stat = "identity"


paises %>%  group_by(continente) %>%
  summarize(N_Registros = n(),
            N_Paises    = n_distinct(pais),
            N_Años      = n_distinct(anio))


# Resumiendo por grupo sin group_by()
paises %>% filter(anio == 2007) %>%
  group_by(continente) %>% summarize(n())

paises %>%  filter(anio == 2007) %>%
  summarize(total = n(), .by = c(continente))

paises %>%  group_by(continente, anio) %>%
  summarize(Promedio = mean(pib_per_capita)) %>% View()

paises %>%  
  summarise(Promedio = mean(pib_per_capita),
            .by = c(continente, anio)) %>% View()


# Sumando con la función summarize(sum())
paises %>%
  filter(anio == 2007) %>%
  summarise(poblacion_total = sum(poblacion))


paises %>%
  filter(anio == 2007) %>%
  group_by(continente) %>%
  summarise(poblacion_total = sum(poblacion))


# Paquete más descargado
# devtools::install_github("metacran/cranlogs")
library(cranlogs)
descarga.dia <- cran_downloads(packages = c("gganimate", "ggrepel", "patchwork"), from = "2020-01-01")
head(descarga.dia)
descarga.dia %>% 
  group_by(package) %>% 
  summarise(descarga = sum(count)) %>%
  arrange(desc(descarga)) %>%
  mutate(Porcentaje = round( descarga / sum(descarga) * 100, 1))





#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   5     #
# Presente un gráfico que muestre el número de países en 2007
# por continente. 




# Añadiendo etiquetas al gráfico
paises %>% filter(anio == 2007) %>% 
  group_by(continente) %>% summarise(Número_Paises = n()) %>%
  ggplot() + 
  aes(x = reorder(continente, -Número_Paises), 
      y = Número_Paises, 
      fill = Número_Paises > 30,    # Color con condición
      label = Número_Paises) +      # se activa con geom_text
  geom_col(show.legend = F)  +      # geom_bar(stat="identity") +
  ylim(0, 60) +
  geom_text(vjust = -1, col = "blue", size = 4) 
  #geom_text(aes(label = paste(round(100*Número_Paises/sum(Número_Paises),0),"%")),vjust=-1.0) 


# Gráfico de barras interactivo con ggiraph
# Fuente:
# How to Make Any ggplot Interactive With {ggiraph} | 
# Step-By-Step Tutorial
# Albert Rapp
# browseURL("https://www.youtube.com/watch?v=ZyjwF3FMjFE")
library(ggiraph)
paises %>% filter(anio == 2007) %>% 
  group_by(continente) %>% summarise(Número_Paises = n()) %>%
  ggplot() + 
  aes(x = continente, y = Número_Paises, 
      tooltip = Número_Paises, data_id = continente) +  
  geom_col_interactive(show.legend = F)  +    
  ylim(0, 60)  -> geo_col_gg
girafe(ggobj = geo_col_gg)


paises %>% group_by(continente, anio) %>%
  summarize(edv = mean(esperanza_de_vida)) %>%
  ggplot() + aes(x = anio, y = edv,
                 col = continente) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) 


paises %>% group_by(continente, anio) %>%
  summarize(edv = mean(esperanza_de_vida)) %>%
  ggplot() + aes(x = anio, y = edv,
                 col = continente) +
  geom_line(linewidth = 1.5) +
  geom_point_interactive(aes(tooltip = edv), 
                         size = 3) -> gg_obj

girafe(ggobj = gg_obj)

girafe(ggobj = gg_obj,
       options = list(
         opts_tooltip(
           css = htmltools::css(background = "white",
                                border = "2px solid black;",
                                padding = "10px",
                                font_weight = 600,
                                font_size = "12pt"))))

paises %>% group_by(continente, anio) %>%
  summarize(edv = mean(esperanza_de_vida)) %>%
  ggplot() + aes(x = anio, y = edv,
                 col = continente) +
  geom_line_interactive(aes(data_id = continente),
                        linewidth = 1.5) +
  geom_point_interactive(aes(tooltip = edv, 
                             data_id = continente),
                         size = 3) -> gg_obj

girafe(ggobj = gg_obj)

girafe(ggobj = gg_obj,
       options = list(
         opts_hover(css = ""),
         opts_hover_inv(css = "opacity:0.2;"),
         opts_tooltip(
           css = htmltools::css(background = "white",
                                border = "2px solid black;",
                                padding = "10px",
                                font_weight = 600,
                                font_size = "12pt"))))


# Desagrupando con ungroup()
# Después de utilizar group_by() y summarise() se recomienda usar
# la función ungroup().

# Ejemplo: Calcular el promedio de esperanza de vida (2007)
#          después de filtrar países cuya población esté por 
#          encima del promedio por continente, 
#          pero el promedio final debe ser global.

# Opción 1: Sin ungroup()
paises %>% filter(anio == 2007) %>%
           group_by(continente) %>%
           filter(poblacion > mean(poblacion)) %>%   # promedio por continente
           summarise(esperanza_media = mean(esperanza_de_vida))


# Opción 2: Con ungroup()
paises %>% filter(anio == 2007) %>%
           group_by(continente) %>%
           filter(poblacion > mean(poblacion)) %>%   # promedio por continente
           ungroup() %>%                             # 👈 CLAVE
           summarise(esperanza_media = mean(esperanza_de_vida))


# Combinando group_by() con slice_max()
paises %>% filter(anio == 2007) %>% group_by(continente) %>%
  slice_max(pib_per_capita)


# Identificando datos duplicados 
str(iris)

iris %>%
  filter(duplicated(.))

iris %>%
  filter(duplicated(.) | duplicated(., fromLast = TRUE))


iris %>% group_by_all() %>% filter(n() > 1)


# Eliminando datos duplicados con distint()
iris %>% distinct() %>% dim()


#---------------------------------------------------------#
#### IV. Control del flujo de procesamiento (magrittr) ####
#---------------------------------------------------------#

# Usos del paquete ‘magrittr’
# browseURL("https://rpubs.com/medusa/575094")

# Operador %<>% -----------------------------------------------
# El operador permite añadir el resultado de la operación que 
# se va a calcular al valor o valores que están a la izquierda.
# Se usa cada vez que queramos emplear <-
# Ejemplo 1:
paises2 <- paises
paises %>% filter(anio == 2007, pib_per_capita > 35000)
paises3 <- paises %>% filter(anio == 2007, pib_per_capita > 35000)
paises3

library(magrittr)
paises2 %<>% filter(anio == 2007, pib_per_capita > 35000)
paises2


# Operador %$% ------------------------------------------------
# Si estás trabajando con funciones que no tienen una API basada
# en data frames (esto es, pasas vectores individuales, no un 
# data frame o expresiones que serán evaluadas en el contexto de
# un data frame), puedes encontrar %$% útil. 
# Este operador “explota” las variables en un dataframe para que
# te puedas referir a ellas de manera explícita. Esto es útil 
# cuando se trabaja con muchas funciones en R base:
View(iris)

iris %>%
  filter(Sepal.Length > mean(Sepal.Length)) %>%  # Incorrecto
  mean(Sepal.Width)


iris %>%
  filter(Sepal.Length > mean(Sepal.Length)) %$%
  mean(Sepal.Width)


paises %>% mean(poblacion)  # No es correcto

paises %>% summarize(mean(poblacion)) # Si es correcto

paises$poblacion %>% mean() # Si es correcto

paises %$% mean(poblacion)  # Si es correcto


#-------------------------#
#### V. Otros gráficos ####
#-------------------------#

library(foreign)
datos <- read.spss("Riesgo_morosidad.sav", 
                   use.value.labels = T,  
                   to.data.frame = TRUE)

attach(datos)

# 5.1 Gráfico de Cascada ---------------------------------------
library(waterfalls)
datos %>%
  group_by(dpto) %>%
  summarise(Frec = n()) %>%
  arrange(-Frec) %>% 
  mutate(
    Porc = round( Frec/ sum(Frec) * 100, 1)) %>% 
  select(dpto, Porc) %>% # Es obligatorio seleccionar
  waterfall(values = Porc, calc_total = T) +
  labs(title = "% de clientes por departamento",
       y = "", x = "") +
  theme_minimal()

datos %>%
  group_by(dpto) %>%
  summarise( Frec = n()) %>%
  arrange(-Frec) %>% 
  select(dpto, Frec) %>% # Es obligatorio seleccionar
  waterfall(values = Frec , calc_total = TRUE) +
  labs(title = "Número de clientes por departamento",
       y = "", x = "")  + theme_minimal()

# 5.2 Gráfico TreeMap ------------------------------------------
library(treemapify) # TreeMap
datos %>%
  count(dpto) %>%
  ggplot() + aes(fill = dpto, area = n) +
  geom_treemap() +
  geom_treemap_text(aes(label = dpto), 
                    place = "centre", 
                    grow = F, 
                    size = 14) +
  theme(legend.position = "none") +
  labs(title = "Distribución de clientes por departamento")

datos %>%
  group_by(dpto) %>%
  summarise( Frec = n()) %>%
  arrange(-Frec) %>% 
  mutate(
    Porc = round( Frec/ sum(Frec) * 100, 1) ,
    Label =  paste0(dpto, "\n ", Porc, "%") ) %>% 
  ggplot(aes(fill = dpto, area = Porc)) +
  geom_treemap() +
  geom_treemap_text(aes(label = Label), 
                    place = "centre", 
                    grow = F, 
                    size = 13) +
  theme(legend.position = "none") +
  labs(title = "Distribución de clientes por departamento")


# 5.3 Gráfico de Segmento --------------------------------------
datos1 <- datos %>% group_by(dpto) %>% summarise(n = n())
datos1

ggplot(datos1) + aes(dpto, n, color = dpto,
                     label = n) +
  geom_point(stat = "identity", size = 16) + coord_flip() +
  theme(legend.position = "none") +
  geom_segment(aes(x = dpto, xend = dpto, y = 0, yend = n)) + 
  geom_text(color = "black", size = 3) +
  labs(title = "Número de clientes por departamento") 

# 5.4 Gráfico de Waffle ----------------------------------------
library(waffle) 
datos %>% count(dpto) %>%
  ggplot() + aes(fill = dpto, values = n) + 
  geom_waffle(make_proportional = T)


datos %>%
  count(dpto) %>%
  ggplot() + aes(fill = dpto, values = n) +
  geom_waffle(make_proportional = T,
              n_rows = 10,
              size = 1.0,
              color = "white") +
  scale_fill_brewer(palette = "Spectral") +
  coord_equal() +
  theme_minimal() +
  theme_enhance_waffle() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  labs(title = "Distribución de clientes por departamento")


# 5.5 Gráficos basados en íconos -------------------------------
# ggpop es un paquete de R construido sobre ggplot2 que 
# simplifica la creación de gráficos de población basados en 
# íconos. Al combinar funcionalidades de ggplot2 y ggimage, 
# ggpop permite a los usuarios visualizar datos de población 
# utilizando íconos personalizables organizados en disposiciones 
# circulares. Diseñado principalmente para la narrativa visual, 
# ggpop ayuda a los usuarios a comunicar estadísticas de 
# población de manera atractiva.

library(dplyr)
library(ggpop)

df_sexo <- datos %>% group_by(sexo) %>% count()
df_sexo

df_sexo_prop <- process_data(data = df_sexo, 
                             group_var = sexo, 
                             sum_var = n, 
                             sample_size = 1000)

# browseURL("https://fontawesome.com/icons")

df_sexo_prop <- df_sexo_prop %>%
  mutate(icon = case_when(
    type == "Masculino" ~ "person",
    type == "Femenino"  ~ "person-dress"))

library(ggplot2)
ggplot(df_sexo_prop) + aes(icon = icon, color = type) +
  geom_pop(size = 1, arrange = T, legend_icons = T) +
  theme_void() +
  theme(legend.position = "bottom")

ggplot(df_sexo_prop) +
  aes(icon = icon, color = type) +
  geom_pop(size = 1, arrange = TRUE) +
  theme_void(base_size = 20) +
  theme(legend.position = "bottom") +
  labs(title = "Distribución de Clientes por Sexo") +
  theme(legend.title = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(color = "#D4AF37"),
        plot.title = element_text(color = "#D4AF37", hjust = 0.5)) +
  scale_legend_icon(size = 10) +
  scale_color_manual(values = c("Masculino" = "#1E88E5",
                                "Femenino" = "#D81B60"),
                     labels = c("Femenino" = "Femenino: 45%", 
                                "Masculino" = "Masculino: 55%"))

# Mayor información en la web:
# browseURL("https://jurjoroa.github.io/ggpop/")


#----------------------------------------#
#### VI. Datos relacionales con dplyr ####
#----------------------------------------#

library(datos)

data(package = "datos")
? vuelos

# vuelos contiene todos los vuelos 336,776 que partieron
#        de la ciudad de Nueva York durante el 2013.
#        Los datos provienen del Departamento de Estadísticas
#        de Transporte de los Estados Unidos
vuelos

View(vuelos)
# aerolineas permite observar el nombre completo de la
#            aerolínea a partir de su código abreviado:
aerolineas

View(aerolineas)
# aeropuertos entrega información de cada aeropuerto,
#             identificado por su código:

aeropuertos
View(aeropuertos)

# aviones entrega información de cada avión, identificado
#         por su codigo_cola:
aviones
View(aviones)

# clima entrega información del clima en cada aeropuerto
#       de Nueva York para cada hora:
clima
View(clima)

# En estos datos:

# - vuelos se conecta con aviones a través de la
#   variable codigo_cola.
# - vuelos se conecta con aerolineas a través de la
#   variable codigo_carrier.
# - vuelos se conecta con aeropuertos de dos formas:
#   a través de las variables origen y destino.
# - vuelos se conecta con clima a través de las variables
#   origen (la ubicación), anio, mes, dia y hora (el horario).

vuelos2 <- vuelos %>%
  select(anio:dia, hora, origen, destino, codigo_cola, aerolinea)
vuelos2



View(aerolineas)


vuelos2 %>%
  left_join(aerolineas, by = "aerolinea")

# Definiendo las columnas clave
View(clima)

vuelos2 %>%
  left_join(clima)

View(aviones)

vuelos2 %>%
  left_join(aviones, by = "codigo_cola")  # Hay que especificar el campo en común, porque hay otros como anio


View(vuelos2)

View(aeropuertos)  #  Advertencia!!!

vuelos2 %>%
  left_join(aeropuertos, by = c("origen" = "codigo_aeropuerto")) %>%
  View()

vuelos2 %>%
  left_join(aeropuertos, c("destino" = "codigo_aeropuerto"))
