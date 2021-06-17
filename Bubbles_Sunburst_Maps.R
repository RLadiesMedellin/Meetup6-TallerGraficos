# Librerias para tratamiento y lectura de datos 
library(dplyr)
library(tidyverse)
library(readxl)

# Fijar el directorio de trabajo 

# Leer las bases de datos a usar en gr?fico de burbujas
b3 <- read.csv("Incidentes_georreferenciados_2016.csv", encoding="UTF-8")
b8 <- read_excel("Datos_demograficos_2016.xlsx")


# Analizar las categorias de la variable COMUNA 
b3 %>%
  count(COMUNA) -> datos0

# Filtrar en COMUNA por valores diferentes a: "","0","AU","In"
# y calcular suma de incidentes viales por comunas
b3  %>%
  filter(!COMUNA %in% c("","0","AU","In")) %>%
  group_by(COMUNA) %>%
  summarise( incidentes = n())  -> datos1

# Unir datos1 y b8 por medio de la columna comuna
b8 %>% 
  left_join(datos1, by = c("comuna" = "COMUNA")) -> datos2

# Libreria para gr?fico de burbujas no interactivo
library(ggplot2)

# Gr?fico de burbujas no interactivo
datos2 %>%
  ggplot(aes(x=incidentes, y=tasa_bruta_mort,
             size = poblaci?n, 
             color = zona)) +
  geom_point() +
  labs(colour = "Zona", size = "Poblaci?n", 
       x= "Incidentes viales",
       y = "Tasa bruta de mortalidad")

# Libreria para gr?fico de burbujas interactivo
library(plotly)

# Gr?fico de burbujas interactivo

p <- datos2 %>%
  mutate(text = paste("Comuna: ", comuna,
                      "\nZona: ", zona,
                      "\nIncidentes: ",incidentes,
                      "\nPoblaci?n: ", poblaci?n,
                      "\nTasa bruta de mortalidad: ", tasa_bruta_mort, sep="")) %>%
  
  ggplot(aes(x=incidentes, y=tasa_bruta_mort,
             size = poblaci?n, 
             color = zona,
             text =text)) +
  geom_point() +
  labs(colour = "Zona", size = "Poblaci?n", 
       x= "Incidentes viales",
       y = "Tasa bruta de mortalidad") +
  theme(legend.position = "none")

pp <- ggplotly(p, tooltip="text")
pp

#### Leer base de datos de incidentes
b1 <- read.csv("Incidentes_georreferenciados_2014.csv", encoding="UTF-8")
b2 <- read.csv("Incidentes_georreferenciados_2015.csv", encoding="UTF-8")
b3 <- read.csv("Incidentes_georreferenciados_2016.csv", encoding="UTF-8")
b4 <- read.csv("Incidentes_georreferenciados_2017.csv", encoding="UTF-8")
b5 <- read.csv("Incidentes_georreferenciados_2018.csv", encoding="UTF-8")
b6 <- read.csv("Incidentes_georreferenciados_2019.csv", encoding="UTF-8")
b7 <- read.csv("Incidentes_georreferenciados_2020.csv", encoding="UTF-8")

# editar b6 (2019)
b6 <- b6[-25]

# agregar una nueva columna de a?o
b1 <- b1 %>% 
  mutate(ano = "2014")

b2 <- b2 %>% 
  mutate(ano = "2015")


b3 <- b3 %>% 
  mutate(ano = "2016")


b4 <- b4 %>% 
  mutate(ano = "2017")


b5 <- b5 %>% 
  mutate(ano = "2018")


b6 <- b6 %>% 
  mutate(ano = "2019")


b7 <- b7 %>% 
  mutate(ano = "2017")


# juntando la base de datos 
datos <- rbind(b1, b2, b3, b4, b5, b6, b7)

library(sunburstR) 

# analizando jerarquias
datos %>% 
  count(GRAVEDAD)

# renombrando para GRAVEDAD
datos <- datos %>% 
  mutate(
    GRAVEDAD = case_when(
      GRAVEDAD %in% c("CON HERIDOS", "HERIDO") ~ "HERIDOS",
      GRAVEDAD %in% c("CON MUERTOS", "MUERTO") ~ "MUERTOS",
      GRAVEDAD %in% c("SOLO DAÑOS") ~ "SOLO DAÑOS"
    )
  )

# analizando jerarquias
datos %>% 
  count(CLASE)

datos <- datos %>% 
  mutate(
    CLASE = case_when(
      CLASE %in% c("Caida de Ocupante", "Caída de Ocupante",
                   "Caida Ocupante"," Caída Ocupante") ~ "CAIDA OCUPANTE",
      CLASE %in% c("Choque", "Choque", "Choque y Atropello") ~ "CHOQUE",
      CLASE %in% "Volcamiento" ~ "VOLCAMIENTO",
      CLASE %in% "Atropello" ~ "ATROPELLO",
      CLASE %in% "Incendio" ~ "INCENDIO",
      CLASE %in% "Otro" ~ "OTRO"
    )
  )


# sunburst 1
sun <- datos %>% 
  select(GRAVEDAD, CLASE) %>% # variables con las que se desea trabajar
  mutate(path = paste(GRAVEDAD, CLASE,   sep = "-")) %>% # crear jerarquia
  slice(2:306228) %>% # indexar las filas
  mutate(
    V = 1 # crear una nueva variable llena de unos 
  )

# para hacer tabla de frecuencias
data.frame(xtabs(V~path, sun))

# sunburst
sunburst(data = data.frame(xtabs(V~path, sun)), legend = FALSE)



#################### MAPAS

library(sf) ### Trabajar Shapefile
library(mapview) ###ver mapas
library(sp)

####1) Cargar el Shapefile
####2) Construir mapa medellin como pol?gono
####3) Conteo de incidentes por comuna 
####4) Generar mapa por defecto
####5) Mejorar la visualizaci?n
####6) Incluir el nombre de la comunas al mapa

####1) Cargar el Shapefile
file.exists(('LimiteComunaCorregimiento_2014.shp'))
med <- st_read('LimiteComunaCorregimiento_2014.shp')

####2) Construir mapa medellin como pol?gono

med %>% ggplot() +
  geom_sf()

####3) Conteo de incidentes por comuna 
COMUNA <- datos %>% 
  group_by(COMUNA) %>%
  summarise(incidentes = n())

COMUNA

####juntamos la informaci?n del shapefile y la de incidentes
mapas <- med %>% left_join(COMUNA, by = c("NOMBRE" = "COMUNA"))

####4) Generar mapa por defecto
m <- mapas %>% 
  ggplot(aes(fill = incidentes)) +
  geom_sf()
m

####5) Mejorar la visualizaci?n
m +
  ####cambiar el grosor de la l?nea 
  geom_sf(colours ="gray", size =0.1) +
  ###agregar t?tulos
  labs(title = "Incidentes de transito en Medell?n",
       subtitle= "Por comunas") +
  ##### cambiar el color de relleno
  scale_fill_gradient(low = "floralwhite", high = "Red", na.value = NA) +
  ###Retirar fondo gris
  theme_bw()

####6) Incluir el nombre de la comunas al mapa

st_centroid(med)
coor<-st_coordinates(st_centroid(med))

tabla<-as_tibble(cbind(as.data.frame(coor), COMUNA = Med$NOMBRE))


Incidentes <- mapas %>% 
    add_column(tabla)
  
Incidentes %>%
  ggplot(aes(fill = incidentes))+
  ####cambiar el grosor de la l?nea 
  geom_sf(colours ="gray", size =0.1) +
  ###agregar t?tulos
  ##### cambiar el color de relleno
  scale_fill_gradient(low = "floralwhite", high = "Red", na.value = NA) +
  geom_text(aes(x=X,y = Y,label=NOMBRE)) +
  labs(title = "Incidentes de transito en Medell?n",
       subtitle= "Por comunas") +
    ###Retirar fondo gris
  theme_bw()

