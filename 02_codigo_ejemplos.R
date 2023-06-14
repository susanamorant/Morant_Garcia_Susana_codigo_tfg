# Este código es totalmente reproducible y permite reproducir los ejemplos llevados a cabo para el marco teórico.

# Cargamos librerías necesarias
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(ggmap)
library("INLA")
library(rgdal)
library(sp)
library(lattice)
library(spdep)
library(gridExtra)

####################################
# MAPA ÁREAS LLANAS ISLAS BALEARES #
####################################

# Cargamos los datos referidos a los límites municipales españoles. A partir de ellos, se filtrán las filas correspondientes a las Islas Baleares. De este modo, ya tenemos el mapa base sobre el cual podremos representar la información. Estos datos también han sido obtenidos del INE.
# Descargamos, descomprimimos y cargamos los datos
temp <- tempfile()
url <- "https://www.ine.es/prodyser/cartografia/seccionado_2019.zip"
download.file(url, temp)
unzip(temp, exdir = "scc19")
mapa <- st_read("scc19/SECC_CE_20190101.shp")
unlink(temp)

# Filtramos solo la provincia de las Islas Baleares
Baleares <- mapa %>%
  filter(NPRO == "Balears, Illes") 

# Representamos de forma gráfica el mapa
lattice <- ggplot() +
  geom_sf(data = Baleares)+
  theme_minimal()+
  theme(axis.text=element_blank(),
        panel.grid = element_blank())

# Guardamos la representación en formato .png
ggsave("s_lattice.png", plot = lattice)


#################################
# MAPA DATOS DISCRETOS EN ÁREAS #
#################################

# Cargamos los datos a representar por municipio: numero de habitantes por municipio en 2021 (https://www.ine.es/jaxiT3/Tabla.htm?t=2860&L=0)
Poblacion <- structure(list(Muni = c("07002", "07001", "07003", "07004", "07005", 
                                     "07901", "07006", "07007", "07008", "07009", "07010", "07011", 
                                     "07012", "07013", "07014", "07064", "07015", "07016", "07017", 
                                     "07018", "07026", "07019", "07020", "07021", "07022", "07023", 
                                     "07024", "07025", "07027", "07028", "07029", "07030", "07031", 
                                     "07033", "07034", "07032", "07035", "07036", "07037", "07902", 
                                     "07038", "07039", "07040", "07041", "07044", "07042", "07043", 
                                     "07045", "07059", "07046", "07049", "07050", "07048", "07051", 
                                     "07052", "07053", "07054", "07055", "07056", "07057", "07058", 
                                     "07047", "07060", "07061", "07062", "07063", "07065"), Poblacion = c(9477, 
                                                                                                          5741, 20651, 5963, 11571, 897, 7984, 542, 8895, 1084, 7037, 51567, 
                                                                                                          2684, 11425, 12003, 7530, 30638, 4243, 1379, 674, 50643, 181, 
                                                                                                          5134, 326, 18164, 4892, 11708, 682, 33726, 1465, 6250, 2385, 
                                                                                                          38224, 44809, 1569, 29578, 2242, 38357, 5444, 1500, 3087, 7515, 
                                                                                                          419366, 3028, 13873, 16969, 5624, 2057, 5010, 27205, 2185, 6635, 
                                                                                                          28299, 8920, 6877, 1724, 40038, 12767, 7507, 12342, 4126, 3542, 
                                                                                                          4122, 13491, 11835, 2042, 3558)), row.names = c(NA, -67L), class = c("tbl_df", 
                                                                                                                                                                               "tbl", "data.frame"))

# Unimos estos datos con el mapa haciendo un "matching"
Poblacion_Baleares <- left_join(Baleares, Poblacion, by = c("CUMUN" = "Muni"))

# Agrupamos los valores en rangos utilizando la función cut()
Poblacion_Baleares$Poblacion_Range <- cut(Poblacion_Baleares$Poblacion, breaks=c(0, 10000, 50000, 100000, Inf), labels=c("0-10k", "10k-50k", "50k-100k", "100k+"))

# Representamos de forma gráfica el mapa
s_lattice_poblacion <- ggplot() +
  geom_sf(data = Poblacion_Baleares, aes(fill=Poblacion_Range)) +
  scale_fill_manual(values=c("ivory2", "orange2", "orangered2", "red4"), 
                    name="Población", 
                    labels=c("0-10k", "10k-50k", "50k-100k", "100k+")) + 
  theme_minimal() +
  theme(axis.text=element_blank(),
        panel.grid = element_blank())

# Guardamos la representación en formato .png
ggsave("s_lattice_poblacion.png", plot = s_lattice_poblacion, width = 8, height = 6, units = "in")


#################################
# MAPA DATOS CONTINUOS EN ÁREAS #
#################################

# Cargamos los datos a representar por municipio: renta neta media por hogar por municipio en el año 2020 (https://www.ine.es/jaxiT3/Tabla.htm?t=30887&L=0)
Renta_media <- structure(list(Muni = c("07001", "07002", "07003", "07004", "07005", 
                                       "07006", "07007", "07008", "07009", "07010", "07011", "07012", 
                                       "07013", "07014", "07015", "07016", "07017", "07018", "07019", 
                                       "07020", "07021", "07022", "07023", "07024", "07025", "07026", 
                                       "07027", "07028", "07029", "07030", "07031", "07032", "07033", 
                                       "07034", "07035", "07036", "07037", "07038", "07039", "07040", 
                                       "07041", "07042", "07043", "07044", "07045", "07046", "07047", 
                                       "07048", "07049", "07050", "07051", "07052", "07053", "07054", 
                                       "07055", "07056", "07057", "07058", "07059", "07060", "07061", 
                                       "07062", "07063", "07064", "07065", "07901", "07902"), Renta = c(36070, 
                                                                                                        31381, 28344, 34493, 32347, 30210, 34200, 36713, 31074, 42192, 
                                                                                                        32380, 33354, 31387, 26086, 31019, 34856, 29741, 32350, 34183, 
                                                                                                        44038, 31054, 30665, 32497, 36852, 35563, 37684, 30775, 31841, 
                                                                                                        31032, 32590, 34252, 34491, 31156, 36359, 31963, 44052, 35540, 
                                                                                                        34073, 30985, 36510, 32095, 31104, 33880, 31351, 40144, 33410, 
                                                                                                        32630, 35215, 31560, 32981, 28666, 37644, 39241, 34394, 28930, 
                                                                                                        38723, 27850, 32820, 30498, 31959, 34323, 28936, 39908, 32201, 
                                                                                                        30865, 30010, 34456)), class = c("tbl_df", "tbl", "data.frame"
                                                                                                        ), row.names = c(NA, -67L))

# Unimos estos datos con el mapa haciendo un "matching"
Baleares_Renta <- left_join(Baleares, Renta_media, by = c("CUMUN" = "Muni"))

# Representamos de forma gráfica el mapa
s_lattice_renta <- ggplot() +
  geom_sf(data = Baleares_Renta, aes(fill=Renta))+
  scale_fill_gradient(low = "ivory2", high = "red4")+
  labs(fill = "Renta media")+
  theme_minimal()+
  theme(axis.text=element_blank(),
        panel.grid = element_blank())+
  theme(legend.text=element_text(size=8),
        legend.title=element_text(size=10))

# Guardamos la representación en formato .png
ggsave("s_lattice_renta.png", plot = s_lattice_renta, width = 8, height = 6, units = "in")

###################
# DISEASE MAPPING #
###################

# Vaciamos el environment de casi todos los objetos
rm(list = setdiff(ls(), c("temp", "url")))

# Cargamos los datos a representar por municipio: número de fallecidos  por el lugar de residencia año 1996 (https://www.ine.es/jaxi/Tabla.htm?tpx=23639&L=0)
datos <- structure(list(CUMUN = c("07001", "07002", "07003", "07004", "07005", 
                                  "07006", "07007", "07008", "07009", "07010", "07011", "07012", 
                                  "07013", "07014", "07015", "07016", "07017", "07018", "07019", 
                                  "07020", "07021", "07022", "07023", "07024", "07025", "07026", 
                                  "07027", "07028", "07029", "07030", "07031", "07032", "07033", 
                                  "07034", "07035", "07036", "07037", "07038", "07039", "07040", 
                                  "07041", "07042", "07043", "07044", "07045", "07046", "07047", 
                                  "07048", "07049", "07050", "07051", "07052", "07053", "07054", 
                                  "07055", "07056", "07057", "07058", "07059", "07060", "07061", 
                                  "07062", "07063", "07064", "07065", "07066", "07067"), Nacidos = c(35, 
                                                                                                     70, 143, 27, 68, 59, 5, 40, 5, 34, 370, 21, 52, 71, 226, 17, 
                                                                                                     4, 7, 1, 49, 3, 149, 45, 61, 6, 415, 191, 10, 50, 19, 189, 189, 
                                                                                                     313, 10, 14, 172, 19, 14, 42, 3081, 27, 133, 23, 81, 11, 128, 
                                                                                                     19, 124, 20, 28, 59, 37, 19, 204, 64, 48, 79, 19, 17, 24, 102, 
                                                                                                     99, 18, 83, 18, 4, 10), Fallecidos = c(66, 64, 55, 40, 85, 66, 
                                                                                                                                            9, 52, 9, 25, 116, 23, 91, 53, 153, 24, 7, 7, 3, 34, 10, 160, 
                                                                                                                                            34, 39, 2, 201, 198, 9, 46, 19, 202, 209, 285, 9, 21, 109, 24, 
                                                                                                                                            31, 78, 2773, 38, 125, 55, 121, 14, 95, 23, 72, 25, 40, 45, 39, 
                                                                                                                                            13, 125, 65, 48, 73, 43, 25, 27, 100, 44, 10, 34, 28, 12, 9)), class = c("tbl_df", 
                                                                                                                                                                                                                     "tbl", "data.frame"), row.names = c(NA, -67L))

# Necesitamos volver a leer la cartografía
mapa <- readOGR(dsn = "scc19/SECC_CE_20190101.shp")

# Filtramos solo la provincia de las Islas Baleares
Baleares <- subset(mapa, NPRO == "Balears, Illes")

# Seleccionar filas únicas por CUMUN

# Calculamos la matriz de adyacencia, como objeto nb "adj" y matriz dispersa "W".
adj <- poly2nb(Baleares)
W <- as(nb2mat(adj, style = "B"), "Matrix")

# Calculamos los casos esperados
datos_esperados <- sum(datos$Fallecidos) / sum(datos$Nacidos)
datos$Esperados <- datos_esperados * datos$Nacidos

# Calculamos el SMR
datos$smr <- datos$Fallecidos / datos$Esperados

# Unimos estos datos con el mapa 
datos_Baleares <- merge(Baleares, datos, by = "CUMUN")

# Representamos el SMR
par(mfrow= c(1,1))
spplot(datos_Baleares, "smr")

# Generamos una lista con los datos observados y esperados
d <- list(OBS = datos_Baleares$Fallecidos, 
          EXP = datos_Baleares$Esperados)

# Generamos un ínndice para los datos espaciales
d$idx <- 1:length(d$OBS)

# Definimos la fórmula 
form <- OBS ~ 1 + f(idx, model = "besag", graph = W)

# Ejecutamos el modelo con INLA
r.uni <- inla(formula = form, 
              data = d, 
              E = EXP, 
              family = "poisson",
              control.predictor = list(compute = TRUE))

# Calculamos el SMR suavizado
datos_Baleares$smr_suavizado <- r.uni$summary.fitted.values[ , "mean"]

# Representamos de forma gráfica el mapa smr
smr <- spplot(datos_Baleares, "smr")

# Guardamos la representación en formato .png
trellis.device(png, filename = "s_smr.png", width = 800, height = 600, units = "px", res = 100)
print(smr)
dev.off()

# Representamos de forma gráfica el mapa smr suavizado
dm <- spplot(datos_Baleares, "smr_suavizado")

# Guardamos la representación en formato .png
trellis.device(png, filename = "s_dm.png", width = 800, height = 600, units = "px", res = 100)
print(dm)
dev.off()


grid.arrange(smr, dm, nrow = 2, padding = 0)



