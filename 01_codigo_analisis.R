# Este código no es reproducible dada la confidencialidad de los datos y permite visualizar el análisis realizado.

# Cargamos las librerías necesarias
library(rgdal) 
library(openxlsx) 
library(ggplot2)
library(sf)
library(spdep)
library(INLA)
library(tidyverse)
library(BayesVarSel) 
library(ggcorrplot)
library(ape)
library(viridis)
library(RColorBrewer)
library(gridExtra)


###################################
# PREPARACIÓN PREVIA DE LOS DATOS #
###################################

# Cargamos los datos

# Objeto espacial

# Leemos los datos
data <- read.xlsx("data/processed/dataset_greenlulus2.xlsx",
                  colNames = TRUE)

# Cambiamos la escala del centro
data$CENTER <- data$CENTER/1000

# Comprobamos la estructura de los datos
str(data)

# Leemos el objeto espacial
data_spdt <- readOGR("data/processed/SHAPES/Valencia/Tracts_Cleaned_Valencia CLEAN.shp")

dir_project <- getwd()

# Dataframe

# Cambiamos a mayúsculas la variable CITY
data$CITY <- toupper(data$CITY)

data_city <- filter(data, CITY == toupper("Valencia"))

# Comprobamos si los identificadores coinciden 

# Preparamos para comprobar si los identificadores de área son los mismos
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
data_city$AREAKEY <- as.character(as.numeric(substrRight(data_city$TRACTID, 5)))

# Comprobamos el ID de área
if(any(data_city$AREAKEY == data_spdt@data$AREAKEY) == FALSE){
   warning("AREAKEY does not match")
 }else{
   cat("AREAKEY match!!!")
 }

rm(substrRight)

############################
# DESCRIPTIVO DE LOS DATOS #
############################

# Filtramos las variables que no son importantes para la Hipótesis que vamos a comprobar, eliminamos las que no vayamos a utilizar
data_city <- subset(data_city, select = c(GENT_TIME2, CH_GSAB_PRE2, NEWTRANSITB_PRE2, 
                                           POP_CHANGE_2, DEV_TIME2, GDP_CHANGE_2, PRE_1990_GREEN, 
                                           CENTER, REGION, CITY, SIZE, TRACTDENSITY_2010, TIMEFROM1990, AREAKEY))

# Hacemos un descriptivo de la variable respuesta y las variables explicativas adecuado 

# Analizamos de qué tipo de objeto se trata y observamos su estructura
class(data_city)
str(data_city) # Para determinar si las variables son numéricas o no

# Detección y tratamiento de datos ausentes

# Obtenemos los nombres de las columnas que tienen únicamente NA
names(data_city)[colSums(is.na(data_city)) == nrow(data_city)]

# Gráficos y tablas para comprender las distribuciones de las variables

# Gráficos para variables numéricas
plot(data_city$GENT_TIME2)
plot(data_city$CH_GSAB_PRE2)
plot(data_city$NEWTRANSITB_PRE2)
plot(data_city$POP_CHANGE_2)
plot(data_city$DEV_TIME2)
plot(data_city$GDP_CHANGE_2)
plot(data_city$PRE_1990_GREEN)
plot(data_city$CENTER)
plot(data_city$TRACTDENSITY_2010)
plot(data_city$TIMEFROM1990)

# Tablas para variables categóricas
table(data_city$REGION) 
table(data_city$CITY) 
table(data_city$SIZE) 
table(data_city$AREAKEY) 

# Mantenemos AREAKEY porque lo necesitaremos posteriormente

# Eliminamos las variables que no expliquen la variable respuesta
data_city <- subset(data_city, select = -c(TIMEFROM1990, POP_CHANGE_2, GDP_CHANGE_2, CITY, REGION, SIZE))

# De todas las variables
str(data_city)
head(data_city) 
str(data_city)
nrow(data_city)
ncol(data_city)
summary(data_city) 

# Revisión de tipos de variable
sapply(data_city, class)

# Detección y tratamiento de datos atípicos

# Creamos gráficos boxplot para las variables continuas
boxplot(data_city$GENT_TIME2,horizontal = TRUE)
boxplot(data_city$CH_GSAB_PRE2,horizontal = TRUE)
boxplot(data_city$NEWTRANSITB_PRE2,horizontal = TRUE)
boxplot(data_city$DEV_TIME2,horizontal = TRUE)
boxplot(data_city$PRE_1990_GREEN,horizontal = TRUE)
boxplot(data_city$CENTER,horizontal = TRUE)
boxplot(data_city$TRACTDENSITY_2010,horizontal = TRUE)

# Los valores atípicos no son significativos, por lo que no hacemos nada al respecto

# Correlación de variables

# Creamos una base de datos sin la variable AREAKEY, ya que no es numérica
data_cor <- subset(data_city, select = -c(AREAKEY))

# Hacemos la correlación entre todas las variables
correlacion <- cor(data_cor)

# Representamos las correlaciones
ggcorrplot(correlacion) 

# Gráficos de las variables

# GENT_TIME2  

# Duplicamos el objeto para poder modificarlo
data_graf <- data_city

# Definimos los rangos a representar
breaks <- c(-Inf, 0.0001, 0.05, 0.10, 0.20)
data_graf$GENT_TIME2 <- cut(data_graf$GENT_TIME2, breaks = breaks)
nuevo_df <- data.frame(rango = levels(data_graf$GENT_TIME2), frecuencia = table(data_graf$GENT_TIME2))

# Definimos los colores deseados
colores <- brewer.pal(4, "RdBu")

# Representamos gráficamente
ggplot(nuevo_df, aes(x = frecuencia.Var1, y = frecuencia.Freq, fill = frecuencia.Var1)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("0", "(0,0.5]", "(0.5,0.1]", "(0.1,0.2]")) +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  guides(fill = "none") +
  scale_fill_manual(values = colores)

# DEV_TIME2

# Duplicamos el objeto para poder modificarlo
data_graf <- data_city

ggplot(data_graf, aes(DEV_TIME2)) +
  geom_histogram(aes(fill=..count..), binwidth = 0.1) +
  scale_fill_gradient("Count", low = "#2166AC", high = "#B2182B") +
  geom_vline(aes(xintercept = mean(DEV_TIME2),
                 color = "media"),
             linetype = "dashed",
             size = 0.7,
             color= "darkgrey") +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  guides(fill = "none") 


# PRE1990GREEN

ggplot(data_graf, aes(x = PRE_1990_GREEN)) +
  geom_density(color= "#B2182B",
               alpha = 0.5,
               fill= "#B2182B")+ 
  geom_vline(aes(xintercept = mean(PRE_1990_GREEN),
                 color = "media"),
             linetype = "dashed",
             size = 0.7,
             color= "grey") +
  theme_minimal() +
  theme(axis.title.x = element_blank()) 

# TRACTDENSITY_2010

ggplot(data_graf, aes(TRACTDENSITY_2010)) +
  geom_histogram(aes(fill=..count..), color="white") +
  scale_fill_gradient("Count", low = "#2166AC", high = "#B2182B") +
  geom_vline(aes(xintercept = mean(TRACTDENSITY_2010),
                 color = "media"),
             linetype = "dashed",
             size = 0.7,
             color= "darkgrey") +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  guides(fill = "none") 

# Conectamos las variables que si que vamos a utilizar con el objeto espacial

# Verificamos que los identificadores coinciden
data_city$AREAKEY
data_spdt$AREAKEYT

# Cambiamos el nombre para que coincidan y no haya problemas posteriores
names(data_city)[names(data_city) == "AREAKEY"] <- "AREAKEYT"

# Unimos ambos objetos
data_espacial <- merge(data_spdt, data_city, by = "AREAKEYT")

# Verificamos que se trata de un objeto espacial
class(data_espacial)

# Estudiamos de forma exploratoria la distribución espacial de la variable respuesta y de las explicativas 

# Cambiamos la clase del objeto a sf
sfdf <- st_as_sf(data_espacial)

# Representamos
ggplot(sfdf) +
  geom_sf(color = "white", aes(fill = GENT_TIME2)) +
  scale_fill_gradient2(mid = "#2166AC", high = "#B2182B") +
  theme_void()+
  labs(fill = "")

ggplot(sfdf) +
  geom_sf(color = "black", aes(fill = CH_GSAB_PRE2)) +
  scale_fill_gradient2(mid = "#CCE5FF", high = "#B2182B")+
  theme_void()+
  labs(fill = "")

ggplot(sfdf) +
  geom_sf(color = "white", aes(fill = NEWTRANSITB_PRE2)) +
  scale_fill_gradient2(mid = "#2166AC", high = "#B2182B")+
  theme_void()+
  labs(fill = "")

ggplot(sfdf) +
  geom_sf(color = "white", aes(fill = DEV_TIME2)) +
  scale_fill_gradient2(mid = "#2166AC", high = "#B2182B")+
  theme_void()+
  labs(fill = "")

ggplot(sfdf) +
  geom_sf(color = "black", aes(fill = PRE_1990_GREEN)) +
  scale_fill_gradient2(mid= "#CCE5FF", high = "#B2182B")+
  theme_void()+
  labs(fill = "")

ggplot(sfdf) +
  geom_sf(color = "white", aes(fill = CENTER)) +
  scale_fill_gradient2(mid = "#2166AC", high = "#B2182B")+
  theme_void()+
  labs(fill = "")

ggplot(sfdf) +
  geom_sf(color = "black", aes(fill = TRACTDENSITY_2010)) +
  scale_fill_gradient2(mid = "#CCE5FF", high = "#B2182B")+
  theme_void()+
  labs(fill = "")

###########
# MODELOS #
###########

# Realizamos la selección de variables 
mod_sel <- Bvs(data = data_cor,
               formula =  GENT_TIME2 ~ ., 
               null.model = GENT_TIME2 ~ 1)

# hacemos un resumen de los modelos
summary(mod_sel)

# Seleccionamos los 10 primeros modelos
best_models <- mod_sel$modelsprob[1:10,]

# Guardamos el mejor de ellos y forzamos la entrada de otra variable en el modelo
best_mod <- "GENT_TIME2 ~ DEV_TIME2 + CH_GSAB_PRE2"

# Definimos la paleta de colores
colores <- c("#FDDBC7","#B2182B","#D6604D","#F4A582","#4393C3","#2166AC","#053061")

# Dibujamos el número de covariables del modelo verdadero adicionales a la hipótesis nula
ggplot(data = data.frame(x = factor(0:(length(mod_sel$postprobdim)-1)), y = mod_sel$postprobdim),
             aes(x = x, y = y)) +
        geom_bar(stat = "identity", fill = colores) +
        geom_text(aes(label = round(y,3)), vjust = 1.6, color = "white", size = 3.5) +
        xlab("") +
        ylab("Probabilidad") +
        theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+
  theme_minimal()+
  scale_fill_manual(values = colores)

# Plasmamos cuál es el mejor modelo
cat("El mejor modelo es \n")
cat(best_mod)

# Ajustamos el modelo con INLA
mod1 <- inla(as.formula(best_mod),
             data = sfdf,
             control.compute = list(dic = TRUE,
                                    waic = TRUE,
                                    cpo = TRUE))

# Calculamos los residuos
res <- data_city$GENT_TIME2 - mod1$summary.fitted.values$mean

# Hacemos un resumen de los residuos
summary(res)

# Calculamos la matriz de vecinos
adj <- poly2nb(data_espacial)
grid_grafo <- as(nb2mat(adj, style = "B"), "Matrix")
matrix_neigh <- as.matrix(inla.graph2matrix(grid_grafo))

# Calculamos el índice de Moran
if(Moran.I(res, matrix_neigh)$p.value < 0.05)
{
  print("Existe autocorrelación espacial")
}else{
  print("No existe autocorrelación espacial")
}

# Añadimos los residuos al df
sfdf$res <- res

# Dibujamos los residuos
ggplot(sfdf) +
  geom_sf(color = "black", aes(fill = res)) +
  scale_fill_gradient2(low= "#2166AC", high = "#B2182B")+
  theme_void()+
  labs(fill = "")

# Ajustamos el modelo espacial

# Definimos un índice
data_city$S <- 1:dim(data_city)[1]

# Definimos las prior
prior_prec <- list(prec = list(prior = "pc.prec", param = c(2, 0.05)))
prior_besag <-  list(theta = list(prior = "pc.prec", param = c(2, 0.05)))

# Especificamos la fórmula
formula_mod <-paste0(best_mod, 
                     "+f(S, model='besag', graph=grid_grafo, scale.model=TRUE, hyper=prior_besag)")

# Construimos las restricciones 
terms_predictor <- strsplit(strsplit(formula_mod, "~")[[1]][2], "[+]")[[1]]
pos <- grep("besag", terms_predictor, value = FALSE)
d <- pos - 2
terms_predictor <- trimws(terms_predictor)

if(d > 0 ){#If there are covariates
  #Create the A and e Ax = 0 
  A <- t(as.matrix(dplyr::select(data_city, one_of(terms_predictor[2:(pos-1)]))))
  e <- rep(0, d)
}

# Ajustamos el modelo con la componente espacial

# Ajustamos con INLA
mod2 <- inla(formula = eval(parse(text = formula_mod)),
             family = "gaussian",
             data = data_city,
             control.compute = list(
               dic  = TRUE,
               cpo  = TRUE, 
               waic = TRUE),
             control.family    = list(hyper = list(
               prec = prior_prec)),
             control.predictor = list(compute = TRUE), 
             num.threads       = 4,
             control.inla=list(strategy='laplace'),
             verbose = FALSE)

# Hacemos un resumen del modelo
summary(mod2)

# Calculamos una medida de referencia
for (i in 2:length(mod2$names.fixed)){
  print(1 - inla.pmarginal(0,mod2$marginals.fixed[[i]]))
}

mod2$summary.fixed

# Media posterior del efecto espacial
sfdf$SPmean <- round(mod2$summary.random$S[["mean"]], 4)

# Desviación típica posterior del efecto espacial
sfdf$SPsd <- round(mod2$summary.random$S[["sd"]],5)

# Representamos gráficamente
mapa1 <- ggplot(sfdf) +
  geom_sf(color = "black", aes(fill = SPmean)) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu")) +
  theme_void() +
  labs(title = "Media", fill = "")

mapa2 <- ggplot(sfdf) +
  geom_sf(color = "black", aes(fill = SPsd)) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu")) +
  theme_void()+
  labs(title = "Desviación típica", fill = "")

grid.arrange(mapa1, mapa2, nrow = 1)

# Media de los valores ajustados de la variable respuesta
sfdf$GENT_TIME2_mean <- mod2$summary.fitted.values$mean

# Desviación típica de los valores ajustados de la variable respuesta
sfdf$GENT_TIME2_sd <- mod2$summary.fitted.values$sd 

# Representamos gráficamente
mapa3 <- ggplot(sfdf) +
  geom_sf(color = "black", aes(fill = GENT_TIME2_mean)) +
  scale_fill_viridis(option = "F", direction= -1) +
  theme_void()+
  labs(title = "Media", fill = "")

mapa4 <- ggplot(sfdf) +
  geom_sf(color = "black", aes(fill = GENT_TIME2_sd)) +
  scale_fill_viridis(option = "F", direction= -1) +
  theme_void() +
  labs(title = "Desviación típica", fill = "")

grid.arrange(mapa3, mapa4, nrow = 1)
