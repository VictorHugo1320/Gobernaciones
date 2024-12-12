# instalamos los paquetes necesarios
#install.packages("xlsx")
#install.packages("repr")
#install.packages("ragg")
#install.packages("palmerpenguins")
#install.packages("maptools")
#install.packages("sf")
#install.packages("rjson")
#install.packages("stringr")
#install.packages("maps")
#install.packages("tidyverse")
#install.packages("ggplot2")



library(rjson)
library(stringr)
library(sf)

library(tidyverse)

library(ggplot2)


library(readxl)
library(repr)

library(ragg)
library(palmerpenguins)

library(maps)


#library(maptools) 

#1.- Cargo el mapa del ecuador
ecuador<- st_read("C:/Users/victo/OneDrive/Escritorio/Gobernaciones 2019/Stata/8. Mapas/entradas/2024/Provincias15.geojson", stringsAsFactors=TRUE)

#2.- Selecciono de la primera base los poligonos para mis mapas 
datos<-ecuador %>%
  select(name, geometry)

#3.- Cambio los nombres de las provincias para que sean iguales en ambas bases

#4.- En esta base cambio el nombre de la columna 
colnames(datos)[1]<- "Gobernacion"

##5. Cargo la matriz de excel con los datos que necesito 

datos2<-read_excel("C:/Users/victo/OneDrive/Documentos/MINISTERIO-DE-GOBIERNO/2024/Plan-Estratégico-Institucional/Revisar-Envío-SNP/mapas/1. datos/01-MAPIS.xlsx", sheet = "Ser. Públicos")

##6. Cambio el nombre de la primera columna de excel

colnames(datos2)[1]<- "Gobernacion"

##7.- Creo una base uniendo las columnas que necesito

total <- merge(datos2,datos,by="Gobernacion")

##9.- Calcular el summary de mi data total
summary(total)

##10.- Creamos la categoría para definir los colores

total <- total %>%
  mutate(CATEGORIA=cut(Publicos, breaks = c(0, 0.50, 0.75, 1), labels = c("Menor a 50%", "Entre 50% y 75%", "Mayor a 75%")))


##11. Creamos la nueva columna con la extracción de los tres primeros dígitos del número de la columna

total <- total %>%
  mutate(Publicos2 = paste0(round(Publicos * 100,2),"%"))
  
##12.- Convertimos la columna categoría a factor  
total[,4]<-as.factor(total$CATEGORIA)

#11. Creo mi mapa #1

ggplot()+
  geom_sf(data=total, aes(fill=CATEGORIA, geometry = geometry), color="#797793", size=4,linewidth = 0.75)+
  scale_fill_manual(name = "Servicios Básicos", values = c("Menor a 50%" = "#DE7278", "Entre 50% y 75%"="#deab00","Mayor a 75%"="#01ae50"))+
  geom_sf_text(data = total, aes(label=Gobernacion,geometry = geometry), color="#31266B", size=3.5, hjust = 0.55, vjust = -0.1, fontface = "bold")+
  geom_sf_text(data=total, aes(label=Publicos2,geometry = geometry),color="#31266B", size=3, hjust = 0.30, vjust = 1.09)+
  theme_minimal() +                     # white background
  theme(axis.text = element_blank(),    # remove geographic coordinates
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank()) # remove los grids
  #coord_sf(ndiscr = 0)                # remove grid in the background, ya no funka

