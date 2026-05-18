---
title: "Proyecto Final Demografía-9219: Chiapas"
author:
  - "Campos Ruiz Evelyn Paola"
  - "Flores Rico André Alán"
format: pdf
editor: visual
---

## Introducción

Chiapas es uno de los 32 estados que conforman los Estados Unidos Mexicanos, ubicado en el sureste del país. Con una extensión territorial de 73,289 km² divididos en 124 municipios, Chiapas limita al norte con Tabasco, al este con la República de Guatemala, al sur con el Océano Pacífico y al oeste con Oaxaca y Veracruz. Es la octava entidad más extensa del país y se caracteriza por su gran diversidad cultural, albergando una de las poblaciones indígenas más numerosas de México, principalmente de las etnias tzotzil, tzeltal, chol y zoque. Su cercanía con la frontera sur y su riqueza natural e histórica lo convierten en un estado con dinámicas migratorias y demográficas particulares que merecen ser analizadas.

Ahora con ayuda de las proyecciones de CONAPO, realizaremos mi equipo y yo un pequeño análisis de la pirámide poblacional correspondiente al estado de Chiapas para el año 2026 y 2070.

```{r}
# 1. Remover los objetos
rm(list = ls())

# 2. Instalar paquetes 
#install.packages("data.table", 
                # dependencies = T)

# 3. Cargar paquetes
library(data.table)

# 4. Descargar tablas de datos
pop <- fread("https://repodatos.atdt.gob.mx/CONAPO/proyecciones/00_Pob_Mitad_1950_2070.csv")

# 5. Exploración de la tabla de población
table(pop$ENTIDAD)
table(pop$CVE_GEO)
table(pop$ANIO)

names(pop)
sum(pop$POBLACION)
# 6. Instalar y cargar ggplot2 
#install.packages("ggplot2")
library(ggplot2)

# 7. Preparar los datos para la pirámide
pop_chis <- pop[ENTIDAD == "Chiapas" & ANIO == 2026, .(SEXO, EDAD, POBLACION)]

# 8. Convertir población masculina a valores negativos para graficar
pop_chis[, POBLACION := ifelse(SEXO == "Hombres", -POBLACION, POBLACION)]

# 9. Graficar pirámide poblacional
ggplot(pop_chis, aes(x = EDAD, y = POBLACION, fill = SEXO)) +
  geom_bar(stat = "identity", width = 1) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = "Pirámide poblacional de Chiapas, 2026",
       x = "Edad",
       y = "Población") +
  theme_minimal() +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "salmon"))

```

## Análisis de la pirámide poblacional 2026

En la pirámide de 2026 podemos ver que Chiapas tiene una población mayormente joven, porque la base es ancha. La mayoría de la gente tiene entre 0 y 25 años. Hay un poco más de hombres que de mujeres en los primeros años, pero después de los 30 años las mujeres empiezan a ser más porque viven más años. También se empieza a notar que los niños de 0 a 4 años son menos que los de 5 a 9 años, lo que sugiere que la población ya no está creciendo tan rápido.

```{r}
#1. Remover los objetos 
rm(list = ls())

#2. Cargar paquetes

library(data.table) 
library(ggplot2)

#3. Descargamos las tablas de datos

pop <- fread( "https://repodatos.atdt.gob.mx/CONAPO/proyecciones/00_Pob_Mitad_1950_2070.csv" )

#4. Filtramos tabla de datos para el estado de Chiapas en el año 2070

pop_chis_2070 <- pop[ ENTIDAD == "Chiapas" & ANIO == 2070, .(SEXO, EDAD, POBLACION)]

#5. Convertimos la población masculina a valores negativos para graficar

pop_chis_2070[, POBLACION := ifelse(SEXO == "Hombres", -POBLACION, POBLACION)]

#6. Graficar pirámide poblacional

ggplot(pop_chis_2070, aes(x = EDAD, y = POBLACION, fill = SEXO)) + geom_bar(stat = "identity", width = 1) + coord_flip() + scale_y_continuous(labels = abs) + labs(title = "Pirámide poblacional de Chiapas, 2070", x = "Edad", y = "Población") + theme_minimal() + scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "salmon"))

```

## Análisis de la pirámide poblacional 2070

Para el 2070 la pirámide cambió mucho. Ya no tiene forma de pirámide, sino más como un rectángulo. La base se hizo más angosta porque ahora nacen menos niños. La mayoría de la gente tiene entre 30 y 60 años, es decir, es una población adulta. También hay muchas más personas mayores que en 2026, especialmente mujeres. Esto muestra que para 2070 Chiapas tendrá una población más envejecida, con menos niños y jóvenes, y más adultos mayores.


###MEDIA PONDERADA



###CRECIMIENTO EXPONENCIAL

```{R}
cre_exp <- function(K_0, K_T, t_0, t_T, t{
  
  
  return()
}

mort <- fread("https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_DeathsBySingleAgeSex_Medium_1950-2023.csv.gz")

popo <- fread("https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationBySingleAgeSex_Medium_1950-2023.csv.gz")


#mexico 208
```
