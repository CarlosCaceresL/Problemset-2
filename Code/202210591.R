## Problem Set N°2
## Carlos Eduardo Caceres Leal
## 202210591
## Version de R 4.3.3

library(pacman)
library(rio)
library(skimr)
p_load(data.table)
library(tidyverse)

## Punto 1: Importar y exportar bases de datos

## Importar
location <- import("C:/Users/ZulmaL/Downloads/ProblemSet-2/Inputs/Modulo de sitio o ubicacion.dta")
identification <- import("C:/Users/ZulmaL/Downloads/ProblemSet-2/Inputs/Modulo de identificacion.dta")

## Exportar
setwd("C:/Users/ZulmaL/Downloads/ProblemSet-2/Outputs")
export(location, "location.rds")
export(identification, "identification.rds")


## Punto 2: Generar variables

## Bussiness_type
identification <- mutate(identification, bussines_type=case_when(identification$GRUPOS4 == "01" ~"agricultura",
                                                                 identification$GRUPOS4 == "02" ~ "Industria Manufacturera",
                                                                 identification$GRUPOS4 == "03" ~ "Comercio",
                                                                 identification$GRUPOS4 == "04" ~ "Servicios"))

## grupo_etario
edades_propietarios <- sample(18:80, 84753, replace = TRUE)
identification$edad <- edades_propietarios

identification <- mutate(identification, grupo_etario = 
                           case_when(identification$edad>=18 & identification$edad<25 ~"joven adulto",
                           identification$edad>=25 & identification$edad<35 ~"adulto_joven",
                           identification$edad>=35 & identification$edad<50 ~"adulto mediana edad",
                           identification$edad>=50 & identification$edad<65 ~"adulto edad madura",
                           identification$edad>=65 ~"adulto_mayor"))

## Ambulante
location <-mutate(location, ambulante=case_when(location$P3053 =="3"~ "1",
                                                location$P3053== "4" ~"1"
                                                ,location$P3053== "5" ~"1"))


## Punto 3: Eliminar filas/columnas de un conjunto de datos

## identification_sub
identification$ambulante <- location$ambulante
identification_sub <- select(.data=identification, DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, grupo_etario,ambulante, COD_DEPTO, F_EXP)

## location_sub
location_sub <- select(.data=location, DIRECTORIO,SECUENCIA_P,SECUENCIA_ENCUESTA,ambulante,P3054,P469,COD_DEPTO,F_EXP)


## Punto 4: Combinar bases de datos

base_datos_combinada <- merge(x=location_sub, y=identification_sub, by = c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA"), all = TRUE)


## Punto 5: Descriptivas

## Estadisticas Descriptivas
summary(identification$edad)
skim(location$F_EXP)
summary(base_datos_combinada$DIRECTORIO)

## Variables Descriptivas
summarise(.data=location, encuestados_dept08 =sum(COD_DEPTO=="08"))
## Se encuestaron 5494 personas del departamento con codigo 08.

identification %>% group_by(COD_DEPTO) %>% summarise(mean_edad=mean(edad))
## La edad media de los propietarios de micronegocios en el departamento con codigo 05 es de 48.8 años.

identification %>% group_by(bussines_type) %>% summarise(jovenes_tipo_negocio=sum(grupo_etario=="joven adulto" , na.rm=T))
## 1148 es la cantidad de jovenes adultos que dirigen micronegocios de agricultura.

## .