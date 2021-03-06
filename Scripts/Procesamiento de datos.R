
rm(list = ls())

#libs======================
library(readxl)
library(dygraphs)
library(xts)
library(forecast)
library(ggiraph)
library(plotly)
library(lubridate)
library(urca)
library(zoo)
library(car)
library(ggiraph)
library(tidyverse)

#Cargar datos=============
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data_original <- read_excel("../Datos/datos.xlsx",
                            sheet = "resumen", 
                            col_types = c("date",rep("numeric", 14)), 
                            )

data_original$fecha <- as.Date(data_original$fecha)

#load("../Datos/ingreso_conc.RData")

#funciones calculos================
crecimiento_interanual <- function(fecha, var) {
  data <- tibble(fecha, var)
  
  data_completa <- tibble(fecha,
                          var,
                          fecha_rezagada = fecha %m-% months(12)) %>%
    left_join(y = data, by = c("fecha_rezagada" = "fecha")) %>% 
    summarise(crecimiento = var.x / var.y - 1)
  
  data_completa$crecimiento
}

crecimiento_interanual_pbs <- function(fecha, var) {
  data <- tibble(fecha, var)
  
  data_completa <- tibble(fecha,
                          var,
                          fecha_rezagada = fecha %m-% months(12)) %>%
    left_join(y = data, by = c("fecha_rezagada" = "fecha")) %>% 
    summarise(crecimiento = (var.x - var.y) * 10000)
  
  data_completa$crecimiento
}

#Construccion de variables-==========

data <- data_original %>% 
  dplyr::select(-m1) %>% 
  mutate(pibn_rollsum_4q = rollapplyr(pibn, width = 4, FUN = sum, partial = T), 
         pibr_yoy = crecimiento_interanual(data_original$fecha, data_original$pibr),
         log_pibryoy = log(pibr_yoy + 1), 
         pibusa_yoy = crecimiento_interanual(data_original$fecha, data_original$pibusa),
         log_pibusayoy = log(pibusa_yoy + 1),
         inflacion = crecimiento_interanual(data_original$fecha, data_original$ipc),
         vartc = crecimiento_interanual(data_original$fecha, data_original$tc),
         mif = mif,
         ied_pib = (ied * tc / pibn),
         tir = tbp - inflacion,
         cred_pib = cred / pibn_rollsum_4q, 
         apert_comercial = (exp + imp) / pibn, 
         cred_yoy = crecimiento_interanual_pbs(fecha, cred_pib), 
         cpib.cri = log_pibryoy, 
         des.sf = cred_pib, 
         cpib.usa = log_pibusayoy, 
         ied.pib = ied_pib, 
         tendencia = 1:n()) %>% 
  na.omit()

# data %>% 
#   dplyr::select(fecha, mif, mifl1) %>% 
#   view()

data %>% ggplot(aes(fecha, cred_yoy)) + geom_line()

data %>% ggplot(aes(fecha, pibusa_yoy)) + geom_line()

data %>% ggplot(aes(fecha, pibr_yoy)) + geom_line()

data %>% ggplot(aes(fecha, log_pibryoy)) + geom_line()

View(data)

# data2 <- data %>%
#   left_join(select(ingreso_conc, fecha, distr, gini)) %>% 
#   na.omit() %>%
#   mutate(distr = distr * 100)

#Dummies
data$d_2Q02 <- ifelse(data$fecha == "2002-06-01", 1, 0)
data$d_2Q04 <- ifelse(data$fecha == "2004-06-01", 1, 0)
data$d_4Q07 <- ifelse(data$fecha == "2007-12-01", 1, 0)
data$d_1Q08 <- ifelse(data$fecha == "2008-03-01", 1, 0)
data$d_4Q08 <- ifelse(data$fecha == "2008-12-01", 1, 0)
data$d_1Q09 <- ifelse(data$fecha == "2009-03-01", 1, 0)
data$d_2Q09 <- ifelse(data$fecha == "2009-06-01", 1, 0)
data$d_3Q09 <- ifelse(data$fecha == "2009-09-01", 1, 0)
data$d_4Q09 <- ifelse(data$fecha == "2009-12-01", 1, 0)
data$d_1Q14 <- ifelse(data$fecha == "2014-03-01", 1, 0)
data$d_1Q20 <- ifelse(data$fecha == "2020-03-01", 1, 0)
data$d_2Q20 <- ifelse(data$fecha == "2020-06-01", 1, 0)
data$d_3Q20 <- ifelse(data$fecha == "2020-09-01", 1, 0)
data$d_4Q20 <- ifelse(data$fecha == "2020-12-01", 1, 0)

#Dummies de estacionalidad

data$est.d1 <- ifelse(quarter(data$fecha) == 1, 1, 0)
data$est.d2 <- ifelse(quarter(data$fecha) == 2, 1, 0)
data$est.d3 <- ifelse(quarter(data$fecha) == 3, 1, 0)
data$est.d4 <- ifelse(quarter(data$fecha) == 4, 1, 0)
  
data$apertcomercial75 <- ifelse(data$apert_comercial > 75, 1, 0)
data$inflacion7 <- ifelse(data$inflacion > 7, 1, 0)

#save(data_original, file = "../Datos/data_original.RData")
#save(data, file = "../Datos/data.RData")

