
rm(list = ls())

#libs======================
library(tidyverse)
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
library(kableExtra)
library(lmtest)

#Cargar datos======================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file = "../Datos/data.RData")
load(file = "../Datos/data_original.RData")
load(file = "../Datos/covariables_ts_data.RData")

#Convertir series a formato ts===================
pib_ts <- data %>% dplyr::select(fecha, pibr)
pib_ts <- ts(pib_ts %>% dplyr::select(pibr), 
             start = c(year(pib_ts$fecha[1]), quarter(pib_ts$fecha[1])), 
             end = c(year(pib_ts$fecha[nrow(pib_ts)]), quarter(pib_ts$fecha[nrow(pib_ts)])), 
             frequency = 4)

cred_pib_ts <- data %>% dplyr::select(fecha, cred_pib) 
cred_pib_ts <- ts(cred_pib_ts %>% dplyr::select(cred_pib), 
                  start = c(year(cred_pib_ts$fecha[1]), quarter(cred_pib_ts$fecha[1])), 
                  end = c(year(cred_pib_ts$fecha[nrow(cred_pib_ts)]),
                          quarter(cred_pib_ts$fecha[nrow(cred_pib_ts)])), 
                  frequency = 4)

#Funciones============================
graf_estacionalidad <- function(mydata, fecha, var) {
  
  fecha <- enquo(fecha)
  var <- enquo(var)
  
  mydata %>% 
    dplyr::select(!!fecha, !!var) %>%
    mutate(Q = paste("Trimestre",quarter(!!fecha))) %>% 
    group_by(Q) %>% 
    mutate(promedio = mean(!!var)) %>%
    ggplot(aes(x = !!fecha, y = !!var)) + 
    geom_line() + 
    geom_hline(aes(yintercept = promedio, colour = "promedio")) +
    scale_color_manual(name = "", values = c(promedio = "blue")) +
    facet_grid(. ~ Q) +
    labs(title = "Promedio por trimestre") +
    theme_bw() %+replace%
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90))
}

#PIB=====================
#Estacionalidad
graf_estacionalidad(data, fecha, pibr_yoy)

#Estacionariedad serie original

#Pruebas de raíz unitaria
ur.df(pibryoy_ts, selectlags = "AIC", type = "trend") %>% 
  summary()

#Correlogramas
ggAcf(pibryoy_ts) +
  ggtitle("Crecimiento Interanual del PIB Real (Serie original)") + 
  theme_bw()

ggPacf(pibryoy_ts) +
  ggtitle("Crecimiento Interanual del PIB Real (Serie original)") + 
  theme_bw()

#Estacionariedad serie en primera diferencias

#Definir variable como ts
dpibryoy_ts <- diff(pibryoy_ts)

#Pruebas de raíz unitaria
ur.df(dpibryoy_ts, selectlags = "AIC", type = "none") %>% 
  summary()

#Correlogramas serie en primeras diferencias
ggAcf(dpibryoy_ts) +
  ggtitle("Crecimiento interanual del PIB Real (Serie en primera diferencias)") + 
  theme_bw()

ggPacf(dpibryoy_ts) +
  ggtitle("PIB Real (Serie en primera diferencias)") + 
  theme_bw()

#Correlogramas serie diferenciada
dspibryoy_ts <- diff(diff(pibryoy_ts, 4), 1)

ggAcf(dspibryoy_ts) +
  ggtitle("Crecimiento interanual del PIB Real (Serie diferenciada con 4 rezagos)") + 
  theme_bw()

ggPacf(dspibryoy_ts) +
  ggtitle("PIB Real (Serie diferenciada con 4 rezagos)") + 
  theme_bw()

#Creditos al PIB=====================
#Estacionalidad
graf_estacionalidad(data, fecha, cred_pib)

#Estacionariedad serie original

#Pruebas de raíz unitaria
ur.df(cred_pib_ts, selectlags = "AIC", type = "trend") %>% 
  summary()

#Correlogramas
ggAcf(cred_pib_ts) +
  ggtitle("Desarrollo del Sistema Financiero (Serie original)") + 
  theme_bw()

ggPacf(cred_pib_ts) +
  ggtitle("Desarrollo del Sistema Financiero (Serie original)") + 
  theme_bw()

#Estacionariedad serie en primera diferencias

#Definir variable como ts
dcred_pib_ts <- diff(cred_pib_ts)

#Pruebas de raíz unitaria
ur.df(dcred_pib_ts, selectlags = "AIC", type = "none") %>% 
  summary()

#Correlogramas serie en primeras diferencias
ggAcf(dcred_pib_ts) +
  ggtitle("Desarrollo del Sistema Financiero (Serie en primera diferencias)") + 
  theme_bw()

ggPacf(dcred_pib_ts) +
  ggtitle("PIB Real (Serie en primera diferencias)") + 
  theme_bw()

#Correlogramas serie diferenciada
dspib_ts <- diff(diff(dcred_pib_ts, 4), 1)

ggAcf(dcred_pib_ts) +
  ggtitle("PIB Real (Serie diferenciada con 4 rezagos)") + 
  theme_bw()

ggPacf(dcred_pib_ts) +
  ggtitle("PIB Real (Serie diferenciada con 4 rezagos)") + 
  theme_bw()

#Cointegración entre el PIB Real y el sistema financiero================
mod_lin <- lm(pibr ~ cred_pib, data = data)
summary(mod_lin)

residuo <- mod_lin$residuals

#Estacionariedad serie original

res_ts <- ts(residuo, 
             start = c(year(data$fecha[1]), quarter(data$fecha[1])), 
             end = c(year(data$fecha[nrow(data)]),
                     quarter(data$fecha[nrow(data)])), 
             frequency = 4)

#Pruebas de raíz unitaria
ur.df(res_ts, selectlags = "AIC", type = "drift") %>% 
  summary()

#Correlogramas
ggAcf(res_ts) +
  ggtitle("Residuo de la regresión PIB vs Desarrollo del Sistema Financiero") + 
  theme_bw()

ggPacf(res_ts) +
  ggtitle("Residuo de la regresión PIB vs Desarrollo del Sistema Financiero") + 
  theme_bw()

#Guardar las series================================

save(pib_ts, cred_pib_ts, res_ts, 
     file = "../Datos/ts_data.Rdata")


