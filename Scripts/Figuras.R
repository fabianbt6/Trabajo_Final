
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
library(vars)
library(latex2exp)
library(tsDyn)
library(tikzDevice)

#Condicion de Estabilidad (No requiere cargar datos)==========================

# Simulacion de datos
B1 <- matrix(c(0.7, 0.2, 0.2, 0.7), 2)
var1 <- VAR.sim(B = B1, n = 100, include = "none")
var_estable <- VAR(var1 , p = 1 , type = "none" , season = NULL , exogen = NULL)
roots(var_estable)

B2 <- rbind(c(0.5, 0.5, 0.5), c(0, 0.5, 0.5))
var2 <- VAR.sim(B = B2, n = 100)
var_inestable <- VAR(var2 , p = 1 , type = "const" , season = NULL , exogen = NULL)
roots(var_inestable)

# Cons de datos
var_estable <- as_tibble(unlist(var1)) %>%
  mutate(Proceso = "Estable", 
         t = 1:nrow(var1))
colnames(var_estable) <- c("y1", "y2", "Proceso", "t")

var_inestable <- as_tibble(unlist(var2)) %>%
  mutate(Proceso = "Inestable", 
         t = 1:nrow(var2))
colnames(var_inestable) <- c("y1", "y2", "Proceso", "t")

var_data <- bind_rows(var_estable, var_inestable) %>% 
  pivot_longer(c(y1, y2), names_to = "Variable", values_to = "Valor")

#grafico

#Ubicacion del doc en Latex
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_var_cond_estabilidad.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

#ggplot
g_var_cond_estabilidad <- var_data %>% 
  ggplot(aes(x = t, y = Valor, color = Variable)) + 
  geom_line(aes(linetype = Variable), size = 0.75) + 
  facet_grid(Proceso ~ ., scales = "free") +
  scale_color_manual(values = c("#4BCDD1", "#D14F4B")) + 
  theme_bw() %+replace%
    theme(legend.position = "none", 
          panel.grid = element_line(colour = "white"))

print(g_var_cond_estabilidad)

dev.off()

setwd(oldwd)

#Cargar datos====================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file = "../Datos/data.RData")

#Grafico crecimiento del PIBr=========================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_pibryoy.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_pibryoy <- ggplot(data = data, aes(x = fecha, y = pibr_yoy)) + 
  geom_line() +
  ylab(TeX("$log\\frac{PIBr_t}{PIBr_{t-4}$")) + 
  theme_classic()

print(g_pibryoy)

dev.off()

setwd(oldwd)

#Grafico de créditos en función del PIB===================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_credpib.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_credpib <- ggplot(data = data, aes(x = fecha, y = cred_pib)) + 
  geom_line() +
  ylab("En términos del PIB nominal") + 
  theme_classic()

print(g_credpib)

dev.off()

setwd(oldwd)

#Apertura Comercial en función del PIB===================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_apertcomercial.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_apertcomercial <- ggplot(data = data, aes(x = fecha, y = apert_comercial)) + 
  geom_line() +
  ylim(0.50, 1) +
  ylab("En términos del PIB nominal") + 
  theme_classic()

print(g_apertcomercial)

dev.off()

setwd(oldwd)
#IED en función del PIB===================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_ied.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_ied <- ggplot(data = data, aes(x = fecha, y = ied_pib)) + 
  geom_line() +
  ylab("En términos del PIB nominal") + 
  theme_classic()

print(g_ied)

dev.off()

setwd(oldwd)


#TBP===================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_tbp.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_tbp <- ggplot(data = data, aes(x = fecha, y = tbp)) + 
  geom_line() +
  ylab(TeX("$\\%$")) + 
  theme_classic()

print(g_tbp)

dev.off()

setwd(oldwd)

#inflación===================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_inflacion.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_inflacion <- ggplot(data = data, aes(x = fecha, y = inflacion)) + 
  geom_line() +
  ylab("valor") + 
  theme_classic()

print(g_inflacion)

dev.off()

setwd(oldwd)

data %>% dplyr::select(tbp, apert_comercial, ied_pib) %>% 
  cor()
