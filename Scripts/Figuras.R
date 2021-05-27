
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
library(tsDyn)
library(tikzDevice)




#Condicion de Estabilidad==========================

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
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Estadística/Práctica Profesional/Documento Final/Figuras"
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
