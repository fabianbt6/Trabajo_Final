
rm(list = ls())

#Libs====================================
library(tidyverse)
library(readxl)
library(lubridate)

#Cargar datos==========================================
 setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# dist_ingreso <- read_excel("../Datos/Distribución del ingreso.xlsx",
#                            sheet = "consolidado",
#                            col_types = rep("numeric", 7))

load("../Datos/data_dist_ing.RData")

#Convertir base a long=====================

# gini <- dist_ingreso %>%
#   group_by(Año) %>%
#   summarise(gini  = mean(gini))
# 
# dist_ingreso_long <- dist_ingreso %>%
#   select(-gini) %>%
#   pivot_longer(cols = -c(ingreso, Año),
#                names_to = "trimestre",
#                values_to = "cantidad") %>%
#   mutate(cantidad = replace_na(cantidad, 0),
#          fecha = make_date(Año, as.numeric(str_sub(trimestre, 1, 1)) * 3 , 1)) %>%
#   uncount(cantidad)

#save(dist_ingreso_long, gini,  file = "../datos/data_dist_ing.RData")


#Total de FT=======================  
dist_ingreso_long %>% 
  group_by(Año, trimestre) %>% 
  dplyr::summarize(FT = n())

#Quintiles========================
ingreso_conc <- dist_ingreso_long %>% 
  group_by(fecha) %>% 
  mutate(quintil = cut(ingreso, 
                       breaks = quantile(ingreso, probs = seq(0, 1, 0.2)),
                       include.lowest = T, 
                       labels(1:5))) %>% 
  group_by(fecha, quintil) %>% 
  summarize(ingreso_quintil = sum(ingreso)) %>% 
  group_by(fecha) %>% 
  mutate(ingreso_total = sum(ingreso_quintil), 
         distr = ingreso_quintil / ingreso_total)

ingreso_conc <- ingreso_conc %>% 
  filter(quintil == 5) %>%
  mutate(Año = year(fecha), 
         mes = month(fecha)) %>% 
  left_join(gini)

ingreso_conc %>%
  pivot_longer(cols = c(distr, gini), names_to = 'indicador', values_to = 'valor') %>% 
  ggplot(aes(x = fecha, y = valor, colour = indicador)) + 
  ggtitle("Concentración del ingreso en el quintil más alto y Coeficiente de Gini") +
  geom_line() + 
  theme_bw()


#save(ingreso_conc, file = "../Datos/ingreso_conc.RData")



