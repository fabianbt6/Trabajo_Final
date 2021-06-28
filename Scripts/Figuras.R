
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
library(gtsummary)

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

mod_lm <- lm(log_pibryoy ~ cred_pib, data = data)
summary(mod_lm)

mydata <- tibble(fecha = data$fecha,
                 log_pibryoy = data$log_pibryoy,
                 residuos = scale(mod_lm$residuals),
                 cred_pib = data$cred_pib,
                 `Desempeño Económico` = ifelse(data$pibr_yoy < 0, 
                                                "Contracción", 
                                                "Crecimiento"),
                 flag  = ifelse(abs(residuos) > 2, "1", "0"))
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

#Grafico Apertura Comercial en función del PIB===================
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
#Grafico IED en función del PIB===================
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


#Grafico TBP===================
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

#Grafico inflación===================
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

#Tabla estadísticas descriptivas======================
vars <- tiblle(c("log_pibryoy", "cred_pib", c("apert_comercial", "ied_pib", "tbp")) 
vars_nombre <- c(paste0("Crecimiento económico", footnote_marker_alphabet(1, "latex")), 
                 paste0("Desarrollo del Sistema Financiero", footnote_marker_alphabet(2, "latex")), 
                 paste0("Apertura Comercial", footnote_marker_alphabet(2, "latex")), 
                 paste0("IED", footnote_marker_alphabet(2, "latex")), 
                 paste0("TBP", footnote_marker_alphabet(3, "latex")))
vars_unidades <- c("Crecimiento", "En relación al PIB Nominal", 
                 "En relación al PIB Nominal", "En relación al PIB Nominal", "Porcentaje")

variables_descripciones <- tibble(vars, vars_nombre, vars_unidades, orden = 1:length(vars))

data_long <- data %>% 
  dplyr::select(fecha, all_of(vars)) %>% 
  pivot_longer(-fecha, "Indicador", values_to = "valor")

View(data)

tab_descriptivos <- data_long %>%   
  group_by(Indicador) %>% 
  summarise(Promedio = round(mean(valor), 3), 
            Mínimo  = round(min(valor), 3), 
            Máximo = round(max(valor), 3), 
            `Desviación Estándar` = round(sd(valor), 3), 
            `Coef. de Variación`  = round(`Desviación Estándar` / Promedio, 3)) %>% 
  ungroup() %>% 
  left_join(variables_descripciones, by = c("Indicador" = "vars")) %>% 
  mutate(Indicador = vars_nombre) %>% 
  arrange(orden) %>% 
  select(-c("vars_nombre", "vars_unidades", "orden"))
  
setwd("C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras")

knitr::kable(tab_descriptivos, booktabs = TRUE, 
             caption = "Estadísticas Descriptivas",
             label = "descritptivos",
             format = "latex") %>% 
  kable_styling(latex_options = c("striped", "scale_down"), full_width = T,
                font_size = 7) %>% 
  column_spec(1, width = "10em") %>% 
  footnote(general = "Fuente: Banco Central de Costa Rica. ",
           alphabet = c("Unidad de medida: crecimiento anual; ", 
                        "Unidad de medida: En términos del PIB; ", 
                        "Unidad de medida: Porcentaje") 
           ) %>% 
  readr::write_lines(file = "tabla_descriptivos.tex")


#Grafico crecimiento economico vs desarrollo del sistema financiero===================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_crec_vs_dsf.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_crec_vs_dsf <- mydata %>%
  mutate(`Desarrollo del S.F.` = ifelse(cred_pib > 0.401, "Alto", "Bajo"), 
         Periodo = ifelse(fecha < "2008-03-01", "Antes de 2008", "Posterior a 2008")) %>% 
  ggplot(aes(x = log_pibryoy, y = cred_pib, fill = Periodo)) + 
  ylab("Crédito al Sector Privado (proporción del PIBn)") + 
  xlab("Crecimiento interanual del PIB") +
  geom_point(aes(colour = Periodo), size = 1) + 
  geom_smooth(method = "lm", se = F, color = "grey", size = 0.65) +
  scale_color_manual(values = c("#00C784", "#C70043")) +
  theme_minimal() %+replace% 
  theme(legend.position = "top", 
        title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.spacing.x = unit(0.35, 'cm'),
        axis.title = element_text(size = 6), 
        axis.text=element_text(size = 6))

print(g_crec_vs_dsf)

dev.off()

setwd(oldwd)

data %>% dplyr::select(log_pibryoy, cred_pib) %>% 
  cor()

data %>% dplyr::select(fecha, log_pibryoy, cred_pib) %>% 
  filter(fecha < "2008-03-01") %>% 
  dplyr::select(-fecha) %>% 
  cor()

data %>% dplyr::select(fecha, log_pibryoy, cred_pib) %>% 
  filter(fecha >= "2008-03-01") %>% 
  dplyr::select(-fecha) %>% 
  cor()



cormat <- data %>% 
  select(log_pibryoy, cred_pib, ied_pib, apert_comercial, tbp) %>% 
  cor() %>% 
  round(2)

colnames(cormat) <- c("Crec. Económico", "Desarrollo del S.F.", "IED", "Apert.Com", "TBP")
row.names(cormat) <- c("Crec. Económico", "Desarrollo del S.F.", "IED", "Apert.Com", "TBP")

knitr::kable(cormat, booktabs = TRUE, 
             caption = "Matriz de correlación",
             label = "cormat",
             format = "latex") %>% 
  kable_styling(latex_options = c("striped", "scale_down"), full_width = T,
                font_size = 7) %>% 
  column_spec(1, width = "10em") 
  

