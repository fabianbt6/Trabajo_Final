
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
library(fUnitRoots)
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
load(file = "../Datos/data_diff.RData")

mod_lm <- lm(log_pibryoy ~ cred_pib, data = data)
summary(mod_lm)

vars <- tibble(indicador = c("log_pibryoy", "cred_pib", "apert_comercial", "ied_pib", "tbp"), 
               vars_nombre = c("Crecimiento económico", "Desarrollo del Sistema Financiero", 
                               "Apertura Comercial", "IED", "TBP"))
               
mydata <- tibble(fecha = data$fecha,
                 log_pibryoy = data$log_pibryoy,
                 residuos = scale(mod_lm$residuals),
                 cred_pib = data$cred_pib,
                 `Desempeño Económico` = ifelse(data$pibr_yoy < 0, 
                                                "Contracción", 
                                                "Crecimiento"),
                 flag  = ifelse(abs(residuos) > 2, "1", "0"))

#Funciones===============================
acf_plot <- function(mydata, alfa) {
  
  ic <- qnorm(1 - alfa / 2) / sqrt(nrow(mydata))
  
  dataset <- mydata %>% 
    pivot_longer(-fecha, names_to = "indicador", values_to = "valor") %>% 
    dplyr::select(-fecha) %>% 
    nest(data = valor) %>% 
    summarise(indicador = indicador, 
              acf = map(data, ~ acf(x = .,
                                    lag.max = NULL, 
                                    plot = F)$acf), 
              pacf = map(data, ~ acf(x = .,
                                     lag.max = NULL, 
                                     plot = F, 
                                     type = "partial")$acf)) %>% 
    pivot_longer(-indicador, names_to = "tipo", values_to = "valor") %>% 
    unnest(valor) %>% 
    group_by(indicador, tipo) %>% 
    mutate(rezago = 1:n())
  
  g <- dataset %>%
    dplyr::filter(tipo == "acf") %>%
    ggplot(aes(x = rezago,  y = valor)) +
    geom_segment(mapping = aes(xend = rezago, yend = 0)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = ic, linetype = 2, color = 'darkblue') +
    geom_hline(yintercept = -ic, linetype = 2, color = 'darkblue') +
    facet_grid(indicador ~ .) +
    theme_bw()
  
  return(g)
  
}

irf_comparativo <- function(modelos, nombres, impulse_var, resp_var, acumulado = F, nper = 12) {
  
  tibble(modelos = modelos) %>% 
    mutate(modelo = nombres,
           irf_obj = map(modelos, ~ irf(x = ., 
                                        impulse = impulse_var, 
                                        response = resp_var, 
                                        cumulative = acumulado)), 
           irf = map(irf_obj, "irf"), 
           l_inf = map(irf_obj, "Lower"), 
           l_sup = map(irf_obj, "Upper")) %>% 
    dplyr::select(-modelos, -irf_obj) %>% 
    unnest(-modelo) %>% 
    unnest(-modelo) %>% 
    mutate(periodo = rep(0:(nper - 2), length(modelos)), 
           irf = irf[,1], 
           l_inf = l_inf[,1], 
           l_sup = l_sup[,1])
}
irf_ggplot <- function(data, tit, lab, intervalo = T, nmax = 12, step = 2) {
  
  line_size <- 0.65
  
  graf <- data %>%
    ggplot(aes(x = periodo, y = irf)) + 
    geom_line(aes(colour = modelo), size = line_size) + 
    scale_x_continuous(breaks = seq(0, nmax, by = step)) +
    geom_hline(yintercept = 0) +  
    ggtitle(tit) + 
    ylab(lab) +
    theme_bw() %+replace% 
    theme(legend.position = "none", 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  
  if(intervalo == T) {
    graf <- graf + 
      geom_line(aes(x = periodo, y = l_sup), colour = "darkgrey", 
                linetype = "dashed",
                size = line_size) + 
      geom_line(aes(x = periodo, y = l_inf), colour = "darkgrey", 
                linetype = "dashed", 
                size = line_size) 
  } 
  
  return(graf)
  
}  
irf_ggplot_comparativo <- function(data, tit, lab, intervalo = T) {
  
  line_size <- 0.65
  
  graf <- data %>%
    ggplot(aes(x = periodo, y = irf)) + 
    geom_line(aes(colour = modelo), size = line_size) + 
    scale_x_continuous(breaks = seq(0, 12, by = 2)) +
    facet_wrap(. ~ modelo) +
    geom_hline(yintercept = 0) +  
    ggtitle(tit) + 
    ylab(lab) +
    theme_bw() %+replace% 
    theme(legend.position = "none", 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  
  if(intervalo == T) {
    graf <- graf + 
      geom_line(aes(x = periodo, y = l_sup), colour = "darkgrey", 
                linetype = "dashed",
                size = line_size) + 
      geom_line(aes(x = periodo, y = l_inf), colour = "darkgrey", 
                linetype = "dashed", 
                size = line_size) 
  } 
  
  return(graf)
  
}  

#Grafico crecimiento del PIBr=========================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_pibryoy.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_pibryoy <- ggplot(data = data, aes(x = fecha, y = log_pibryoy)) + 
  geom_line() +
  ylab("Crecimiento interanual del PIB") + 
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


#Grafico crecimiento del PIB USA=========================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_pibusa.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_pibusa <- ggplot(data = data, aes(x = fecha, y = log_pibusayoy)) + 
  geom_line() +
  ylab("Crecimiento interanual del PIB") + 
  theme_classic()

print(g_pibusa)

dev.off()

setwd(oldwd)

#Grafico de variación del tipo de cambio=========================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_vartc.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_vartc <- ggplot(data = data, aes(x = fecha, y = vartc)) + 
  geom_line() +
  ylab("variación") + 
  theme_classic()

print(g_vartc)

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

#Grafico MIF===================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_mif.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_mif <- ggplot(data = data, aes(x = fecha, y = mif)) + 
  geom_line() +
  ylab(TeX("$\\%$")) + 
  theme_classic()

print(g_mif)

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
vars <-  c("log_pibryoy", "cred_pib", "log_pibusayoy", "ied_pib", "inflacion", "tbp") 
vars_nombre <- c(paste0("Crecimiento económico", footnote_marker_alphabet(1, "latex")), 
                 paste0("Desarrollo del Sistema Financiero", footnote_marker_alphabet(2, "latex")), 
                 paste0("Crecimiento econ. (EUA)", footnote_marker_alphabet(1, "latex")), 
                 paste0("IED", footnote_marker_alphabet(2, "latex")),
                 "Inflación",
                 "MIF")
vars_unidades <- c("Crecimiento", "En relación al PIB Nominal", 
                 "Crecimiento", "En relación al PIB Nominal", "Porcentaje")

variables_descripciones <- tibble(vars, vars_nombre, orden = 1:length(vars))

data_long <- data %>% 
  dplyr::select(fecha, all_of(vars)) %>% 
  pivot_longer(-fecha, "Indicador", values_to = "valor")

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
  select(-c("vars_nombre", "orden"))
  
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
  theme_classic() %+replace% 
  theme(legend.position = "top", 
        title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.spacing.x = unit(0.35, 'cm'),
        axis.title = element_text(size = 7), 
        axis.text = element_text(size = 7))

print(g_crec_vs_dsf)

dev.off()

setwd(oldwd)

data %>% dplyr::select(log_pibryoy, cred_pib) %>% 
  cor()

#Tabla de correlaciones=================================
data %>% dplyr::select(fecha, log_pibryoy, cred_pib) %>% 
  filter(fecha < "2008-03-01") %>% 
  dplyr::select(-fecha) %>% 
  cor()

data %>% dplyr::select(fecha, log_pibryoy, cred_pib) %>% 
  filter(fecha >= "2008-03-01") %>% 
  dplyr::select(-fecha) %>% 
  cor()

cormat <- data %>% 
  select(cpib.cri, des.sf, apert_comercial, cpib.usa, inflacion, tbp, vartc) %>% 
  cor() %>% 
  round(2)

nombres <- c("Crec. Económico", "Des. S.F.", "Apert. Com.", "Crec. EUA", "Inflación", "TBP", "Var. TC")
colnames(cormat) <- nombres
row.names(cormat) <- nombres

mod_lm <- lm(cpib.cri ~ des.sf + cpib.usa +  apert_comercial + vartc, data = data)
summary(mod_lm)
vif(mod_lm)

knitr::kable(cormat, booktabs = TRUE, 
             caption = "Matriz de correlaciones",
             label = "cormat",
             format = "latex") %>% 
  kable_styling(latex_options = c("striped", "scale_down"), full_width = T,
                font_size = 7) %>% 
  column_spec(1, width = "10em") 
  

#Grafico correlogramas niveles============================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_acf_pacf.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_acf_pacf <- data %>% dplyr::select(fecha, log_pibryoy, cred_pib) %>% 
  `colnames<-`(c("fecha", "Crecimiento Económico", "Desarrollo del S.F.")) %>% 
  acf_plot(alfa = 0.05) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 6), 
        axis.text=element_text(size = 6), 
        strip.text.y = element_text(size = 7))

print(g_acf_pacf)

dev.off()

setwd(oldwd)
#Tabla de pruebas de raíz unitaria niveles=====================================

pibr_df <- ur.df(data$log_pibryoy, type = "trend", selectlags = "AIC") %>% 
  summary()

cred_pib_df <- ur.df(data$cred_pib, type = "trend", selectlags = "AIC") %>% 
  summary()

tab_df <- tibble("Estadístico" = round(t(pibr_df@teststat)[, 1], 2) , 
                 pibr_df@cval %>% as_tibble()) %>% 
  bind_rows(tibble("Estadístico" = round(t(cred_pib_df@teststat)[, 1], 2),
                  cred_pib_df@cval %>% as_tibble())) %>% 
  mutate(`Parámetro` = c("tau_3", "phi_2", "phi_3", "tau_3", "phi_2", "phi_3")) %>% 
  relocate(`Parámetro`, .before = "Estadístico")

knitr::kable(tab_df, booktabs = TRUE, 
             caption = "Prueba Dickey-Fuller de las series en niveles",
             label = "DF_niveles",
             format = "latex") %>% 
  kable_styling(font_size = 8) %>% 
  column_spec(1, width = "10em") %>% 
  add_header_above(c("", "", "Valores Críticos"  = 3)) %>% 
  pack_rows("Crecimiento Económico", 1, 3) %>% 
  pack_rows("Desarrollo del S.F.", 4, 6)

#Grafico correlogramas primeras diff============================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_acf_pacf_diff.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_acf_pacf_diff <- data_diff %>% dplyr::select(fecha, log_pibryoy_diff, cred_pib_diff) %>% 
  `colnames<-`(c("fecha", "Crecimiento Económico", "Desarrollo del S.F.")) %>% 
  acf_plot(alfa = 0.05) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 6), 
        axis.text=element_text(size = 6), 
        strip.text.y = element_text(size = 7))

print(g_acf_pacf_diff)

dev.off()

setwd(oldwd)



#Tabla de pruebas de raíz unitaria primeras diff=====================================

pibr_diff_df <- ur.df(data_diff$log_pibryoy_diff, type = "drift", selectlags = "AIC") %>% 
  summary()

cred_pib_diff_df <- ur.df(data_diff$cred_pib_diff, type = "drift", selectlags = "AIC") %>% 
  summary()

tab_diff_df <- tibble("Estadístico" = round(t(pibr_diff_df@teststat)[, 1], 2) , 
                 pibr_diff_df@cval %>% as_tibble()) %>% 
  bind_rows(tibble("Estadístico" = round(t(cred_pib_diff_df@teststat)[, 1], 2),
                   cred_pib_diff_df@cval %>% as_tibble())) %>% 
  mutate(`Parámetro` = c("tau_2", "phi_1", "tau_2", "phi_1")) %>% 
  relocate(`Parámetro`, .before = "Estadístico")

knitr::kable(tab_diff_df, booktabs = TRUE, 
             caption = "Prueba Dickey-Fuller de las series en primeras diferencias",
             label = "DF_diff",
             format = "latex") %>% 
  kable_styling(font_size = 8) %>% 
  column_spec(1, width = "10em") %>% 
  add_header_above(c("", "", "Valores Críticos"  = 3)) %>% 
  pack_rows("Crecimiento Económico", 1, 2) %>% 
  pack_rows("Desarrollo del S.F.", 3, 4)

#Tabla resultados del análisis de cointegración====================

cajo_covars <- ca.jo(data %>% dplyr::select(cpib.cri, des.sf),
                     type = "eigen",
                     K = 7, 
                     ecdet = "trend", 
                     dumvar = dplyr::select(data, apert_comercial, cpib.usa, inflacion, tbp, vartc, est.d2, est.d3, est.d4),
                     spec = "longrun") %>% 
  summary()

cajo_simple <- ca.jo(data %>% dplyr::select(log_pibryoy, cred_pib), 
      type = "eigen",  
      K = 6, 
      ecdet = "const", 
      spec = "longrun") %>% 
  summary()

tabla_cajo1 <- tibble("Estadístico" = round(cajo_simple@teststat, 2), 
                     round(cajo_simple@cval, 2) %>% as_tibble()) %>% 
  bind_rows(tibble("Estadístico" = round(cajo_covars@teststat, 2), 
            round(cajo_covars@cval, 2) %>% as_tibble())) %>% 
  mutate(`Hipótesis` = rep(c("r <= 1", "r = 0"), 2)) %>% 
  relocate(`Hipótesis`, .before = `Estadístico`)

tabla_cajo2 <- tibble(`Hipótesis` = c("r <= 1", "r = 0"), 
                      "Modelo 9" = round(cajo_simple@teststat, 2),
                      "Modelo 10" = round(cajo_covars@teststat, 2),
                      round(cajo_simple@cval, 2) %>% as_tibble())

knitr::kable(tabla_cajo2, booktabs = TRUE, 
             caption = "Rango de cointegración: Estadístico de auto-valor máximo" ,
             label = "cajo",
             format = "latex") %>% 
  kable_styling(font_size = 10) %>% 
  column_spec(1, width = "10em") %>% 
  add_header_above(c("", "Estadístico" = 2, "Valores Críticos"  = 3))

#Tabla Test de Cointegración del residuo de la regresión lineal==============================
#Prueba DF
mod_lm <- lm(log_pibryoy ~ cred_pib, data= data)
summary(mod_lm)

resid_modlm <- ur.df(residuals(mod_lm) , type = "none", selectlags = "AIC") %>% 
  summary()

resid_modlm@cval

unitrootTest(residuals(mod_lm), lags = 1, type = "nc")

unitrootTable(trend = "nc", statistic = "t")
qunitroot(0.99, length(residuals(mod_lm)), trend = "nc", statistic = "t")  

tab_res_df <- tibble("Estadístico" = round(t(resid_modlm@teststat)[, 1], 2) , 
                     resid_modlm@cval %>% as_tibble()) %>% 
  mutate(`Parámetro` = c("tau_1")) %>% 
  relocate(`Parámetro`, .before = "Estadístico")

knitr::kable(tab_res_df, booktabs = TRUE, 
             caption = "Prueba Dickey-Fuller del residuo",
             label = "DF_res",
             format = "latex") %>% 
  kable_styling(font_size = 8) %>% 
  column_spec(1, width = "10em") %>% 
  add_header_above(c("", "", "Valores Críticos"  = 3))

#Correlograma

td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_acf_residuo.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

g_acf_residuo <- tibble(fecha = data$fecha,
                        residuos = residuals(mod_lm), 
                        log_pibr = data$log_pibryoy, 
                        dsf = data$cred_pib) %>%
  dplyr::select(fecha, residuos, log_pibr, dsf) %>% 
  `colnames<-`(c("fecha", "Residuos", "crecimiento económico", "dsf")) %>% 
  acf_plot(alfa = 0.05) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 6), 
        axis.text=element_text(size = 6), 
        strip.text.y = element_text(size = 7))

print(g_acf_pacf)

dev.off()

setwd(oldwd)










#Modelo VAR===================================================
# mod <- VAR(dplyr::select(data, cpib.cri, des.sf),
#            type = "both",
#            lag.max = 12, ic = "AIC",
#            exogen = dplyr::select(data, cpib.usa, ied.pib, mif, inflacion))

mod <- VAR(dplyr::select(data, cpib.cri, des.sf),
           type = "both",
           lag.max = 12, 
           ic = "AIC", 
           exogen = dplyr::select(data, apert_comercial, cpib.usa, inflacion, tbp, vartc, est.d2, est.d3, est.d4))

cor(dplyr::select(data, cpib.cri, des.sf, cpib.usa, inflacion, vartc, tir))
summary(mod)

mod_lm <- lm(cpib.cri ~ des.sf + apert_comercial + cpib.usa + inflacion + tbp + vartc, data = data)  

summary(mod_lm)
vif(mod_lm)

mod$datamat
mod1$datamat

#Análisis=======================================================
roots(mod, modulus = T)

##Evaluación del modelo

### Autocorrelación serial

residuals_mod <- data.frame(fecha = data$fecha[(nrow(data) - nrow(residuals(mod)) + 1):nrow(data)],
                             res = scale(residuals(mod))) %>% as_tibble()

residuals_mod_long <- residuals_mod %>%
  pivot_longer(-fecha, names_to = "indicador", values_to = "residuo estandarizado")

serial.test(mod, type = "PT.asymptotic")
serial.test(mod, type = "BG")
serial.test(mod, type = "PT.adjusted")
serial.test(mod, type = "ES")

ccf(residuals(mod)[, 1],  residuals(mod)[, 2], plot = T)

### Heterocedasticidad
arch.test(mod, lags.multi = 5, multivariate.only = T)

### Normalidad del residuo
normality.test(mod, multivariate.only = T)

knitr::kable(tabla_var_tests, booktabs = TRUE, 
             caption = "Pruebas de diagnosis del VAR",
             label = "modelo_var_pruebas",
             format = "latex") %>% 
  kable_styling(font_size = 10) 

### Estabilidad
stability(mod, type = "OLS-CUSUM") %>% plot()
roots(mod, modulus = T)

resz_mod <- residuals_mod_long %>% 
  ggplot(aes(x = fecha, y = `residuo estandarizado`)) + 
  geom_point_interactive(aes(tooltip = paste0("Fecha: ", fecha),
                             data_id = fecha)) + 
  geom_hline(yintercept = 2,linetype = "dashed") +
  geom_hline(yintercept = -2,linetype = "dashed") +
  facet_wrap(. ~ indicador) +
  ggtitle(str_wrap("Residuos del Modelo VAR", 40)) +
  theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA), legend.position = "none")

girafe(ggobj = resz_mod,
       width_svg = 10, height_svg = 4,
       options = list(
         opts_sizing(rescale = TRUE),
         opts_hover_inv(css = "opacity:0.2;"),
         opts_hover(css = "stroke-width:2;")
       ))

residuals_mod_long %>% 
  ggplot(aes(x = `residuo estandarizado`)) + 
  geom_density() + 
  facet_wrap(. ~ indicador) + 
  theme_bw() + 
  ggtitle("Gráficos de densidad para los residuos del Modelo VAR")

#dev.off()
residuals_mod_long %>% 
  ggplot(aes(sample = `residuo estandarizado`)) + 
  stat_qq() +
  stat_qq_line() +
  facet_wrap(. ~ indicador) + 
  theme_bw() + 
  ggtitle("QQplots los residuos del Modelo VAR")

#Granger causalidad
causality(mod, cause = "des.sf")
causality(mod, cause = "cpib.cri")

#Tabla Coeficientes delo modelo VAR======================
tab_var <- tibble(`Parámetros` = names(mod$varresult$cpib.cri$coefficients), 
                  cpib.cri = round(mod$varresult$cpib.cri$coefficients, 3),
                  t1 = round(summary(mod)$varresult$cpib.cri$coefficients[, "t value"], 2),
                  pvalue1 = summary(mod)$varresult$cpib.cri$coefficients[, "Pr(>|t|)"],
                  stars1 = ifelse(pvalue1 <= 0.001, " ***", ifelse(pvalue1 <= 0.01, " **", 
                                                                   ifelse(pvalue1 <= 0.05, " *", ifelse(pvalue1 <= 0.1, ".", "")))), 
                  des.sf = round(mod$varresult$des.sf$coefficients, 3), 
                  t2 = round(summary(mod)$varresult$des.sf$coefficients[, "t value"], 2),
                  pvalue2 = summary(mod)$varresult$des.sf$coefficients[, "Pr(>|t|)"],
                  stars2 = ifelse(pvalue2 <= 0.001, " ***", ifelse(pvalue2 <= 0.01, " **", 
                                                                   ifelse(pvalue2 <= 0.05, " *", ifelse(pvalue2 <= 0.1, ".", ""))))) %>%  
  mutate(cpib.cri = paste0(paste0(paste0(paste0(cpib.cri, " ("),t1),")"), stars1),
         des.sf = paste0(paste0(paste0(paste0(des.sf, " ("),t2),")"), stars2)) %>% 
  dplyr::select(`Parámetros`, cpib.cri, des.sf) %>% 
  column_to_rownames(var = "Parámetros")

knitr::kable(tab_var, booktabs = TRUE, 
             caption = "Coeficientes estimados del modelo VAR(4)",
             label = "modelo_var",
             format = "latex") %>% 
  kable_styling(font_size = 10) %>% 
  column_spec(1, width = "3cm") %>% 
  column_spec(2, width = "3cm") %>% 
  column_spec(3, width = "3cm") %>% 
  footnote(general = c("Códigos de significancia: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1", 
                       "Estadístico t en paréntesis"))

#Tabla de diagnostico modelo VAR=============
ptm_test <- serial.test(mod, type = "PT.asymptotic")
arch <- arch.test(mod, multivariate.only = T)
jb <- normality.test(mod, multivariate.only = T)

tabla_var_tests <- tibble(Prueba = c("Portmanteau", "ARCH", "Jarque-Bera"),
                          `Estadístico` = c(round(ptm_test$serial$statistic, 2),
                                            round(arch$arch.mul$statistic, 2), 
                                            round(jb$jb.mul$JB$statistic, 2)),
                          `Valor p` =  c(round(ptm_test$serial$p.value, 2), 
                                         round(arch$arch.mul$p.value, 2), 
                                         round(jb$jb.mul$JB$p.value, 2)))  

knitr::kable(tabla_var_tests, booktabs = TRUE, 
             caption = "Pruebas de diagnóstico del VAR",
             label = "modelo_var_pruebas",
             format = "latex") %>% 
  kable_styling(font_size = 11)
#Grafico acf modelo var========
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_acf_var.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 4)

t_acf_var <- tibble(acf(residuals(mod), plot = F)$acf %>% as_tibble())
colnames(t_acf_var) = c("cpib.cri", "cpib.cri - cred.pib", "des.sf", "des.sf - cpib.cri")

ic <- qnorm(1 - 0.05 / 2) / sqrt(nrow(data))

g_acf_var <- t_acf_var %>%
  dplyr::select(cpib.cri, des.sf) %>% 
  mutate(rezago = 1:n()) %>%  
  pivot_longer(-c(rezago), names_to = "variable", values_to = "valor") %>%
  arrange(desc(variable)) %>% 
  ggplot(aes(x = rezago,  y = valor)) +
  geom_segment(mapping = aes(xend = rezago, yend = 0)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = ic, linetype = 2, color = 'darkblue') +
  geom_hline(yintercept = -ic, linetype = 2, color = 'darkblue') +
  facet_grid(variable ~ .) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 7), 
        axis.text=element_text(size = 7), 
        strip.text.y = element_text(size = 8))

print(g_acf_var)

dev.off()

setwd(oldwd)

#Grafico ccf modelo var=====================================================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_ccf_var.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

ic <- qnorm(1 - 0.05 / 2) / sqrt(nrow(data))

g_ccf_var <- tibble(ccf = ccf(residuals(mod)[, 1], residuals(mod)[, 2], plot = F)$acf, 
                    rezago = -16:16) %>%  
  ggplot(aes(x = rezago,  y = ccf)) +
  geom_segment(mapping = aes(xend = rezago, yend = 0)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = ic, linetype = 2, color = 'darkblue') +
  geom_hline(yintercept = -ic, linetype = 2, color = 'darkblue') +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 8), 
        axis.text=element_text(size = 8))

print(g_ccf_var)

dev.off()

setwd(oldwd)

residuals_mod <- data.frame(fecha = data$fecha[(nrow(data) - nrow(residuals(mod)) + 1):nrow(data)],
                            res = scale(residuals(mod))) %>% as_tibble()
#Grafico qqplot modelo var=====================================================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_qqplot_var.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

residuals_mod <- data.frame(fecha = data$fecha[(nrow(data) - nrow(residuals(mod)) + 1):nrow(data)],
                            res = scale(residuals(mod))) %>% as_tibble()

residuals_mod_long <- residuals_mod %>%
  pivot_longer(-fecha, names_to = "indicador", values_to = "residuo estandarizado")

g_qqplot_var <- residuals_mod_long %>% 
  ggplot(aes(sample = `residuo estandarizado`)) + 
  stat_qq(size = 0.7) +
  stat_qq_line() +
  facet_wrap(. ~ indicador) + 
  theme_bw() + 
  xlab("Cuartiles teóricos") +
  ylab("Cuartiles observados") +
  theme_bw() +
  theme(axis.title = element_text(size = 8), 
        axis.text=element_text(size = 8))

print(g_qqplot_var)

dev.off()

setwd(oldwd)

#Tabla estabilidad VAR=============

tab_raizes <- tibble(param = 'Valores propios',
                     val = round(roots(mod), 2)) %>% 
  mutate(cons = 1:n())


tab_raizes <- tab_raizes %>% pivot_wider(param, cons, values_from = val) %>%   
  column_to_rownames("param")

knitr::kable(tab_raizes, booktabs = TRUE, 
             caption = "Módulo de los valores propios de los coeficientes del Modelo VAR (4)",
             label = "estabilidad_var",
             format = "latex") %>% 
  kable_styling(font_size = 10)

#Granger causalidad
granger_cred <- causality(mod, cause = "des.sf")
granger_pib <- causality(mod, cause = "cpib.cri")

tabla_caus <- tibble(Prueba  = c("D.S.F causa en el sentido de Granger a C.E.", 
                                  "C.E. causa en el sentido de Granger a D.S.F", 
                                  "Wald"),
                     `Estadístico` = c(round(granger_cred$Granger$statistic[1, ], 3), 
                                       round(granger_pib$Granger$statistic[1, ], 3),
                                       round(granger_cred$Instant$statistic[1, ], 3)),
                     `Valor p` = c(round(granger_cred$Granger$p.value[1, ], 3), 
                                   round(granger_pib$Granger$p.value[1, ], 3),
                                   round(granger_cred$Instant$p.value[1, ], 3))) 


knitr::kable(tabla_caus, booktabs = TRUE, 
             caption = "Pruebas de Causalidad",
             label = "var_causalidad",
             format = "latex") %>% 
  footnote(general = c("C.E. = Crecimiento económico", 
                     "D.S.F. = Desarrollo del Sistema Financiero"))

                                      
#Gráfico impulso-respuesta dsf_crec========================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_irf_dsf.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

irf1 <- irf(mod,
            impulse = "des.sf",
            response = "cpib.cri",
            n.ahead = 24,
            cumulative = F)  

var_dsf_crec_simple <- tibble(irf = irf1$irf %>% unlist(), 
                              l_inf = irf1$Lower %>% unlist(),
                              l_sup = irf1$Upper %>% unlist()) %>% 
  mutate(tipo = "Efecto simple", 
         modelo = "mod1", 
         periodo = 0:(nrow(.) - 1))


g_irf_dsf <- var_dsf_crec_simple %>% 
  ggplot(aes(x = periodo, y = irf)) +
  geom_line(size = 0.65, colour = "red") +
  scale_x_continuous(breaks = seq(0, 36, by = 4)) +
  geom_hline(yintercept = 0) +  
  geom_line(aes(x = periodo, y = l_sup), colour = "darkgrey", linetype = "dashed",
            size = 0.65) + 
  geom_line(aes(x = periodo, y = l_inf), colour = "darkgrey", linetype = "dashed", 
            size = 0.65) +
  ylab("crec. econ.") +
  theme_bw() %+replace%
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black") ,
        axis.title = element_text(size = 6), 
        axis.text=element_text(size = 6), 
        strip.text.y = element_text(size = 7))

print(g_irf_dsf)

dev.off()

setwd(oldwd)

#Gráfico impulso-respuesta crec_dsf========================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_irf_crec.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

irf1 <- irf(mod,
            impulse = "cpib.cri",
            response = "des.sf",
            n.ahead = 24,
            cumulative = F)  

var_crec_dsf_simple <- tibble(irf = irf1$irf %>% unlist(), 
                              l_inf = irf1$Lower %>% unlist(),
                              l_sup = irf1$Upper %>% unlist()) %>% 
  mutate(tipo = "Efecto simple", 
         modelo = "mod1", 
         periodo = 0:(nrow(.) - 1))

g_irf_crec <- var_crec_dsf_simple %>% 
  ggplot(aes(x = periodo, y = irf)) +
  geom_line(size = 0.65, colour = "red") +
  scale_x_continuous(breaks = seq(0, 36, by = 4)) +
  geom_hline(yintercept = 0) +  
  geom_line(aes(x = periodo, y = l_sup), colour = "darkgrey", linetype = "dashed",
            size = 0.65) + 
  geom_line(aes(x = periodo, y = l_inf), colour = "darkgrey", linetype = "dashed", 
            size = 0.65) +
  ylab("Desarrollo del s.f.") +
  theme_bw() %+replace%
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black") ,
        axis.title = element_text(size = 6), 
        axis.text=element_text(size = 6), 
        strip.text.y = element_text(size = 7))

print(g_irf_crec)

dev.off()

setwd(oldwd)


#Modelo VECM===================================================

vecm <- ca.jo(data %>% dplyr::select(cpib.cri, des.sf),
              type = "eigen",
              K = 7, 
              ecdet = "trend", 
              dumvar = dplyr::select(data, apert_comercial, cpib.usa, inflacion, tbp, vartc, est.d2, est.d3, est.d4),
              spec = "longrun") 

summary(vecm)

vecm.r1 <- cajorls(vecm, r = 1)
summary(vecm.r1$rlm)

vecm.level <- vec2var(vecm, 1)

arch.test(vecm.level)

normality.test(vecm.level)

serial.test(vecm.level)

#Tabla Coeficientes delo modelo VECM======================

str(vec2var(vecm))

vecm.r1$rlm$coefficients

summary(vecm.r1$rlm)$'Response cpib.cri.d'$coefficients[, "t value"]


tab_vecm <- tibble(`Parámetros` = rownames(vecm.r1$rlm$coefficients),
                   cpib.cri = round(vecm.r1$rlm$coefficients[, 1], 3),
                   t1 = round(summary(vecm.r1$rlm)$'Response cpib.cri.d'$coefficients[, "t value"], 2),
                   pvalue1 = summary(vecm.r1$rlm)$'Response cpib.cri.d'$coefficients[, "Pr(>|t|)"],
                   stars1 = ifelse(pvalue1 <= 0.001, " ***", ifelse(pvalue1 <= 0.01, " **", 
                                                                   ifelse(pvalue1 <= 0.05, " *", ifelse(pvalue1 <= 0.1, ".", "")))), 
                  des.sf = round(vecm.r1$rlm$coefficients[, 2], 3), 
                  t2 = round(summary(vecm.r1$rlm)$'Response des.sf.d'$coefficients[, "t value"], 2),
                  pvalue2 = summary(vecm.r1$rlm)$'Response des.sf.d'$coefficients[, "Pr(>|t|)"],
                  stars2 = ifelse(pvalue2 <= 0.001, " ***", ifelse(pvalue2 <= 0.01, " **", 
                                                                   ifelse(pvalue2 <= 0.05, " *", ifelse(pvalue2 <= 0.1, ".", ""))))) %>%  
  mutate(cpib.cri = paste0(paste0(paste0(paste0(cpib.cri, " ("),t1),")"), stars1),
         des.sf = paste0(paste0(paste0(paste0(des.sf, " ("),t2),")"), stars2)) %>% 
  dplyr::select(`Parámetros`, cpib.cri, des.sf) %>% 
  column_to_rownames(var = "Parámetros")

knitr::kable(tab_vecm, booktabs = TRUE, 
             caption = "Coeficientes estimados del modelo VECM (7)",
             label = "vecm",
             format = "latex") %>% 
  kable_styling(font_size = 10) %>% 
  column_spec(1, width = "3cm") %>% 
  column_spec(2, width = "3cm") %>% 
  column_spec(3, width = "3cm") %>% 
  footnote(general = c("Códigos de significancia: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1", 
                       "Estadístico t en paréntesis"))

#Tabla de diagnostico modelo VAR=============
ptm_test_vecm <- serial.test(vecm.level, type = "PT.asymptotic")
arch_vecm <- arch.test(vecm.level, multivariate.only = T)
jb_vecm <- normality.test(vecm.level, multivariate.only = T)

tabla_vecm_tests <- tibble(Prueba = c("Portmanteau", "ARCH", "Jarque-Bera"),
                          `Estadístico` = c(round(ptm_test_vecm$serial$statistic, 2),
                                            round(arch_vecm$arch.mul$statistic, 2), 
                                            round(jb_vecm$jb.mul$JB$statistic, 2)),
                          `Valor p` =  c(round(ptm_test_vecm$serial$p.value, 2), 
                                         round(arch_vecm$arch.mul$p.value, 2), 
                                         round(jb_vecm$jb.mul$JB$p.value, 2)))  

knitr::kable(tabla_vecm_tests, booktabs = TRUE, 
             caption = "Pruebas de diagnóstico del VECM",
             label = "modelo_vecm_pruebas",
             format = "latex") %>% 
  kable_styling(font_size = 11)


#Gráfico VECM impulso-respuesta dsf_crec========================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_irf_dsf_vecm.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

irf1 <- irf(vecm.level,
            impulse = "des.sf",
            response = "cpib.cri",
            n.ahead = 36,
            cumulative = F)  

irf2 <- irf(vec2var(vecm),
            impulse = "des.sf",
            response = "cpib.cri",
            n.ahead = 36,
            cumulative = T)  

vecm_dsf_crec_simple <- tibble(irf = irf1$irf %>% unlist(), 
                               l_inf = irf1$Lower %>% unlist(), 
                               l_sup = irf1$Upper %>% unlist()) %>% 
  mutate(tipo = "Efecto simple", 
         modelo = "mod1", 
         periodo = 0:(nrow(.) - 1))

vecm_dsf_crec_acum <- tibble(irf = irf2$irf %>% unlist(), 
                             l_inf = irf2$Lower %>% unlist(), 
                             l_sup = irf2$Upper %>% unlist()) %>% 
  mutate(tipo = "Efecto acumulado", 
         modelo = "mod1", 
         periodo = 0:(nrow(.) - 1))

vecm_dsf_crec <- bind_rows(vecm_dsf_crec_simple, vecm_dsf_crec_acum)

g_irf_dsf_vecm <- vecm_dsf_crec_simple %>% 
  ggplot(aes(x = periodo, y = irf)) +
  geom_line(size = 0.65, colour = "darkblue") +
  scale_x_continuous(breaks = seq(0, 36, by = 4)) +
  geom_hline(yintercept = 0) +  
  geom_line(aes(x = periodo, y = l_sup), colour = "darkgrey", linetype = "dashed",
            size = 0.65) + 
  geom_line(aes(x = periodo, y = l_inf), colour = "darkgrey", linetype = "dashed", 
            size = 0.65) +
  ylab("Crec. econ.") +
  theme_bw() %+replace%
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black") ,
        axis.title = element_text(size = 6), 
        axis.text=element_text(size = 6), 
        strip.text.y = element_text(size = 7))
        
print(g_irf_dsf_vecm)

dev.off()

setwd(oldwd)

#Gráfico VECM impulso-respuesta crec_dsf========================
td <- "C:/Users/FBrenes/OneDrive - Habitat for Humanity International/Documents/github/Trabajo_Final/Figuras"
tf <- file.path(td,'g_irf_crec_vecm.tex')
oldwd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(td)
tikz(tf, standAlone = FALSE, width = 4, height = 3)

irf1 <- irf(vecm.level,
            impulse = "cpib.cri",
            response = "des.sf",
            n.ahead = 36,
            cumulative = F)  

irf2 <- irf(vec2var(vecm),
            impulse = "cpib.cri",
            response = "des.sf",
            n.ahead = 36,
            cumulative = T)  

vecm_crec_dsf_simple <- tibble(irf = irf1$irf %>% unlist(), 
                               l_inf = irf1$Lower %>% unlist(), 
                               l_sup = irf1$Upper %>% unlist()) %>% 
  mutate(tipo = "Efecto simple", 
         modelo = "mod1", 
         periodo = 0:(nrow(.) - 1))

vecm_crec_dsf_acum <- tibble(irf = irf2$irf %>% unlist(), 
                             l_inf = irf2$Lower %>% unlist(), 
                             l_sup = irf2$Upper %>% unlist()) %>% 
  mutate(tipo = "Efecto acumulado", 
         modelo = "mod1", 
         periodo = 0:(nrow(.) - 1))

vecm_crec_dsf <- bind_rows(vecm_crec_dsf_simple, vecm_crec_dsf_acum)

g_irf_crec_vecm <- vecm_crec_dsf_simple %>% 
  ggplot(aes(x = periodo, y = irf)) +
  geom_line(size = 0.65, colour = "darkblue") +
  scale_x_continuous(breaks = seq(0, 36, by = 4)) +
  geom_hline(yintercept = 0) +  
  geom_line(aes(x = periodo, y = l_sup), colour = "darkgrey", linetype = "dashed",
            size = 0.65) + 
  geom_line(aes(x = periodo, y = l_inf), colour = "darkgrey", linetype = "dashed", 
            size = 0.65) +
  ylab("Desarrollo del s.f.") +
  theme_bw() %+replace%
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black") ,
        axis.title = element_text(size = 6), 
        axis.text=element_text(size = 6), 
        strip.text.y = element_text(size = 7))

print(g_irf_crec_vecm)

dev.off()

setwd(oldwd)

