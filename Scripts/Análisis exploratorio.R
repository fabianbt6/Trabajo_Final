
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

#Funciones estadísticas descriptivas===============
est_descriptivas <- function(mydata, var) {
  
  var <- enquo(var)
  
  mydata %>% 
    select(!!var) %>% 
    na.omit() %>% 
    summarize(id = "var", 
              min = min(!!var),
              perc_25 = quantile(!!var, 0.25), 
              mediana = quantile(!!var, 0.50),
              media = mean(!!var),
              perc_75 = quantile(!!var, 0.75),
              max = max(!!var)) %>% 
    pivot_longer(-id, names_to = "Estadístico", values_to = "Valor") %>% 
    select(-id)
}

tab_outliers <- function(mydata, fecha, var) {
  
  fecha <- enquo(fecha)
  var <- enquo(var)
  
  mydata %>% 
    select(!!fecha, !!var) %>% 
    na.omit() %>% 
    mutate(Q1 = quantile(!!var, 0.25), 
           Q3 = quantile(!!var, 0.75), 
           IQ = Q3 - Q1, 
           lim_inf = Q1 - 1.5 * IQ, 
           lim_sup = Q3 + 1.5 * IQ, 
           outlier = ifelse(!!var < lim_inf | !!var > lim_sup, T, F)) 
}

#funciones graficos==================
graf_lin <- function(mydata, fecha, var, tit, lab) {
  
  mydata %>% 
    dplyr::select(!!fecha, !!var) %>% 
    column_to_rownames(fecha) %>% as.xts() %>% 
    dygraph(main = tit) %>% 
    dyAxis("x", drawGrid = FALSE) %>% 
    dySeries(var, label = lab) 
  
}

#cargar datos======================================================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file = "../Datos/data_original.Rdata")
load(file = "../Datos/data.Rdata")

#Análisis bivariado entre variables de interés=====================

data$flag <- ifelse(data$fecha >= as_date("2008-09-01") & data$fecha < as_date("2009-12-01"), "1", 
                    ifelse(year(data$fecha) == 2020, "1", "0"))

ggplot(data, aes(x = pibr, y = cred_pib,)) + 
  ylab("Crédito al Sector Privado (% del PIBn)") + 
  xlab("PIB real (colones encadenados)") +
  ggtitle("Desarrollo del Sistema Financiero vs Crecimiento Económico") + 
  geom_smooth(method = "lm", se = F, color = "grey", size = 0.5) +
  geom_point(aes(color = flag)) + 
  theme_bw() %+replace% 
    theme(panel.background = element_rect(fill = NA), legend.position = "none")
mod_lm <- lm(pibr ~ cred_pib, data)

mydata <- tibble(fecha = data$fecha, 
                 residuos = scale(mod_lm$residuals),
                 flag = ifelse(abs(residuos)> 2, "1", "0"))

mydata %>% 
  ggplot(aes(y = residuos, x = fecha)) + geom_point(aes(color = flag))+ 
  geom_hline(yintercept = 2, linetype = "dashed") +
  geom_hline(yintercept = -2, linetype = "dashed") + 
  theme_minimal()

graf_lin(data, "fecha", "pibr",
         "PIB Real de Costa Rica",
         "Millones de colones encadenados")

#Análisis estadístico variables de control =========================

names(data)
vars <- c("pibr", "cred_pib")

vars_control <- c("ied", "inflacion", "tbp", "mif", "itcr")
dummies <- c("d_2Q20")

nombres_vars_control <- c("Inversión Extranjera Directa",
                          "Inflación",
                          "Tasa Básica Pasiva")

vars_control_unidades <- c("Millones de USD",
                           "%",
                           "%")

data_long <- data %>% 
  dplyr::select(fecha, c(vars, vars_control)) %>% 
  pivot_longer(-fecha, "Indicador", values_to = "valor")

tab_vars_control <- data_long %>% 
  filter(Indicador %in% vars_control) %>% 
  group_by(Indicador) %>% 
  summarise(Promedio = round(mean(valor), 1), 
            Mínimo  = round(min(valor), 1), 
            Máximo = round(max(valor), 1), 
            `Desviación Estándar` = round(sd(valor), 1), 
            `Coeficiente de Variación`  = `Desviación Estándar` / Promedio) %>% 
  ungroup() %>% 
  mutate(`Nombre Completo` = nombres_vars_control, 
         `Unidad` = vars_control_unidades) %>% 
  relocate(`Nombre Completo`, Unidad, .before = Promedio)

tab_vars_control %>% 
  kbl() %>% 
  kable_classic(full_width = F) %>% 
  add_header_above(c(" " = 3, "Estadísticas Descriptivas" = 5))

data_long %>% 
  filter(Indicador != "cred_pib", Indicador != "pibr") %>% 
  ggplot(aes(x = fecha, y = valor)) + geom_line() + facet_wrap(~Indicador, scales = "free")

data %>% 
  select(fecha, pibr, vars_control, pibr_yoy) %>% 
  pivot_longer(-fecha, "Indicador", values_to = "valor")  %>% 
  filter(Indicador != "cred_pib") %>% 
  pivot_wider(names_from = "Indicador", values_from = "valor") %>% 
  pivot_longer(-c(fecha, pibr, pibr_yoy), names_to = "Indicador", values_to = "valor") %>% 
  mutate(`Desempeño Económico` = ifelse(pibr_yoy < 0,
                                        "Contracción",
                                        "Crecimiento"))  %>% 
  ggplot(aes(x = pibr, y = valor)) + 
  geom_point(aes(color = `Desempeño Económico`)) + 
  geom_smooth(method = "lm", se = F, color = "grey", size = 0.5) +
  facet_wrap(~Indicador, scales = "free") + 
  theme_minimal() %+replace% 
    theme(legend.position = "bottom")
    
#Regresión==============

vars <- c("pibr", "cred_pib")
vars_control <- c("apert_comercial", "ied", "inflacion", "tbp", "mif", "itcr")
vars_control <- c("exp", "ied", "inflacion", "tbp")
dummies <- c()

formula_mod_lineal_pib <- paste0("pibr ~ cred_pib+", paste0(c(vars_control, dummies), "+", collapse = "")) 
formula_mod_lineal_pib <- str_sub(formula_mod_lineal_pib, 1, 
                                  str_length(formula_mod_lineal_pib) - 1)

formula_mod_lineal_dsf <- paste0("cred_pib ~ pibr +", paste0(c(vars_control, dummies), "+", collapse = "")) 
formula_mod_lineal_dsf <- str_sub(formula_mod_lineal_dsf, 1, 
                                  str_length(formula_mod_lineal_dsf) - 1)

#Variable dependiente = PIBr=================================
mod_lm_pib <- lm(formula_mod_lineal_pib, data = data)
summary(mod_lm_pib)

data %>% 
  dplyr::select(pibr, cred_pib, vars_control) %>% 
  cor()

#Multicolinealidad  
vif(mod_lm_pib)

#Heterocedasticidad
bptest(mod_lm_pib)

# Autocorrelación en los residuos
bgtest(mod_lm_pib)

#Normalidad de los residuos
tibble(`residuos estandarizados` = scale(mod_lm_pib$residuals)) %>% 
  ggplot(aes(x = `residuos estandarizados`)) + geom_histogram(bins = 25)

shapiro.test(mod_lm_pib$residuals)

#Variable dependiente = Desarrollo del Sistema Financiero=================================
mod_lm_dsf <- lm(formula_mod_lineal_dsf, data = data)
summary(mod_lm_dsf)

data %>% 
  dplyr::select(pibr, cred_pib, vars_control) %>% 
  cor()

#Multicolinealidad  
vif(mod_lm_dsf)

#Heterocedasticidad
bptest(mod_lm_dsf)

# Autocorrelación en los residuos
bgtest(mod_lm_dsf)

#Normalidad de los residuos

tibble(`residuos estandarizados` = scale(mod_lm_dsf$residuals)) %>% 
  ggplot(aes(x = `residuos estandarizados`)) + geom_histogram(bins = 25)

shapiro.test(mod_lm_dsf$residuals)

res_data <- data.frame(fecha = data$fecha,
                   res_estandarizados = scale(mod_lm_dsf$res)) %>% 
  as_tibble() %>% 
  mutate(flag = ifelse(abs(res_estandarizados)> 2, 1, 0))



