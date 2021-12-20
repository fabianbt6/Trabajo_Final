
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
library(kableExtra)
library(lmtest)
library(vars)
library(latex2exp)
library(tikzDevice)
library(gtsummary)
library(multDM)
library(cowplot)
library(DT)
library(ggfan)

#Cargar datos====================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file = "../Datos/data.RData")
data
proy_2021 <- read_excel("../Datos/datos.xlsx",
                        sheet = "proyeccion",
                        range = "A1:I125",
                        col_types = c("date",rep("numeric", 8)))

historical_data <- read_excel("../Datos/datos.xlsx",
                              sheet = "proyeccion",
                              range = "J1:S121",
                              col_types = c("date", rep("numeric", 9))) %>% 
  select(fecha, pibr, des.sf)
  
proy_2021$fecha <- as.Date(proy_2021$fecha)
historical_data$fecha <- as.Date(historical_data$fecha)

proy_2021 <- proy_2021[(nrow(proy_2021) - 3):nrow(proy_2021), ]

#Modelos===================
var <- VAR(dplyr::select(data, cpib.cri, des.sf),
           type = "both",
           lag.max = 12, 
           ic = "AIC", 
           exogen = dplyr::select(data, apert_comercial, cpib.usa, inflacion, tbp, vartc, est.d2, est.d3, est.d4))

var_2020 <- VAR(data %>% filter(fecha < "2020-03-01") %>% dplyr::select(cpib.cri, des.sf) ,
                type = "both",
                lag.max = 12,
                ic = "AIC",
                exogen = data %>% filter(fecha < "2020-03-01") %>% dplyr::select(apert_comercial, cpib.usa, inflacion, tbp, vartc, est.d2, est.d3, est.d4))

summary(var)

vecm <- ca.jo(data %>% dplyr::select(cpib.cri, des.sf),
              type = "eigen",
              K = 5, 
              dumvar = dplyr::select(data, apert_comercial, cpib.usa, inflacion, tbp, vartc, est.d2, est.d3, est.d4, tendencia),
              spec = "longrun") 

vecm.level <- vec2var(vecm, 1)

#Proyección 2021 con datos observados======================

escen_base <- bind_cols(tibble(Fecha = c("2021-03-01", "2021-06-01", "2021-09-01", "2021-12-01")), 
                        round(proy_2021[(nrow(proy_2021) - 3):nrow(proy_2021), -c(1, 7, 8, 9)], 4))
colnames(escen_base) <- c("Fecha", "Apertura Comercial", "PIB USA", "Inflación", "TBP", "Tipo de Cambio (var. anual)")

knitr::kable(escen_base, booktabs = TRUE, 
             caption = "Covariables para el escenario base",
             label = "escen_base",
             format = "latex") %>% 
  kable_styling(full_width = F,
                font_size = 7) %>% 
  column_spec(1, width = "10em") %>% 
  column_spec(2, width = "10em") %>% 
  column_spec(3, width = "10em") %>% 
  column_spec(4, width = "10em") %>% 
  column_spec(5, width = "10em") %>%
  column_spec(6, width = "10em") %>% 
  footnote(alphabet = c("Estimado; ") 
  )

pred_base_2021 <- predict(var,
                          dumvar = proy_2021[(nrow(proy_2021) - 3):nrow(proy_2021), -1],
                          n.ahead = 4)

#Crecimiento del PIB
pred_cpib <- proy_2021 %>% filter(fecha > "2020-12-01") %>% select(fecha) %>% 
  bind_cols(pred_base_2021$fcst$cpib.cri %>% as_tibble) %>% 
  select(-CI) %>% 
  bind_rows(tibble(fecha = date("2020-12-01"), 
                   fcst = data %>%
                     select(fecha, cpib.cri) %>% 
                     filter(fecha == "2020-12-01") %>% 
                     select(-fecha) %>% 
                     as.numeric(), 
                   lower = fcst, 
                   upper = fcst))

pred_cpib_long <- pred_cpib %>% 
  pivot_longer(-fecha, names_to = "tipo", values_to = "valor") %>% 
  mutate(fecha.l4 = add_with_rollback(fecha, months(-12)), 
         yr = year(fecha)) %>% 
  left_join(data %>% select(fecha, pibr), 
            by = c("fecha.l4" = "fecha")) %>% 
  mutate(pibr.l4 = pibr) %>% 
  select(-pibr) %>% 
  mutate(pibr = exp(log(pibr.l4) + valor))

cpib_2021 <- pred_cpib_long %>% 
  group_by(yr, tipo) %>% 
  summarize(crec.anual = log(sum(pibr)) - log(sum(pibr.l4)))

#Desarrollo del sistema financiero
pred_des.sf <- proy_2021 %>% 
  filter(fecha > "2020-12-01") %>% 
  select(fecha) %>% 
  bind_cols(pred_base_2021$fcst$des.sf %>% as_tibble) %>%
  select(-CI) %>% 
  bind_rows(tibble(fecha = date("2020-12-01"), 
                   fcst = data %>%
                     select(fecha, des.sf) %>% 
                     filter(fecha == "2020-12-01") %>% 
                     select(-fecha) %>% 
                     as.numeric(), 
                   lower = fcst, 
                   upper = fcst))

#Tabla documento

estimaciones_2021_trim <- tibble(pred_cpib) %>% 
  left_join(pred_des.sf, by = "fecha") %>% 
  pivot_longer(-fecha) %>% 
  mutate(value = round(value, 2)) %>% 
  pivot_wider(fecha)

knitr::kable(estimaciones_2021_trim, booktabs = TRUE, 
             caption = "Estimaciones del modelo VAR para el escenario base" ,
             label = "base_2021",
             format = "latex",
             col.names = c("fecha", rep(c("Estimación", "Lim. inferior", "Lim.superior"), 2))) %>% 
  kable_styling(font_size = 10) %>% 
  add_header_above(c("","Crecimiento Económico" = 3, "Desarrollo del Sistema Financiero" = 3)) 


estimaciones_2021 <- tibble(Variable = c("Crecimiento económico", "Desarrollo del S.F."), 
                            BCCR = c(0.039, NA), 
                            "Estimación" = round(c(as.numeric(cpib_2021[4, 3]), 
                                             as.numeric(pred_des.sf[4, 2])), 4), 
                            "Límite Inferior" = round(c(as.numeric(cpib_2021[5, 3]), 
                                                  as.numeric(pred_des.sf[4, 3])), 4), 
                            "Límite Superior" = round(c(as.numeric(cpib_2021[6, 3]),
                                                  as.numeric(pred_des.sf[4, 4])), 4))

knitr::kable(estimaciones_2021, booktabs = TRUE, 
             caption = "Estimaciones del modelo VAR para el escenario base" ,
             label = "base_2021",
             format = "latex") %>% 
  kable_styling(font_size = 10) %>% 
  add_header_above(c("","", "Modelo VAR" = 4)) 

df_2021 <- tibble("Estimación" = as.numeric(pred_des.sf[4, 2]),
                  "Límite Inferior" = as.numeric(pred_des.sf[4, 3]), 
                  "Límite Superior" = as.numeric(pred_des.sf[4, 4]))

knitr::kable(round(df_2021, 3), booktabs = TRUE, 
             caption = "Estimaciones del desarrollo del S.F. en 2021" ,
             label = "ds_2021",
             format = "latex") %>% 
  kable_styling(font_size = 10)
#Funciones============================

generar_datos_sim1 <- function(variable, q.inf, q.sup) {
  
  nombres <- c("fecha", "apert_comercial", "cpib.usa", "inflacion", "tbp", "vartc",
               "est.d2", "est.d3", "est.d4") 
  var <- sym(variable)
  
  pos <- which(nombres == as_label(var))
  
  mydata <- data %>% dplyr::select(fecha, {{var}}) %>% 
    group_by(quarter(fecha)) %>% 
    summarize(lim.inf = quantile(x = {{var}}, q.inf),
              lim.sup = quantile(x = {{var}}, q.sup), 
              sim = runif(1, lim.inf, lim.sup))
  
  mydata <- bind_cols(proy_2021 %>% select(-{{var}}),
                      mydata %>% select(sim)) %>% 
    relocate(sim, .before = pos)
  
  colnames(mydata) <- nombres
  
  mydata <- mydata %>% select(-fecha)
  
  return(mydata)  
}

generar_datos_sim2 <- function(data, variable, q.inf, q.sup) {
  
  mydata <- data %>% dplyr::select(fecha, {{variable}}) %>% 
    group_by(quarter(fecha)) %>% 
    summarize(lim.inf = quantile(x = {{variable}}, q.inf),
              lim.sup = quantile(x = {{variable}}, q.sup), 
              sim = runif(1, lim.inf, lim.sup))
  
  return(mydata$sim)  
}

generar_determ <- function(fecha1, tendencia1){
  
  tibble(periodo = 1:4,
         fecha = add_with_rollback(fecha1, months(periodo * 3)),
         est.d2 = ifelse(quarter(fecha) == 2, 1, 0),
         est.d3 = ifelse(quarter(fecha) == 3, 1, 0),
         est.d4 = ifelse(quarter(fecha) == 4, 1, 0),
         tendencia = tendencia1 + periodo) %>% 
    select(-c(periodo, fecha))
}

#Simulación 1: Análisis de sensibilidad================== 

#tabla con percentiles de covariables

quantile(data$apert_comercial, seq(0, 1, 0.1))

rng_cuantil <- seq(0, 1, 0.2)

percentil_covar <- data %>% select(apert_comercial, cpib.usa, inflacion, tbp, vartc) %>% 
  summarize(Cuantil = round(rng_cuantil, 2), 
            "Apertura Comercial" = round(quantile(apert_comercial, rng_cuantil), 4), 
            "PIB USA" = round(quantile(cpib.usa, rng_cuantil), 4), 
            "Inflación" = round(quantile(inflacion, rng_cuantil), 4), 
            "TBP" = round(quantile(tbp, rng_cuantil), 2), 
            "Tipo de Cambio (Var. Anual)" = round(quantile(vartc, rng_cuantil), 4))

percentil_covar %>% 
  kbl(booktabs = T, caption = "Cuantiles de las covariables",
      label = "covar_cuant",  format = "latex") %>% 
  kable_paper(full_width = T) %>% 
  kable_styling() %>% 
  column_spec(1, width = "1em") %>%
  column_spec(4, width = "4em") %>% 
  column_spec(5, width = "3em")

ultima_fecha <- data$fecha[nrow(data)]
ultima_tendencia <- var$datamat$trend[nrow(var$datamat)]

set.seed(21921)
n.iter <- 100
#Generación de data simulada
data_sim1 <- tibble(l.inf = rep(rng_cuantil[-length(rng_cuantil)],  n.iter),
                    l.sup = l.inf + 0.2,
                    Rango = paste(round(l.inf, 2), paste("-", round(l.sup, 2))),
                    covariable = list(c("apert_comercial", "cpib.usa", "inflacion", "tbp", "vartc"))) %>%
  unnest(covariable) %>% 
  mutate(iter = 1:n()) %>% 
  mutate(data = pmap(list("variable" = covariable, "q.inf" = l.inf, "q.sup" = l.sup),
                     generar_datos_sim1)) %>%
  mutate(data = pmap(list("variable" = covariable, "q.inf" = l.inf, "q.sup" = l.sup),
                     generar_datos_sim1)) %>%
  group_by(iter) %>% 
  mutate(var_pred = map(data, ~ predict(var, dumvar = ., n.ahead = 4)$fcst)) 

#Predicciones desagregadas
pred_sim1 <- data_sim1 %>% 
  unnest(var_pred) %>% 
  mutate(y = rep(c("cpib.cri", "des.sf"), n() / 2)) %>%  
  unnest(var_pred) %>% 
  mutate(periodo = rep(c(1:4), n() / 4), 
         fecha = add_with_rollback(ultima_fecha, months(periodo * 3)), 
         fecha.l4 = add_with_rollback(fecha, months(-12))) %>% 
  select(-periodo) %>% 
  mutate(estimado = var_pred[, 1], 
         ic.inf = var_pred[, 2], 
         ic.sup = var_pred[, 3]) %>% 
  select(-var_pred) %>% 
  left_join(data %>% select(fecha, pibr), by = c("fecha.l4" = "fecha")) %>% 
  mutate(pibr.l4 = pibr) %>% 
  select(-pibr) %>% 
  mutate(pibr = ifelse(y == "cpib.cri", exp(estimado + log(pibr.l4)), NA), 
         des.sf = ifelse(y == "des.sf", estimado, NA), 
         tipo = "estimado") %>% 
  ungroup()

#Gráfico  
real_data <- data %>% select(fecha, pibr, des.sf) %>% 
  pivot_longer(-fecha, names_to = "y", values_to = "estimado") %>% 
  nest(data = c(fecha, y, estimado)) %>% 
  mutate(covariable = list(c("apert_comercial", "cpib.usa", "inflacion", "tbp", "vartc")), 
         l.inf = list(rng_cuantil[-length(rng_cuantil)])) %>% 
  unnest(l.inf) %>% 
  unnest(covariable) %>% 
  unnest(data) %>% 
  mutate(l.sup = l.inf + 0.2,
         Rango = paste(round(l.inf, 2), paste("-", round(l.sup, 2))), 
         tipo = "real") %>% 
  select(-c(l.inf, l.sup)) %>% 
  relocate(Rango, .after = covariable) 

#Crecimiento interanual del PIB        
pibr_sim1_ggplot <- real_data %>% 
  filter(y == "pibr") %>% 
  mutate(pibr = estimado) %>% 
  select(-estimado) %>% 
  bind_rows(pred_sim1 %>% 
              select(fecha, y, covariable, Rango, tipo, pibr)) %>% 
  drop_na(pibr) %>% 
  filter(fecha > "2015-03-01") %>% 
  ggplot(aes(x = fecha, y = pibr, colour = Rango)) + 
  geom_line() + 
  facet_grid(. ~ covariable)

crec_mat <- pred_sim1 %>% 
  filter(y == "cpib.cri") %>% 
  select(iter, Rango, covariable, fecha, fecha.l4, pibr, pibr.l4) %>% 
  group_by(iter, covariable, Rango) %>% 
  summarize(crec.anual = log(sum(pibr)) - log(sum(pibr.l4))) %>%  
  group_by(Rango, covariable) %>% 
  summarize(mediana = round(median(crec.anual), 4)) %>% 
  pivot_wider(Rango, names_from = covariable, values_from = mediana) %>% 
  ungroup()

crec_colores <- tibble(l.inf = as.numeric(cpib_2021[5, 3]), 
                       l.sup = as.numeric(cpib_2021[6, 3]))

#Tabla para documento
crec_mat %>% 
  kbl(booktabs = T, caption = "Resultados de simulación 1: Medianas del crecimiento del PIB",
      format = "html",
      label = "sensibilidad",
      col.names = c("Rango", "Apertura Comercial", "Crec. PIB USA", 
                    "Inflación", "Tasa de Interés", "Tipo de Cambio (Var. Interanual)")) %>%
  kable_paper(full_width = T) %>% 
  kable_styling() %>% 
  column_spec(1, width = "3em") %>% 
  column_spec(2, width = "10em", background = case_when(crec_mat$apert_comercial > crec_colores$l.sup ~ 'lightgreen',
                                                        crec_mat$apert_comercial < crec_colores$l.inf ~ 'indianred1',
                                                        TRUE ~ 'white')) %>% 
  column_spec(3, width = "10em",  background = case_when(crec_mat$cpib.usa > crec_colores$l.sup ~ 'lightgreen',
                                                         crec_mat$cpib.usa < crec_colores$l.inf ~ 'lightcoral',
                                                         TRUE ~ 'white')) %>% 
  column_spec(4, width = "10em", background = case_when(crec_mat$inflacion > crec_colores$l.sup ~ 'lightgreen',
                                                        crec_mat$inflacion < crec_colores$l.inf ~ 'lightcoral',
                                                        TRUE ~ 'white')) %>% 
  column_spec(5, width = "10em", background = case_when(crec_mat$tbp > crec_colores$l.sup ~ 'lightgreen',
                                                        crec_mat$tbp < crec_colores$l.inf ~ 'lightcoral',
                                                        TRUE ~ 'white')) %>% 
  column_spec(6, width = "10em", background = case_when(crec_mat$vartc > crec_colores$l.sup ~ 'lightgreen',
                                                        crec_mat$vartc < crec_colores$l.inf ~ 'lightcoral',
                                                        TRUE ~ 'white')) %>% 
  footnote(symbol = c(
    "> límite superior estimación del modelo VAR en escenario base (Verde)", 
    "< límite inferior estimación del modelo VAR en escenario base (Rojo)"),  
    general_title = "")

#Desarrollo del sistema financiero

des.sf_mat <- pred_sim1 %>% 
  filter(y == "des.sf") %>% 
  select(iter, Rango, covariable, fecha, des.sf) %>%
  filter(quarter(fecha) == 4) %>% 
  group_by(covariable, Rango) %>% 
  summarize(mediana = round(median(des.sf), 4)) %>% 
  pivot_wider(Rango, names_from = covariable, values_from = mediana) %>% 
  ungroup()

des.sf_colores <- tibble(l.inf = pred_base_2021$fcst$des.sf[4, 2], 
                         l.sup = pred_base_2021$fcst$des.sf[4, 3])

des.sf_mat %>% 
  kbl(booktabs = T, caption = "Medianas del Sistema Financiero", 
      label = "sensibilidad_sf",
      format = "html",
      col.names = c("Rango cuantil", 
                    "Apertura Comercial", 
                    "Crec. PIB USA", 
                    "Inflación", 
                    "Tasa de Interés", 
                    "Tipo de Cambio (Var. Interanual)")) %>%
  kable_paper(full_width = F) %>% 
  kable_styling() %>% 
  column_spec(1, width = "5em") %>% 
  column_spec(2, width = "5em", background = case_when(des.sf_mat$apert_comercial > des.sf_colores$l.sup ~ 'lightgreen',
                                                       des.sf_mat$apert_comercial < des.sf_colores$l.inf ~ 'indianred1',
                                                       TRUE ~ 'white')) %>% 
  column_spec(3, width = "5em", background = case_when(des.sf_mat$cpib.usa > des.sf_colores$l.sup ~ 'lightgreen',
                                                       des.sf_mat$cpib.usa < des.sf_colores$l.inf ~ 'lightcoral',
                                                       TRUE ~ 'white')) %>% 
  column_spec(4, width = "4em", background = case_when(des.sf_mat$inflacion > des.sf_colores$l.sup ~ 'lightgreen',
                                                       des.sf_mat$inflacion < des.sf_colores$l.inf ~ 'lightcoral',
                                                       TRUE ~ 'white')) %>% 
  column_spec(5, width = "4em", background = case_when(des.sf_mat$tbp > des.sf_colores$l.sup ~ 'lightgreen',
                                                       des.sf_mat$tbp < des.sf_colores$l.inf ~ 'lightcoral',
                                                       TRUE ~ 'white')) %>% 
  column_spec(6, width = "8em", background = case_when(des.sf_mat$vartc > des.sf_colores$l.sup ~ 'lightgreen',
                                                       des.sf_mat$vartc < des.sf_colores$l.inf ~ 'lightcoral',
                                                       TRUE ~ 'white')) %>% 
  footnote(symbol = c(
    "> límite superior estimación del modelo VAR en escenario base", 
    "< límite inferior estimación del modelo VAR en escenario base"),  
    general_title = "")

#Simulación 2:Escenarios de estrés===============================

ultima_fecha <- data$fecha[nrow(data)]
ultima_tendencia <- var$datamat$trend[nrow(var$datamat)]

# set.seed(130618)
# data_sim2 <- tibble(iter = 1:100) %>% 
#   mutate(apert_comercial = rep(data %>% select(fecha, apert_comercial) %>% 
#            mutate(año = year(fecha)) %>% 
#            filter(año == 2019) %>% 
#            select(-c(fecha, año)), 100), 
#          cpib.usa = rep(data %>% select(fecha, cpib.usa) %>% 
#            mutate(año = year(fecha)) %>% 
#            filter(año == 2019) %>% 
#            select(-c(fecha, año)), 100),
#          inflacion = map(iter, ~ generar_datos_sim2(data, inflacion, 0.80, 1)),
#          tbp = map(iter, ~ generar_datos_sim2(data, tbp, 0.80, 1)),
#          vartc = map(iter, ~ generar_datos_sim2(data, vartc, 0.80, 1)),
#          determ = map(iter, ~ generar_determ(ultima_fecha, ultima_tendencia))) %>%
#   unnest(-c(iter)) %>% 
#   nest(data_var = apert_comercial:est.d4, 
#        data_vecm = apert_comercial:tendencia)

set.seed(130618)
data_sim2 <- tibble(iter = 1:100) %>% 
  mutate(apert_comercial = rep(escen_base %>% select(`Apertura Comercial`), 100), 
         cpib.usa = rep(escen_base %>% select(`PIB USA`), 100),
         inflacion = map(iter, ~ generar_datos_sim2(data, inflacion, 0.80, 1)),
         tbp = map(iter, ~ generar_datos_sim2(data, tbp, 0.80, 1)),
         vartc = map(iter, ~ generar_datos_sim2(data, vartc, 0.80, 1)),
         determ = map(iter, ~ generar_determ(ultima_fecha, ultima_tendencia))) %>%
  unnest(-c(iter)) %>% 
  nest(data_var = apert_comercial:est.d4, 
       data_vecm = apert_comercial:tendencia)

pred2 <- data_sim2 %>%
  summarize(iter = iter,
            var_pred = map(data_var, ~ predict(var, dumvar = .,
                                               n.ahead = 4)$fcst), 
            vecm_pred = map(data_vecm, ~predict(vecm.level, 
                                                dumvar = as.matrix(.),
                                                n.ahead = 4)$fcst)) %>% 
  unnest(c(var_pred, vecm_pred)) %>% 
  mutate(y = rep(c("cpib.cri", "des.sf"), n() / 2)) %>%  
  unnest(c(var_pred, vecm_pred)) %>% 
  pivot_longer(-c(iter, y), names_sep = "_", names_to = c("mod", "pred")) %>% 
  select(-pred) 

#Desarrollo del Sistema Financiero
sim2_des.sf <- pred2 %>% filter(y == "des.sf") %>% 
  select(-y) %>%
  mutate(periodo = rep(c(rep(1, 2), rep(2, 2), rep(3, 2), rep(4, 2)), n() / 8), 
         fecha = add_with_rollback(ultima_fecha, months(periodo * 3))) %>% 
  select(-periodo) %>% 
  mutate(`estimación puntual` = value[, 1], 
         l.inf = value[, 2], 
         l.sup = value[, 3]) %>% 
  select(-value) %>% 
  pivot_longer(-c(iter, mod, fecha), names_to = "tipo", values_to = "des.sf")

des.sf_data <- data %>% 
  select(fecha, des.sf) %>% 
  mutate(iter = NA,
         mod = "real",
         fecha = fecha) %>% 
  relocate(iter, .before = fecha) %>% 
  bind_rows(sim2_des.sf %>% select(iter, mod, fecha, tipo, des.sf)) %>% 
  mutate(fecha.l4 = add_with_rollback(fecha, months(-12)), 
         año = year(fecha)) %>% 
  left_join(data %>% select(fecha, des.sf) %>% 
              summarize(fecha = fecha, 
                        des.sf.l4 = des.sf), 
            by = c("fecha.l4" = "fecha")) 

ds_proy_sim2 <- des.sf_data %>% 
  filter(mod == "var", 
         tipo == "estimación puntual") %>% 
  group_by(fecha) %>% 
  summarize('máximo' = max(des.sf), 
            'mediana' = median(des.sf), 
            'mínimo' = min(des.sf)) %>% 
  bind_rows(tibble(fecha = date("2020-12-01"), 
                   'máximo' = data %>%
                     select(fecha, des.sf) %>% 
                     filter(fecha == "2020-12-01") %>% 
                     select(-fecha) %>% 
                     as.numeric(), 
                   mediana = `máximo`, 
                   mínimo = `máximo`))

ggplot() + 
  geom_line(data = des.sf_data %>% filter(mod == "real", fecha > "2010-01-01"),
            aes(x = fecha, y = des.sf)) +
  geom_ribbon(data = ds_proy_sim2, aes(x = fecha, ymax = `máximo`, ymin = `mínimo`, fill = "Escenario Estrés")) +
  geom_line(data = pred_des.sf, aes(y = upper, x = fecha, colour = "IC Escenario Base"), linetype = "dashed") +
  geom_line(data = pred_des.sf, aes(y = lower, x = fecha, colour = "IC Escenario Base"), linetype = "dashed") +
  scale_color_manual(values = c(`IC Escenario Base` = "blue")) +
  scale_fill_manual(values = c(`Escenario Estrés` = "grey70")) +
  ylab("Desarrollo del Sist. Financiero") +
  theme_classic() %+replace% 
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.major.y = element_line(color = "grey70",
                                          size = 0.5, 
                                          linetype = 2))

#Tabla
pred_des.sf %>% 
  left_join(ds_proy_sim2) %>% 
  select(-c(`máximo`, `mínimo`)) %>%
  filter(fecha != "2020-12-01") %>% 
  pivot_longer(-fecha) %>% 
  mutate(value = round(value, 4)) %>% 
  pivot_wider(fecha) %>% 
  kbl(booktabs = T, 
      caption = "Estimaciones del Sistema Financiero en el escenario estresado",
    label = "sim2_sf",  
    format = "latex", 
    col.names = c("Fecha", "Estimación", "IC Lim. Superior", "IC Lim. Inferior", "Mediana")) %>% 
  kable_paper(full_width = F) %>% 
  kable_styling(font_size = 8) %>% 
  add_header_above(c("", "Escenario Base" = 3, "Escenario Estresado")) %>% 
  column_spec(1, width = "6em")
  
#Crecimiento económico
sim2_cpib <- pred2 %>% filter(y == "cpib.cri") %>% 
  select(-y) %>%
  mutate(periodo = rep(c(rep(1, 2), rep(2, 2), rep(3, 2), rep(4, 2)), n() / 8), 
         fecha = add_with_rollback(ultima_fecha, months(periodo * 3)), 
         fecha.l4 = add_with_rollback(fecha, months(-12))) %>% 
  left_join(data %>% select(fecha, pibr), by = c("fecha.l4" = "fecha")) %>% 
  select(-periodo) %>% 
  mutate(pibr.l4 = pibr) %>% 
  select(-pibr) %>% 
  mutate(`estimación puntual` = value[, 1], 
         l.inf = value[, 2], 
         l.sup = value[, 3]) %>% 
  select(-value) %>% 
  pivot_longer(-c(iter, mod, fecha, fecha.l4, pibr.l4), 
               names_to = "tipo", values_to = "cpib.cri") %>% 
  mutate(pibr = exp(cpib.cri + log(pibr.l4)))

cpib_data <- data %>% 
  select(fecha, pibr, cpib.cri) %>% 
  mutate(iter = NA,
         mod = "real",
         tipo = NA,
         fecha = fecha,
         pibr = pibr, 
         cpib.cri = cpib.cri) %>% 
  relocate(c(iter, mod), .before = fecha) %>% 
  bind_rows(sim2_cpib %>% select(iter, mod, fecha, tipo, pibr, cpib.cri)) %>% 
  mutate(fecha.l4 = add_with_rollback(fecha, months(-12)), 
         año = year(fecha)) %>% 
  left_join(data %>% select(fecha, pibr) %>% 
              summarize(fecha = fecha, 
                        pibr.l4 = pibr),
            by = c("fecha.l4" = "fecha")) %>% 
  group_by(mod, año, tipo, iter) %>% 
  mutate(cpib.anual = log(sum(pibr)) - log(sum(pibr.l4))) 

cpib_proy_sim2 <- cpib_data %>% 
  filter(mod == "var", 
         tipo == "estimación puntual") %>% 
  group_by(fecha) %>% 
  summarize('máximo' = max(cpib.cri), 
            'mediana' = median(cpib.cri), 
            'mínimo' = min(cpib.cri)) %>% 
  bind_rows(tibble(fecha = date("2020-12-01"), 
                   'máximo' = data %>%
                     select(fecha, cpib.cri) %>% 
                     filter(fecha == "2020-12-01") %>% 
                     select(-fecha) %>% 
                     as.numeric(), 
                   mediana = `máximo`, 
                   mínimo = `máximo`))

cpib_anual_proy_sim2 <- cpib_data %>% 
  filter(mod == "var", 
         tipo == "estimación puntual") %>% 
  group_by(año) %>% 
  summarize('máximo' = max(cpib.anual), 
            'mediana' = median(cpib.anual), 
            'mínimo' = min(cpib.anual)) %>% 
  bind_rows(tibble("año" = 2020, 
                   'máximo' = cpib_data %>%
                     ungroup() %>% 
                     select(fecha, cpib.anual) %>% 
                     filter(fecha == "2020-12-01") %>% 
                     select(-fecha) %>% 
                     as.numeric(), 
                   mediana = `máximo`, 
                   mínimo = `máximo`))

ggplot() + 
  geom_line(data = cpib_data %>% 
              filter(mod == "real", `año` > 2000),
            aes(x = `año`, y = cpib.anual)) +
  geom_ribbon(data = cpib_anual_proy_sim2, aes(x = `año`, ymax = `máximo`, ymin = `mínimo`, fill = "Escenario Estrés")) +
  geom_line(data = cpib_2021 %>% filter(tipo == "upper"), aes(y = crec.anual, x = yr, colour = "IC Escenario Base"), linetype = "dashed") +
  geom_line(data = cpib_2021 %>% filter(tipo == "lower"), aes(y = crec.anual, x = yr, colour = "IC Escenario Base"), linetype = "dashed") +
  scale_color_manual(values = c(`IC Escenario Base` = "blue")) +
  scale_fill_manual(values = c(`Escenario Estrés` = "grey70")) +                  
  ylab("Crecimiento Económico") +
  theme_classic() %+replace% 
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.major.y = element_line(color = "grey70",
                                          size = 0.5, 
                                          linetype = 2))

#Tabla
pred_cpib %>% 
  left_join(cpib_proy_sim2) %>% 
  select(-c(`máximo`, `mínimo`)) %>%
  filter(fecha != "2020-12-01") %>% 
  pivot_longer(-fecha) %>% 
  mutate(value = round(value, 4)) %>% 
  pivot_wider(fecha) %>% 
  kbl(booktabs = T, 
      caption = "Estimaciones del crecimiento económico en el escenario estresado",
      label = "sim2_cpib",  
      format = "latex", 
      col.names = c("Fecha", "Estimación", "IC Lim. Superior", "IC Lim. Inferior", "Mediana")) %>% 
  kable_paper(full_width = F) %>% 
  kable_styling(font_size = 8) %>% 
  add_header_above(c("", "Escenario Base" = 3, "Escenario Estresado")) %>% 
  column_spec(1, width = "6em")

cpib_data %>% 
  filter(mod != "vecm", 
         tipo == "estimación puntual") %>%
  mutate(año = year(fecha)) %>% 
  group_by(año) %>% 
  summarize('mediana cpib.anual' = median(cpib.anual))

#Simulación 3: Escenario estrés para el año 2020=========

ultima_fecha <- date("2019-12-01")
ultima_tendencia <- var_2020$datamat$trend[nrow(var_2020$datamat)] 

set.seed(130618)
data_sim3 <- tibble(iter = 1:100) %>% 
  mutate(apert_comercial = rep(data %>% mutate(yr = year(fecha)) %>%  filter (yr == 2020) %>% select(apert_comercial), 100), 
         cpib.usa = rep(data %>% mutate(yr = year(fecha)) %>%  filter (yr == 2020) %>% select(cpib.usa), 100),
         inflacion = map(iter, ~ generar_datos_sim2(data, inflacion, 0.80, 1)),
         tbp = map(iter, ~ generar_datos_sim2(data, tbp, 0.80, 1)),
         vartc = map(iter, ~ generar_datos_sim2(data, vartc, 0.80, 1)),
         determ = map(iter, ~ generar_determ(ultima_fecha, ultima_tendencia))) %>%
  unnest(-c(iter)) %>% 
  nest(data_var = apert_comercial:est.d4, 
       data_vecm = apert_comercial:tendencia)

pred3 <- data_sim3 %>%
  summarize(iter = iter,
            var_pred = map(data_var, ~ predict(var, dumvar = .,
                                               n.ahead = 4)$fcst), 
            vecm_pred = map(data_vecm, ~predict(vecm.level, 
                                                dumvar = as.matrix(.),
                                                n.ahead = 4)$fcst)) %>% 
  unnest(c(var_pred, vecm_pred)) %>% 
  mutate(y = rep(c("cpib.cri", "des.sf"), n() / 2)) %>%  
  unnest(c(var_pred, vecm_pred)) %>% 
  pivot_longer(-c(iter, y), names_sep = "_", names_to = c("mod", "pred")) %>% 
  select(-pred) 

#Desarrollo del Sistema Financiero
sim3_des.sf <- pred3 %>% filter(y == "des.sf") %>% 
  select(-y) %>%
  mutate(periodo = rep(c(rep(1, 2), rep(2, 2), rep(3, 2), rep(4, 2)), n() / 8), 
         fecha = add_with_rollback(ultima_fecha, months(periodo * 3))) %>% 
  select(-periodo) %>% 
  mutate(`estimación puntual` = value[, 1], 
         l.inf = value[, 2], 
         l.sup = value[, 3]) %>% 
  select(-value) %>% 
  pivot_longer(-c(iter, mod, fecha), names_to = "tipo", values_to = "des.sf")

des.sf_data <- data %>% 
  select(fecha, des.sf) %>% 
  mutate(iter = NA,
         mod = "real",
         fecha = fecha) %>% 
  relocate(iter, .before = fecha) %>% 
  bind_rows(sim3_des.sf %>% select(iter, mod, fecha, tipo, des.sf)) %>% 
  mutate(fecha.l4 = add_with_rollback(fecha, months(-12)), 
         año = year(fecha)) %>% 
  left_join(data %>% select(fecha, des.sf) %>% 
              summarize(fecha = fecha, 
                        des.sf.l4 = des.sf), 
            by = c("fecha.l4" = "fecha")) 

ds_proy_sim3 <- des.sf_data %>% 
  filter(mod == "var", 
         tipo == "estimación puntual") %>% 
  group_by(fecha) %>% 
  summarize('máximo' = max(des.sf), 
            'mediana' = median(des.sf), 
            'mínimo' = min(des.sf)) %>% 
  bind_rows(tibble(fecha = date("2019-12-01"), 
                   'máximo' = data %>%
                     select(fecha, des.sf) %>% 
                     filter(fecha == "2019-12-01") %>% 
                     select(-fecha) %>% 
                     as.numeric(), 
                   mediana = `máximo`, 
                   mínimo = `máximo`))

ggplot() + 
  geom_ribbon(data = ds_proy_sim3, aes(x = fecha, ymax = `máximo`, ymin = `mínimo`, fill = "Escenario Estrés")) +
  geom_line(data = des.sf_data %>% filter(mod == "real", fecha > "2014-01-01"),
            aes(x = fecha, y = des.sf, colour = "Real")) +
  scale_color_manual(values = c(Real = "black")) +
  scale_fill_manual(values = c(`Escenario Estrés` = "grey70")) +
  ylab("Desarrollo del Sist. Financiero") +
  theme_classic() %+replace% 
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.major.y = element_line(color = "grey70",
                                          size = 0.5,
                                          linetype = 2))

#Crecimiento económico
sim3_cpib <- pred3 %>% filter(y == "cpib.cri") %>% 
  select(-y) %>%
  mutate(periodo = rep(c(rep(1, 2), rep(2, 2), rep(3, 2), rep(4, 2)), n() / 8), 
         fecha = add_with_rollback(ultima_fecha, months(periodo * 3)), 
         fecha.l4 = add_with_rollback(fecha, months(-12))) %>% 
  left_join(data %>% select(fecha, pibr), by = c("fecha.l4" = "fecha")) %>% 
  select(-periodo) %>% 
  mutate(pibr.l4 = pibr) %>% 
  select(-pibr) %>% 
  mutate(`estimación puntual` = value[, 1], 
         l.inf = value[, 2], 
         l.sup = value[, 3]) %>% 
  select(-value) %>% 
  pivot_longer(-c(iter, mod, fecha, fecha.l4, pibr.l4), 
               names_to = "tipo", values_to = "cpib.cri") %>% 
  mutate(pibr = exp(cpib.cri + log(pibr.l4)))

cpib_data <- data %>% 
  select(fecha, pibr, cpib.cri) %>% 
  mutate(iter = NA,
         mod = "real",
         tipo = NA,
         fecha = fecha,
         pibr = pibr, 
         cpib.cri = cpib.cri) %>% 
  relocate(c(iter, mod), .before = fecha) %>% 
  bind_rows(sim3_cpib %>% select(iter, mod, fecha, tipo, pibr, cpib.cri)) %>% 
  mutate(fecha.l4 = add_with_rollback(fecha, months(-12)), 
         año = year(fecha)) %>% 
  left_join(data %>% select(fecha, pibr) %>% 
              summarize(fecha = fecha, 
                        pibr.l4 = pibr),
            by = c("fecha.l4" = "fecha")) %>% 
  group_by(mod, año, tipo, iter) %>% 
  mutate(cpib.anual = log(sum(pibr)) - log(sum(pibr.l4))) 

cpib_proy_sim3 <- cpib_data %>% 
  filter(mod == "var", 
         tipo == "estimación puntual") %>% 
  group_by(fecha) %>% 
  summarize('máximo' = max(cpib.cri), 
            'mediana' = median(cpib.cri), 
            'mínimo' = min(cpib.cri)) %>% 
  bind_rows(tibble(fecha = date("2019-12-01"), 
                   'máximo' = data %>%
                     select(fecha, cpib.cri) %>% 
                     filter(fecha == "2019-12-01") %>% 
                     select(-fecha) %>% 
                     as.numeric(), 
                   mediana = `máximo`, 
                   mínimo = `máximo`))

cpib_anual_proy_sim3 <- cpib_data %>% 
  filter(mod == "var", 
         tipo == "estimación puntual") %>% 
  group_by(año) %>% 
  summarize('máximo' = max(cpib.anual), 
            'mediana' = median(cpib.anual), 
            'mínimo' = min(cpib.anual)) %>% 
  bind_rows(tibble("año" = 2019, 
                   'máximo' = cpib_data %>%
                     ungroup() %>% 
                     select(fecha, cpib.anual) %>% 
                     filter(fecha == "2019-12-01") %>% 
                     select(-fecha) %>% 
                     as.numeric(), 
                   mediana = `máximo`, 
                   mínimo = `máximo`))

ggplot() + 
  geom_line(data = cpib_data %>% 
              filter(mod == "real", `año` > 2011),
            aes(x = `año`, y = cpib.anual, colour = "Real")) +
  geom_ribbon(data = cpib_anual_proy_sim3, aes(x = `año`, ymax = `máximo`, ymin = `mínimo`, fill = "Escenario Estrés"), alpha = 1 / 2) +
  scale_color_manual(values = c(Real = "black")) +
  scale_fill_manual(values = c(`Escenario Estrés` = "black")) +                  
  ylab("Crecimiento Económico") +
  theme_classic() %+replace% 
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.major.y = element_line(color = "grey70",
                                          size = 0.5,
                                          linetype = 2))

#Trimestral
ggplot() + 
  geom_line(data = cpib_data %>% 
              filter(mod == "real", `año` > 2000),
            aes(x = fecha, y = cpib.cri, colour = "Real")) +
  geom_ribbon(data = cpib_proy_sim3, aes(x = fecha, ymax = `máximo`, ymin = `mínimo`, fill = "Escenario Estrés")) +
  scale_color_manual(values = c(Real = "black")) +
  scale_fill_manual(values = c(`Escenario Estrés` = "grey70")) +                  
  ylab("Crecimiento Económico") +
  theme_bw() %+replace% 
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.minor.x = element_blank())

cpib_data %>% 
  filter(mod != "vecm", 
         tipo == "estimación puntual") %>%
  mutate(año = year(fecha)) %>% 
  group_by(año) %>% 
  summarize('mediana cpib.anual' = median(cpib.anual))

#Tabla consolidada
data %>%
  mutate(yr = year(fecha)) %>%
  filter(yr == 2020) %>% 
  select(fecha, des.sf, cpib.cri) %>% 
  left_join(ds_proy_sim3) %>% 
  mutate(D.S. = mediana) %>% 
  select(-c(`mediana`, `máximo`, `mínimo`)) %>%
  left_join(cpib_proy_sim3) %>% 
  mutate(cpib.real = mediana) %>% 
  select(-c(`mediana`, `máximo`, `mínimo`)) %>%
  pivot_longer(-fecha) %>% 
  mutate(value = round(value, 4)) %>% 
  pivot_wider(fecha) %>% 
  relocate(c(cpib.cri, cpib.real), .after = fecha) %>% 
  kbl(booktabs = T, 
      caption = "Real vs escenario estresado para 2020",
      label = "sim3",  
      format = "html", 
      col.names = c("Fecha", "Real", "Simulaciones (Mediana)", "Real", "Simulaciones (Mediana)")) %>% 
  kable_paper(full_width = F) %>% 
  kable_styling(font_size = 8) %>% 
  column_spec(1, width = "6em") %>% 
  column_spec(2, width = "6em") %>% 
  column_spec(3, width = "6em") %>% 
  column_spec(4, width = "6em") %>% 
  column_spec(5, width = "6em") %>% 
  add_header_above(c("", "Crecimiento Económico" = 2, "Desarrollo del S.F." = 2)) 


#Pruebas para presentación=================

tab_descriptivos <- function(mydata, var, dgts) {
  
  var <- enquo(var)
  tab <- mydata %>% 
    summarize(indicador = 1,
              'Mínimo' = min(!!var), 
              'Máximo' = max(!!var), 
              Promedio = mean(!!var),
              'Percentil 25' = quantile(!!var, 0.25), 
              'Percentil 50' = quantile(!!var, 0.50),
              'Percentil 75' = quantile(!!var, 0.75),
              'Desviación Estándar' = sd(!!var), 
              'Coeficiente de Variación' =  Promedio / sd(!!var)) %>% 
    pivot_longer(-indicador, names_to = "Indicador", "Valor") %>% 
    select(-indicador)
  

  kable(tab, digits = dgts,  format = 'html', 
   caption = "Estadísticas Descriptivas") %>% 
    kable_styling(bootstrap_options = 'basic', font_size = 10)
  
}

tab_descriptivos(data, log_pibryoy)

tab <- data %>% 
  summarize(indicador = "var",
            'Mínimo' = min(log_pibryoy), 
            'Máximo' = max(log_pibryoy), 
            Promedio = mean(log_pibryoy),
            'Percentil 25' = quantile(log_pibryoy, 0.25), 
            'Percentil 50' = quantile(log_pibryoy, 0.50),
            'Percentil 75' = quantile(log_pibryoy, 0.75),
            'Desviación Estándar' = sd(log_pibryoy), 
            'Coeficiente de Variación' =  Promedio / sd(log_pibryoy)) %>% 
  pivot_longer(-indicador, names_to = "Indicador", "Valor") %>% 
  select(-indicador)
