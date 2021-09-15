
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

vecm <- ca.jo(data %>% dplyr::select(cpib.cri, des.sf),
              type = "eigen",
              K = 5, 
              dumvar = dplyr::select(data, apert_comercial, cpib.usa, inflacion, tbp, vartc, est.d2, est.d3, est.d4, tendencia),
              spec = "longrun") 

vecm.level <- vec2var(vecm, 1)

#Proyección 2021 con datos observados======================

pred_base_2021 <- predict(var,
                          dumvar = proy_2021[(nrow(proy_2021) - 3):nrow(proy_2021), -1],
                          n.ahead = 4)

#Crecimiento del PIB
pred_cpib <- pred_base_2021 %>% filter(fecha > "2020-12-01") %>% select(fecha) %>% 
  bind_cols(pred_base_2021$fcst$cpib.cri %>% as_tibble) 

pred1_cpib_long <- pred1_cpib %>% select(-CI) %>% 
  pivot_longer(-fecha, names_to = "tipo", values_to = "valor") %>% 
  mutate(fecha.l4 = add_with_rollback(fecha, months(-12)), 
         yr = year(fecha)) %>% 
  left_join(data %>% select(fecha, pibr), 
            by = c("fecha.l4" = "fecha")) %>% 
  mutate(pibr.l4 = pibr) %>% 
  select(-pibr) %>% 
  mutate(pibr = exp(log(pibr.l4) + valor))

pred1_cpib_long %>% 
  group_by(yr, tipo) %>% 
  summarize(crec.anual = log(sum(pibr)) - log(sum(pibr.l4)))
  
ggplot() +
  geom_line(data = data, aes(x = fecha, y = cpib.cri)) + 
  geom_line(data = pred1_cpib, aes(x = fecha, y = fcst)) +
  geom_ribbon(data = pred1_cpib, aes(x = fecha, y = fcst, 
                                     ymin = lower, ymax = upper), 
              alpha = 1 / 3) +
  theme_bw() %+replace% 
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.minor.x = element_blank())

#Desarrollo del sistema financiero
pred1_des.sf <- proy_2021 %>% filter(fecha > "2020-12-01") %>% select(fecha) %>% 
  bind_cols(pred1_2021$fcst$des.sf %>% as_tibble) 

ggplot() +
  geom_line(data = data, aes(x = fecha, y = des.sf)) + 
  geom_line(data = pred1_des.sf, aes(x = fecha, y = fcst)) +
  geom_ribbon(data = pred1_des.sf, aes(x = fecha, y = fcst, 
                                     ymin = lower, ymax = upper), 
              alpha = 1 / 3) +
  theme_bw() %+replace% 
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.minor.x = element_blank())

#Funciones============================
generar_datos_sim1 <- function(data, variable, q.inf, q.sup) {
  
  mydata <- data %>% dplyr::select(fecha, {{variable}}) %>% 
    group_by(quarter(fecha)) %>% 
    summarize(lim.inf = quantile(x = {{variable}}, q.inf),
              lim.sup = quantile(x = {{variable}}, q.sup), 
              sim = runif(1, lim.inf, lim.sup))
  
  return(mydata$sim)  
}

generar_datos_sim2 <- function(variable, q.inf, q.sup) {
  
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

generar_determ <- function(fecha1, tendencia1){
  
  tibble(periodo = 1:4,
         fecha = add_with_rollback(fecha1, months(periodo * 3)),
         est.d2 = ifelse(quarter(fecha) == 2, 1, 0),
         est.d3 = ifelse(quarter(fecha) == 3, 1, 0),
         est.d4 = ifelse(quarter(fecha) == 4, 1, 0),
         tendencia = tendencia1 + periodo) %>% 
    select(-c(periodo, fecha))
}

#Simulación 1:Escenarios de estrés===============================

ultima_fecha <- data$fecha[nrow(data)]
ultima_tendencia <- data$tendencia[nrow(data)]

data_sim <- tibble(iter = 1:10) %>% 
  mutate(apert_comercial = map(iter, ~ generar_datos_sim1(data, apert_comercial, 0.40, 0.55)), 
         cpib.usa = map(iter, ~ generar_datos_sim1(data, cpib.usa, 0.01, 0.05)),
         inflacion = map(iter, ~ generar_datos_sim1(data, inflacion, 0.75, 0.85)),
         tbp = map(iter, ~ generar_datos_sim1(data, tbp, 0.85, 0.90)),
         vartc = map(iter, ~ generar_datos_sim1(data, vartc, 0.85, 0.90)),
         determ = map(iter, ~ generar_determ(ultima_fecha, ultima_tendencia))) %>%
  unnest(-c(iter)) %>% 
  nest(data_var = apert_comercial:est.d4, 
       data_vecm = apert_comercial:tendencia)

pred <- data_sim %>%
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

#Graficos==============================================

pred_des.sf <- pred %>% filter(y == "des.sf") %>% 
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
  bind_rows(pred_des.sf %>% select(iter, mod, fecha, tipo, des.sf)) %>% 
  mutate(fecha.l4 = add_with_rollback(fecha, months(-12)), 
         año = year(fecha)) %>% 
  left_join(data %>% select(fecha, des.sf) %>% 
              summarize(fecha = fecha, 
                        des.sf.l4 = des.sf), 
            by = c("fecha.l4" = "fecha")) 
ggplot() + 
  geom_line(data = des.sf_data %>% filter(mod == "real"),
            aes(x = fecha, y = des.sf)) +
  geom_boxplot(data = des.sf_data %>% filter(mod == "var",
                                       tipo == "estimación puntual"), 
               aes(x = fecha, y = des.sf, group = fecha))

pred_cpib <- pred %>% filter(y == "cpib.cri") %>% 
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
  bind_rows(pred_cpib %>% select(iter, mod, fecha, tipo, pibr, cpib.cri)) %>% 
  mutate(fecha.l4 = add_with_rollback(fecha, months(-12)), 
         año = year(fecha)) %>% 
  left_join(data %>% select(fecha, pibr) %>% 
              summarize(fecha = fecha, 
                        pibr.l4 = pibr),
            by = c("fecha.l4" = "fecha")) %>% 
  group_by(mod, año, tipo, iter) %>% 
  mutate(cpib.anual = log(sum(pibr)) - log(sum(pibr.l4))) 

ggplot() + 
  geom_line(data = cpib_data %>% filter(mod == "real"),
            aes(x = fecha, y = cpib.cri)) +
  geom_boxplot(data = cpib_data %>% filter(mod == "var", 
                                             tipo == "estimación puntual"), 
               aes(x = fecha, y = cpib.cri, group = fecha)) +
  ylab("Crecimiento anual") +
  ggtitle("Resultado de las Simulaciones - Crecimiento económico (VAR (4))") +
  theme_bw() %+replace% 
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.minor.x = element_blank())
  
knitr::kable(pred_cpib, booktabs = TRUE, 
             caption = "Mediana de las estimaciones",
             format = "html") %>% 
  kable_styling(latex_options = "scale_down", full_width = F,
                font_size = 10)

med_cpib <- pred_cpib %>%
  filter(tipo == "estimación puntual") %>%
  mutate(Modelo = mod) %>% 
  group_by(Modelo) %>% 
  summarize(`Crecimiento Económico` = round(median(cpibr.anual.estimado), 4))

med_dessf <- pred_sf %>% 
  filter(tipo == "estimación puntual") %>% 
  mutate(Modelo = mod) %>% 
  group_by(Modelo) %>% 
  summarize(Desarrollo.SF = round(median(valor), 4))

medianas <- left_join(med_cpib, med_dessf)

#Simulación 2: Análisis de sensibilidad================== 

ultima_fecha <- data$fecha[nrow(data)]
ultima_tendencia <- data$tendencia[nrow(data)]

n.iter <- 10
#Generación de data simulada
data_sim2 <- tibble(l.inf = rep(seq(0, 0.9, 0.1),  n.iter),
                    l.sup = l.inf + 0.1,
                    decil = round(l.sup * 10, 0),
                    covariable = list(c("apert_comercial", "cpib.usa", "inflacion", "tbp", "vartc"))) %>%
  unnest(covariable) %>% 
  mutate(iter = 1:n()) %>% 
  mutate(data = pmap(list("variable" = covariable, "q.inf" = l.inf, "q.sup" = l.sup),
                     generar_datos_sim2)) %>%
  mutate(data = pmap(list("variable" = covariable, "q.inf" = l.inf, "q.sup" = l.sup),
                     generar_datos_sim2)) %>%
  group_by(iter) %>% 
  mutate(var_pred = map(data, ~ predict(var, dumvar = ., n.ahead = 4)$fcst)) 

#Predicciones desagregadas
pred_sim2 <- data_sim2 %>% 
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
         decil = list(1:10)) %>% 
  unnest(decil) %>% 
  unnest(covariable) %>% 
  unnest(data) %>% 
  mutate(tipo = "real") %>% 
  relocate(estimado, .after = tipo) 

#Crecimiento interanual del PIB        
pibr_sim2_ggplot <- real_data %>% 
  filter(y == "pibr") %>% 
  mutate(pibr = estimado, 
         decil.fct = as_factor(decil)) %>% 
  select(-estimado) %>% 
  bind_rows(pred_sim2 %>% 
              select(fecha, y, covariable, decil, tipo, pibr)) %>% 
  drop_na(pibr) %>% 
  filter(tipo == "estimado") %>% 
  ggplot(aes(x = decil, y = pibr, colour = decil)) + 
  geom_boxplot() + 
  facet_grid(. ~ covariable)

crec_mat <-pred_sim2 %>% 
  filter(y == "cpib.cri") %>% 
  select(iter, decil, covariable, fecha, fecha.l4, pibr, pibr.l4) %>% 
  group_by(iter, covariable, decil) %>% 
  summarize(crec.anual = log(sum(pibr)) - log(sum(pibr.l4))) %>%  
  group_by(decil, covariable) %>% 
  summarize(mediana = round(median(crec.anual), 4)) %>% 
  pivot_wider(decil, names_from = covariable, values_from = mediana) %>% 
  ungroup()

crec_colores <- historical_data %>% select(fecha, pibr) %>% 
  mutate(fecha.l4 = add_with_rollback(fecha, months(-12)), 
         yr = year(fecha)) %>% 
  left_join(data %>% select(fecha, pibr), by = c("fecha.l4" = "fecha")) %>% 
  drop_na()  %>% 
  group_by(yr) %>% 
  summarize(crec.anual  = log(sum(pibr.x)) - log(sum(pibr.y))) %>% 
  summarize(id = 1, 
            p10 = quantile(crec.anual, 0.10), 
            p90 = quantile(crec.anual, 0.90)) %>%  
  pivot_longer(-id, names_to = "percentil", values_to = "valor") %>% 
  select(-id)

crec_mat %>% 
  kbl(booktabs = T, caption = "Crecimiento del PIB",
      col.names = c("decil", "Apertura Comercial", "Crec. PIB USA", 
                    "Inflación", "Tasa de Interés", "Tipo de Cambio (Var. Interanual)")) %>%
  kable_paper(full_width = F) %>% 
  kable_styling() %>% 
  column_spec(2, width = "10cm", background = case_when(crec_mat$apert_comercial > crec_colores$valor[2] ~ 'lightgreen',
                                                        crec_mat$apert_comercial < crec_colores$valor[1] ~ 'indianred1',
                                        TRUE ~ 'white')) %>% 
  column_spec(3, width = "10cm",  background = case_when(crec_mat$cpib.usa > crec_colores$valor[2] ~ 'lightgreen',
                                                         crec_mat$cpib.usa < crec_colores$valor[1] ~ 'lightcoral',
                                        TRUE ~ 'white')) %>% 
  column_spec(4, width = "10cm", background = case_when(crec_mat$inflacion > crec_colores$valor[2] ~ 'lightgreen',
                                                        crec_mat$inflacion < crec_colores$valor[1] ~ 'lightcoral',
                                        TRUE ~ 'white')) %>% 
  column_spec(5, width = "10cm", background = case_when(crec_mat$tbp > crec_colores$valor[2] ~ 'lightgreen',
                                                        crec_mat$tbp < crec_colores$valor[1] ~ 'lightcoral',
                                        TRUE ~ 'white')) %>% 
  column_spec(6, width = "10cm", background = case_when(crec_mat$vartc > crec_colores$valor[2] ~ 'lightgreen',
                                                        crec_mat$vartc < crec_colores$valor[1] ~ 'lightcoral',
                                        TRUE ~ 'white')) %>% 
  footnote(general = 
             "Celdas en verde: crecimiento > percentil 90 de los datos histórico \
              Celdas en rojo: crecimiento < percentil 10 de los datos histórico", 
           general_title = "")

#Desarrollo del sistema financiero

des.sf_mat <- pred_sim2 %>% 
  filter(y == "des.sf") %>% 
  select(iter, decil, covariable, fecha, des.sf) %>%
  filter(quarter(fecha) == 4) %>% 
  group_by(covariable, decil) %>% 
  summarize(mediana = round(median(des.sf), 4)) %>% 
  pivot_wider(decil, names_from = covariable, values_from = mediana) %>% 
  ungroup()

des.sf_colores <- tibble(l.inf = pred_base_2021$fcst$des.sf[4, 2], 
                         l.sup = pred_base_2021$fcst$des.sf[4, 3])

des.sf_mat %>% 
  kbl(booktabs = T, caption = "Análisis de sensibilidad: Sistema Financiero", 
      col.names = c("decil", 
                    "Apertura Comercial", 
                    "Crec. PIB USA", 
                    "Inflación", 
                    "Tasa de Interés", 
                    "Tipo de Cambio (Var. Interanual)")) %>%
  kable_paper(full_width = F) %>% 
  kable_styling() %>% 
  column_spec(2, width = "10cm", background = case_when(des.sf_mat$apert_comercial > des.sf_colores$l.sup ~ 'lightgreen',
                                        des.sf_mat$apert_comercial < des.sf_colores$l.inf ~ 'indianred1',
                                        TRUE ~ 'white')) %>% 
  column_spec(3, width = "10cm", background = case_when(des.sf_mat$cpib.usa > des.sf_colores$l.sup ~ 'lightgreen',
                                        des.sf_mat$cpib.usa < des.sf_colores$l.inf ~ 'lightcoral',
                                        TRUE ~ 'white')) %>% 
  column_spec(4, width = "10cm", background = case_when(des.sf_mat$inflacion > des.sf_colores$l.sup ~ 'lightgreen',
                                        des.sf_mat$inflacion < des.sf_colores$l.inf ~ 'lightcoral',
                                        TRUE ~ 'white')) %>% 
  column_spec(5, width = "10cm", background = case_when(des.sf_mat$tbp > des.sf_colores$l.sup ~ 'lightgreen',
                                        des.sf_mat$tbp < des.sf_colores$l.inf ~ 'lightcoral',
                                        TRUE ~ 'white')) %>% 
  column_spec(6, width = "10cm", background = case_when(des.sf_mat$vartc > des.sf_colores$l.sup ~ 'lightgreen',
                                        des.sf_mat$vartc < des.sf_colores$l.inf ~ 'lightcoral',
                                        TRUE ~ 'white')) %>% 
  footnote(general = 
             "Celdas en verde: Valor > límite superior estimación del modelo VAR en escenario base \
              Celdas en rojo: Valor < límite inferior estimación del modelo VAR en escenario base", 
           general_title = "")

  

  
  
  
