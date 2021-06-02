
rm(list = ls())

#Libs===================
library(tidyverse)
library(readxl)
library(lubridate)
library(urca)
library(zoo)
library(car)
library(lmtest)
library(vars)
library(broom)
library(ggiraph)
library(urca)

#Cargar datos===========
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file = "../Datos/data.RData")
load(file = "../Datos/data_original.RData")
load(file = "../Datos/data_diff.RData")
load(file = "../Datos/ts_data.RData")
load(file = "../Datos/covariables_ts_data.RData")

# data_diff <- data %>%
#   mutate(across(-fecha,  .fns =  ~ . - lag(.), .names = "{.col}_diff")) %>%
#   na.omit()

#save(data_diff, file = "../Datos/data_diff.RData")

#Crear variables============
#Función para crear variable ts
convert_ts <- function(data, fecha, var) {
  
  fecha <- enquo(fecha)
  var <- enquo(var)
  
  var_ts <- data %>% dplyr::select(fecha, !!var) 
  var_ts <- ts(var_ts %>% dplyr::select(!!var), 
               start = c(year(var_ts$fecha[1]), quarter(var_ts$fecha[1])), 
               end = c(year(var_ts$fecha[nrow(var_ts)]),
                       quarter(var_ts$fecha[nrow(var_ts)])), 
               frequency = 4)
}

pibryoy_ts <- convert_ts(data, "fecha", "pibr_yoy")
ied_ts <- convert_ts(data, "fecha", "ied")
ied_pibryoy_ts <- convert_ts(data, "fecha", "ied_pib")
inflacion_ts <- convert_ts(data, "fecha", "inflacion")
inflacion7_ts <- convert_ts(data, "fecha", "inflacion7")
tbp_ts <- convert_ts(data, "fecha", "tbp")
tbpl1_ts <- convert_ts(data, "fecha", "tbpl1")
tbpl2_ts <- convert_ts(data, "fecha", "tbpl2")
tbpl3_ts <- convert_ts(data, "fecha", "tbpl3")
tbpl4_ts <- convert_ts(data, "fecha", "tbpl4")
mif_ts <- convert_ts(data, "fecha", "mif")
mifl1_ts <- convert_ts(data, "fecha", "mifl1")
mifl2_ts <- convert_ts(data, "fecha", "mifl2")
mifl3_ts <- convert_ts(data, "fecha", "mifl3")
mifl4_ts <- convert_ts(data, "fecha", "mifl4")
exp_ts <- convert_ts(data, "fecha", "exp")
apert_comercial_ts <- convert_ts(data, "fecha", "apert_comercial")
apertcomercial75_ts <- convert_ts(data, "fecha", "apertcomercial75")
itcr_ts <- convert_ts(data, "fecha", "itcr")
tc_ts <- convert_ts(data, "fecha", "tc")
vartc_ts <- convert_ts(data, "fecha", "vartc")
credyoy_ts <- convert_ts(data, "fecha", "cred_yoy")

#Dummies
d_2Q04_ts <- convert_ts(data, "fecha", "d_2Q04")
d_4Q07_ts <- convert_ts(data, "fecha", "d_4Q07")
d_1Q08_ts <- convert_ts(data, "fecha", "d_1Q08")
d_4Q08_ts <- convert_ts(data, "fecha", "d_4Q08")
d_1Q09_ts <- convert_ts(data, "fecha", "d_1Q09")
d_2Q09_ts <- convert_ts(data, "fecha", "d_2Q09")
d_3Q09_ts <- convert_ts(data, "fecha", "d_3Q09")
d_4Q09_ts <- convert_ts(data, "fecha", "d_4Q09")
d_1Q14_ts <- convert_ts(data, "fecha", "d_1Q14")
d_1Q20_ts <- convert_ts(data, "fecha", "d_1Q20")
d_2Q20_ts <- convert_ts(data, "fecha", "d_2Q20")
d_3Q20_ts <- convert_ts(data, "fecha", "d_3Q20")
d_4Q20_ts <- convert_ts(data, "fecha", "d_4Q20")

save(ied_ts, ied_pibryoy_ts, credyoy_ts, inflacion_ts, inflacion7_ts, pibryoy_ts,  
     tbp_ts, tbpl1_ts, tbpl2_ts, tbpl3_ts, tbpl4_ts,  
     mif_ts, mifl1_ts, mifl2_ts, mifl3_ts, mifl4_ts, 
     exp_ts, apert_comercial_ts, apertcomercial75_ts, itcr_ts, tc_ts,
     d_2Q04_ts, d_4Q07_ts, d_1Q08_ts, 
     d_4Q08_ts, d_1Q09_ts, d_2Q09_ts, d_3Q09_ts, d_4Q09_ts, 
     d_1Q20_ts, d_2Q20_ts, d_3Q20_ts, d_4Q20_ts,
     file = "../Datos/covariables_ts_data.Rdata")

diff(base)

#Comparación de modelos con la Log verosimilitud

# ll_nested <- logLik(mod_var_simple)
# ll_complex <- logLik(mod_var)
# 
# test <- -2 * (as.numeric(ll_nested) - as.numeric(ll_complex))
# 
# df <- 60 - 54
# 
# pchisq(test, df = df, lower.tail = FALSE)
# 
# lrtest(mod_var_simple, mod_var) # Se rechaza la hipotésis nula de que la loglik es igual en ambos modelos, con alfa de 5%.

#Pruebas de raíz unitaria===================

#PIB real
ur.df(data$pibr, type = "trend", selectlags = "AIC") %>% 
  summary()

ggAcf(data$pibr) +
  ggtitle("PIB Real") + 
  theme_bw()

#PIB real en primeras diferencias
ur.df(data_diff$pibr_diff, type = "drift", selectlags = "AIC") %>% 
  summary()

ggAcf(data_diff$pibr_diff) +
  ggtitle("PIB Real (serie en primeras diferencias)") + 
  theme_bw()

#Credito
ur.df(data$cred_pib, type = "drift", selectlags = "AIC") %>% 
  summary()

ggAcf(data$cred_pib) +
  ggtitle("Créditos") + 
  theme_bw()

#Credito en primeras diferencias
ur.df(data_diff$pibr_diff, type = "drift", selectlags = "AIC") %>% 
  summary()

ggAcf(data_diff$cred_pib_diff) +
  ggtitle("Créditos (en primeras diferencias)") + 
  theme_bw()

#Cointegración=======================================

#PIB y Cred

ca.jo(data %>% dplyr::select(pibr, cred_pib), type = "eigen",  
      season = 4, 
      K = 2,
      ecdet = "none") %>% 
  summary()

#Crecimiento del PIB y credito
ca.jo(data %>% dplyr::select(pibr_yoy, cred_pib), 
      type = "eigen",  
      season = 4, 
      K = 2,
      ecdet = "const") %>% 
  summary()

ca.jo(data %>% dplyr::select(pibr_yoy, cred_pib), 
      type = "trace",  
      K = 2, 
      ecdet = "const") %>% 
  summary()

#Exogeneidad debil

jo_test <- ca.jo(data %>% dplyr::select(log_pibryoy, cred_pib, apert_comercial, tbp, ied_pib), 
      type = "trace",
      spec = "transitory",
      K = 2) 

summary(jo_test)

vecm_exog <- cajorls(jo_test, r = 2)
vecm_exog$rlm$coefficients
coef(vecm_exog$rlm) [2 , ]
beta

#Prueba de exogeneidad de apert_comercial
A <- matrix(c(1, 0, 0, 0, 
              0, 1, 0, 0, 
              0, 0, 0, 0, 
              0, 0, 1, 0, 
              0, 0, 0, 1), nrow = 5, ncol = 4, byrow = T)

summary(alrtest(z = jo_test, A = A , r = 2))

#Prueba de exogeneidad del tbp
A <- matrix(c(1, 0, 0, 0, 
              0, 1, 0, 0, 
              0, 0, 1, 0, 
              0, 0, 0, 0, 
              0, 0, 0, 1), nrow = 5, ncol = 4, byrow = T)

summary(alrtest(z = jo_test, A = A , r = 2))

#Prueba de exogeneidad del ied_pib
A <- matrix(c(1, 0, 0, 0, 
              0, 1, 0, 0, 
              0, 0, 1, 0, 
              0, 0, 0, 1, 
              0, 0, 0, 0), nrow = 5, ncol = 4, byrow = T)

summary(alrtest(z = jo_test, A = A , r = 2))

#Modelo VAR 1===============================
mod1 <- VAR(dplyr::select(data, log_pibryoy, cred_pib), 
            type = "both", 
            lag.max = 12, ic = "AIC",
            exogen = dplyr::select(data, d_2Q20))

summary(mod1)

roots(mod1, modulus = T)

VARselect(cbind(pibryoy_ts, `credyoy_ts`), 
          12, 
          type = "none", 
          exogen = d_2Q20_ts)

##Evaluación del modelo básico

### Autocorrelación serial

residuals_mod1 <- data.frame(fecha = data$fecha[(nrow(data) - nrow(residuals(mod1)) + 1):nrow(data)],
                             res = scale(residuals(mod1))) %>% as_tibble()

serial.test(mod1, lags.pt = 16, type = "PT.asymptotic")
serial.test(mod1, lags.pt = 16, type = "BG")
serial.test(mod1, lags.pt = 16, type = "PT.adjusted")
serial.test(mod1, lags.pt = 16, type = "ES")

acf(residuals(mod1))

ggCcf(x = residuals_mod1$res.log_pibryoy, y = residuals_mod1$res.cred_pib, 
      type = "correlation") + 
  ggtitle("Residuos del modelo PIB vs residuos modelo Cred")

### Heterocedasticidad
arch.test(mod1, lags.multi = 5, multivariate.only = T)

### Normalidad del residuo
normality.test(mod1, multivariate.only = T)

### Estabilidad
stability(mod1, type = "OLS-CUSUM") %>% plot()
roots(mod1, modulus = T)

residuals_mod1_long <- residuals_mod1 %>%
  pivot_longer(-fecha, names_to = "indicador", values_to = "residuo estandarizado")

dev.off()
resz_mod1 <- residuals_mod1_long %>% 
  ggplot(aes(x = fecha, y = `residuo estandarizado`)) + 
  geom_point_interactive(aes(tooltip = paste0("Fecha: ", fecha),
                             data_id = fecha)) + 
  geom_hline(yintercept = 2,linetype = "dashed") +
  geom_hline(yintercept = -2,linetype = "dashed") +
  facet_wrap(. ~ indicador) +
  ggtitle(str_wrap("Residuos del Modelo VAR", 40)) +
  theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA), legend.position = "none")

girafe(ggobj = resz_mod1,
       width_svg = 10, height_svg = 4,
       options = list(
         opts_sizing(rescale = TRUE),
         opts_hover_inv(css = "opacity:0.2;"),
         opts_hover(css = "stroke-width:2;")
       ))

residuals_mod1_long %>% 
  ggplot(aes(x = `residuo estandarizado`)) + 
  geom_density() + 
  facet_wrap(. ~ indicador) + 
  theme_bw() + 
  ggtitle("Gráficos de densidad para los residuos del Modelo VAR")

residuals_mod1_long %>% 
  ggplot(aes(sample = `residuo estandarizado`)) + 
  stat_qq() +
  stat_qq_line() +
  facet_wrap(. ~ indicador) + 
  theme_bw() + 
  ggtitle("QQplots los residuos del Modelo VAR")

mod1_data <- tibble(residuals_mod1, mod1$datamat)

# mod1_data %>% 
#   ggplot(aes(y = res.pibryoy_ts, x = covariables.tc_ts)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F) + 
#   theme_bw()
# 
# mod1_data %>% 
#   ggplot(aes(y = res.credyoy_ts, x = covariables.tbp_ts)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F) + 
#   theme_bw()

#Granger causalidad
causality(mod1, cause = "cred_pib")
causality(mod1, cause = "log_pibryoy")

#Funciones impulso respuesta
pib_a_cred_mod1 <- irf(mod1,
                       impulse = "log_pibryoy",
                       response = "cred_pib",
                       n.ahead = 12,
                       cumulative = T)

plot(pib_a_cred_mod1)

dev.off()
plot(pib_a_cred_cum_mod1)

cred_a_pib_mod1 <- irf(mod1,
                     impulse = "cred_pib",
                     response = "log_pibryoy",
                     n.ahead = 12,
                     cumulative = T)


cred_a_pib_cum_mod1 <- irf(mod1,
                         impulse = "credyoy_ts",
                         response = "pibryoy_ts",
                         n.ahead = 12,
                         cumulative = T)

plot(cred_a_pib_mod1)

#Modelo VAR 2===============================
mod2 <- VAR(dplyr::select(data, log_pibryoy, cred_pib), 
            type = "both", 
            lag.max = 12, ic = "AIC",
            exogen = dplyr::select(data, apert_comercial, tbp, ied_pib, d_2Q20))

summary(mod2)

roots(mod2, modulus = T)

VARselect(cbind(pibryoy_ts, credyoy_ts), 
          12, 
          type = "none", 
          exogen = d_2Q20_ts)

##Evaluación del modelo básico

### Autocorrelación serial

residuals_mod2 <- data.frame(fecha = data$fecha[(nrow(data) - nrow(residuals(mod2)) + 1):nrow(data)],
                             res = scale(residuals(mod2))) %>% as_tibble()

residuals_mod2_long <- residuals_mod2 %>%
  pivot_longer(-fecha, names_to = "indicador", values_to = "residuo estandarizado")

serial.test(mod2, lags.pt = 16, type = "PT.asymptotic")
serial.test(mod2, lags.pt = 16, type = "BG")
serial.test(mod2, lags.pt = 16, type = "PT.adjusted")
serial.test(mod2, lags.pt = 16, type = "ES")

acf(residuals(mod2))

ggCcf(x = residuals_mod2$res.pibr_yoy_diff, y = residuals_mod2$res.cred_pib_diff, 
      type = "correlation") + 
  ggtitle("Residuos del modelo PIB vs residuos modelo Cred")

ggCcf(x = residuals_mod2$res.pibr_yoy_diff, y = residuals_mod2$res.mif_diff, 
      type = "correlation") + 
  ggtitle("Residuos del modelo PIB vs residuos modelo MIF")

ggCcf(x = residuals_mod2$res.cred_pib_diff, y = residuals_mod2$res.mif_diff, 
      type = "correlation") + 
  ggtitle("Residuos del modelo PIB vs residuos modelo MIF")

### Heterocedasticidad
arch.test(mod2, lags.multi = 5, multivariate.only = T)

### Normalidad del residuo
normality.test(mod2, multivariate.only = T)

### Estabilidad
stability(mod2, type = "OLS-CUSUM") %>% plot()
roots(mod2, modulus = T)

resz_mod2 <- residuals_mod2_long %>% 
  ggplot(aes(x = fecha, y = `residuo estandarizado`)) + 
  geom_point_interactive(aes(tooltip = paste0("Fecha: ", fecha),
                             data_id = fecha)) + 
  geom_hline(yintercept = 2,linetype = "dashed") +
  geom_hline(yintercept = -2,linetype = "dashed") +
  facet_wrap(. ~ indicador) +
  ggtitle(str_wrap("Residuos del Modelo VAR", 40)) +
  theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA), legend.position = "none")

girafe(ggobj = resz_mod2,
       width_svg = 10, height_svg = 4,
       options = list(
         opts_sizing(rescale = TRUE),
         opts_hover_inv(css = "opacity:0.2;"),
         opts_hover(css = "stroke-width:2;")
       ))

residuals_mod2_long %>% 
  ggplot(aes(x = `residuo estandarizado`)) + 
  geom_density() + 
  facet_wrap(. ~ indicador) + 
  theme_bw() + 
  ggtitle("Gráficos de densidad para los residuos del Modelo VAR")

residuals_mod2_long %>% 
  ggplot(aes(sample = `residuo estandarizado`)) + 
  stat_qq() +
  stat_qq_line() +
  facet_wrap(. ~ indicador) + 
  theme_bw() + 
  ggtitle("QQplots los residuos del Modelo VAR")

#Granger causalidad
causality(mod2, cause = "cred_pib")
causality(mod2, cause = "log_pibryoy")

#Funciones impulso respuesta
pib_a_cred_mod2 <- irf(mod2,
                     impulse = "log_pibryoy",
                     response = "cred_pib",
                     n.ahead = 12,
                     cumulative = T)

plot(pib_a_cred_mod2)

cred_a_pib_mod2 <- irf(mod2,
                     impulse = "cred_pib",
                     response = "log_pibryoy",
                     n.ahead = 12,
                     cumulative = F,
                     boot = T,
                     ortho = T)


plot(cred_a_pib_mod2)

fevd(mod2, n.ahead = 10) %>% plot()

#Comparativo contra el modelo parsimonioso

lrtest(mod1, mod2) # Se rechaza la hipotésis nula de que la loglik es igual en ambos modelos, con alfa de 5%.

mod2 <- VAR(cbind(pibryoy_ts, credyoy_ts),
                type = "const",
                lag.max = 12, ic = "AIC",
                exogen = cbind(apertcomercial75_ts, inflacion7_ts, ied_ts, d_2Q20_ts))

summary(mod2)

roots(mod2, modulus = T)

VARselect(cbind(pibryoy_ts, credyoy_ts), 
          12, 
          type = "none", 
          exogen = d_2Q20_ts)

#Modelo VAR 3===============================
mod3 <- VAR(dplyr::select(data, log_pibryoy, cred_pib, apert_comercial), 
            type = "both", 
            lag.max = 8, ic = "AIC",
            exogen = dplyr::select(data, tbp, ied_pib, d_2Q20))

summary(mod3)

roots(mod3, modulus = T)

VARselect(cbind(pibryoy_ts, credyoy_ts), 
          12, 
          type = "none", 
          exogen = d_2Q20_ts)

##Evaluación del modelo básico

### Autocorrelación serial

acf(residuals(mod3))

residuals_mod3 <- data.frame(fecha = data$fecha[(nrow(data) - nrow(residuals(mod3)) + 1):nrow(data)],
                             res = scale(residuals(mod3))) %>% as_tibble()

residuals_mod3_long <- residuals_mod3 %>%
  pivot_longer(-fecha, names_to = "indicador", values_to = "residuo estandarizado")

serial.test(mod3, lags.pt = 16, type = "PT.asymptotic")
serial.test(mod3, lags.pt = 16, type = "BG")
serial.test(mod3, lags.pt = 16, type = "PT.adjusted")
serial.test(mod3, lags.pt = 16, type = "ES")

ggCcf(x = residuals_mod3$res.pibryoy_ts, y = residuals_mod3$res.credyoy_ts, 
      type = "correlation") + 
  ggtitle("Residuos del modelo PIB vs residuos modelo Cred")

ggCcf(x = residuals_mod3$res.pibryoy_ts, y = residuals_mod3$res.tbp_ts, 
      type = "correlation") + 
  ggtitle("Residuos del modelo PIB vs residuos modelo margen de intermediación")

ggCcf(x = residuals_mod3$res.credyoy_ts, y = residuals_mod3$res.tbp_ts, 
      type = "correlation") + 
  ggtitle("Residuos del modelo PIB vs residuos modelo Cred")

### Heterocedasticidad
arch.test(mod3, lags.multi = 5, multivariate.only = T)

### Normalidad del residuo
normality.test(mod3, multivariate.only = T)

### Estabilidad
stability(mod3, type = "OLS-CUSUM") %>% plot()
roots(mod3, modulus = T)

resz_mod3 <- residuals_mod3_long %>% 
  ggplot(aes(x = fecha, y = `residuo estandarizado`)) + 
  geom_point_interactive(aes(tooltip = paste0("Fecha: ", fecha),
                             data_id = fecha)) + 
  geom_hline(yintercept = 2,linetype = "dashed") +
  geom_hline(yintercept = -2,linetype = "dashed") +
  facet_wrap(. ~ indicador) +
  ggtitle(str_wrap("Residuos del Modelo VAR", 40)) +
  theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA), legend.position = "none")

girafe(ggobj = resz_mod3,
       width_svg = 10, height_svg = 4,
       options = list(
         opts_sizing(rescale = TRUE),
         opts_hover_inv(css = "opacity:0.2;"),
         opts_hover(css = "stroke-width:2;")
       ))

residuals_mod3_long %>% 
  ggplot(aes(x = `residuo estandarizado`)) + 
  geom_density() + 
  facet_wrap(. ~ indicador) + 
  theme_bw() + 
  ggtitle("Gráficos de densidad para los residuos del Modelo VAR")

#dev.off()
residuals_mod3_long %>% 
  ggplot(aes(sample = `residuo estandarizado`)) + 
  stat_qq() +
  stat_qq_line() +
  facet_wrap(. ~ indicador) + 
  theme_bw() + 
  ggtitle("QQplots los residuos del Modelo VAR")

m2_data <- tibble(residuals_mbasico, mod3$datamat)

mbasico_data %>% 
  ggplot(aes(y = res.pibryoy_ts, x = covariables.tc_ts)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  theme_bw()

mbasico_data %>% 
  ggplot(aes(y = res.credyoy_ts, x = covariables.tbp_ts)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  theme_bw()

#Granger causalidad
causality(mod3, cause = "cred_pib")
causality(mod3, cause = "pibr_yoy")
causality(mod3, cause = "pibusa_yoy")

#Funciones impulso respuesta
pib_a_cred_mod3 <- irf(mod3,
                  impulse = "pibryoy_ts",
                  response = "credyoy_ts",
                  n.ahead = 12,
                  cumulative = F)

plot(pib_a_cred_mod3)

pib_a_cred_cum_mod3 <- irf(mod3,
                         impulse = "pibryoy_ts",
                         response = "credyoy_ts",
                         n.ahead = 12,
                         cumulative = T)

cred_a_pib_mod3 <- irf(mod3,
                     impulse = "credyoy_ts",
                     response = "pibryoy_ts",
                     n.ahead = 12,
                     cumulative = F)

plot(cred_a_pib_mod3)

cred_a_pib_cum_mod3 <- irf(mod3,
                         impulse = "credyoy_ts",
                         response = "pibryoy_ts",
                         n.ahead = 12,
                         cumulative = T,
                         boot = T,
                         ortho = T)

mif_a_pib_mod3 <- irf(mod3,
                    impulse = "tbp_ts",
                    response = "pibryoy_ts",
                    n.ahead = 12,
                    cumulative = F,
                    boot = T,
                    ortho = T)
plot(mif_a_pib_mod3)

mif_a_pib_cum_mod3 <- irf(mod3,
                        impulse = "tbp_ts",
                        response = "pibryoy_ts",
                        n.ahead = 12,
                        cumulative = T)

mif_a_cred_mod3 <- irf(mod3,
                     impulse = "tbp_ts",
                     response = "credyoy_ts",
                     n.ahead = 12,
                     cumulative = T)
plot(mif_a_cred_mod3)

fevd(mod3, n.ahead = 10) %>% plot()

#Comparativo contra el modelo parsimonioso

logLik(mod3)

lrtest(mod1, mod3) # Se rechaza la hipotésis nula de que la loglik es igual en ambos modelos, con alfa de 5%.

lrtest(mod2, mod3) # Se rechaza la hipotésis nula de que la loglik es igual en ambos modelos, con alfa de 5%.

#Modelo VAR 4===============================
mod4 <- VAR(data %>% dplyr::select(pibr_yoy, cred_pib, mif, apert_comercial),
            type = "both",
            lag.max = 8, ic = "AIC",
            exogen = data %>% dplyr::select(pibusa_yoy, petroleo, d_2Q04, d_4Q07, d_1Q20, d_2Q20))

roots(mod4, modulus = T)

VARselect(cbind(pibryoy_ts, credyoy_ts), 
          12, 
          type = "none", 
          exogen = d_2Q20_ts)

##Evaluación del modelo básico

### Autocorrelación serial

residuals_mod4 <- data.frame(fecha = data$fecha[(nrow(data) - nrow(residuals(mod4)) + 1):nrow(data)],
                             res = scale(residuals(mod4))) %>% as_tibble()

residuals_mod4_long <- residuals_mod4 %>%
  pivot_longer(-fecha, names_to = "indicador", values_to = "residuo estandarizado")

serial.test(mod4, lags.pt = 16, type = "PT.asymptotic")
serial.test(mod4, lags.pt = 16, type = "BG")
serial.test(mod4, lags.pt = 16, type = "PT.adjusted")
serial.test(mod4, lags.pt = 16, type = "ES")

ggCcf(x = residuals_mod4$res.pibryoy_ts, y = residuals_mod4$res.credyoy_ts, 
      type = "correlation") + 
  ggtitle("Residuos del modelo PIB vs residuos modelo Cred")

ggCcf(x = residuals_mod4$res.pibryoy_ts, y = residuals_mod4$res.tbp_ts, 
      type = "correlation") + 
  ggtitle("Residuos del modelo PIB vs residuos modelo margen de intermediación")

ggCcf(x = residuals_mod4$res.credyoy_ts, y = residuals_mod4$res.tbp_ts, 
      type = "correlation") + 
  ggtitle("Residuos del modelo PIB vs residuos modelo Cred")

### Heterocedasticidad
arch.test(mod4, lags.multi = 5, multivariate.only = T)

### Normalidad del residuo
normality.test(mod4, multivariate.only = T)

### Estabilidad
stability(mod4, type = "OLS-CUSUM") %>% plot()
roots(mod4, modulus = T)

resz_mod4 <- residuals_mod4_long %>% 
  ggplot(aes(x = fecha, y = `residuo estandarizado`)) + 
  geom_point_interactive(aes(tooltip = paste0("Fecha: ", fecha),
                             data_id = fecha)) + 
  geom_hline(yintercept = 2,linetype = "dashed") +
  geom_hline(yintercept = -2,linetype = "dashed") +
  facet_wrap(. ~ indicador) +
  ggtitle(str_wrap("Residuos del Modelo VAR", 40)) +
  theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA), legend.position = "none")

dev.off()
girafe(ggobj = resz_mod4,
       width_svg = 10, height_svg = 4,
       options = list(
         opts_sizing(rescale = TRUE),
         opts_hover_inv(css = "opacity:0.2;"),
         opts_hover(css = "stroke-width:2;")
       ))

residuals_mod4_long %>% 
  ggplot(aes(x = `residuo estandarizado`)) + 
  geom_density() + 
  facet_wrap(. ~ indicador) + 
  theme_bw() + 
  ggtitle("Gráficos de densidad para los residuos del Modelo VAR")

residuals_mod4_long %>% 
  ggplot(aes(sample = `residuo estandarizado`)) + 
  stat_qq() +
  stat_qq_line() +
  facet_wrap(. ~ indicador) + 
  theme_bw() + 
  ggtitle("QQplots los residuos del Modelo VAR")

m2_data <- tibble(residuals_mbasico, mod4$datamat)

mbasico_data %>% 
  ggplot(aes(y = res.pibryoy_ts, x = covariables.tc_ts)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  theme_bw()

mbasico_data %>% 
  ggplot(aes(y = res.credyoy_ts, x = covariables.tbp_ts)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  theme_bw()

#Granger causalidad
causality(mod4, cause = "credyoy_ts")
causality(mod4, cause = "pibryoy_ts")

#Funciones impulso respuesta
pib_a_cred_mod4 <- irf(mod4,
                     impulse = "pibryoy_ts",
                     response = "credyoy_ts",
                     n.ahead = 12,
                     cumulative = F)

plot(pib_a_cred_mod4)

pib_a_cred_cum_mod4 <- irf(mod4,
                         impulse = "pibryoy_ts",
                         response = "credyoy_ts",
                         n.ahead = 12,
                         cumulative = T)

cred_a_pib_mod4 <- irf(mod4,
                     impulse = "credyoy_ts",
                     response = "pibryoy_ts",
                     n.ahead = 12,
                     cumulative = F)

plot(cred_a_pib_mod4)

cred_a_pib_cum_mod4 <- irf(mod4,
                         impulse = "credyoy_ts",
                         response = "pibryoy_ts",
                         n.ahead = 12,
                         cumulative = T,
                         boot = T,
                         ortho = T)

plot(cred_a_pib_cum_mod4)

mif_a_pib_m3 <- irf(mod4,
                    impulse = "tbp_ts",
                    response = "pibryoy_ts",
                    n.ahead = 12,
                    cumulative = F,
                    boot = T,
                    ortho = T)
plot(mif_a_pib_m3)

mif_a_pib_cum_m3 <- irf(mod4,
                        impulse = "tbp_ts",
                        response = "pibryoy_ts",
                        n.ahead = 12,
                        cumulative = T)

mif_a_cred_m3 <- irf(mod4,
                     impulse = "tbp_ts",
                     response = "credyoy_ts",
                     n.ahead = 12,
                     cumulative = T)
plot(mif_a_cred_m3)

fevd(mod4, n.ahead = 10) %>% plot()

#Comparativo contra el modelo parsimonioso

lrtest(mod_var1, mod_var4) # Se rechaza la hipotésis nula de que la loglik es igual en ambos modelos, con alfa de 5%.

lrtest(mod_var2, mod_var4) # Se rechaza la hipotésis nula de que la loglik es igual en ambos modelos, con alfa de 5%.

#comparativo entre modelos===========================

#Funciones
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
irf_ggplot <- function(data, tit, lab, intervalo = T) {
  
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
    
modelos <- list(mod1, mod2, mod3) 

cred_a_pib <- irf_comparativo(modelos, 
                              nombres = paste("mod", c("1", "2", "3"), sep = ""), 
                              impulse_var = "cred_pib", 
                              resp_var = "log_pibryoy", 
                              acumulado = T)

#dev.off() Correr si ggplot no funciona
irf_ggplot(cred_a_pib, 
           "Respuesta del PIB a un impulso en el cred", 
           "%",
           intervalo = T)

pib_a_cred <- irf_comparativo(modelos, 
                              nombres = paste("mod", c("1", "2", "3"), sep = ""), 
                              impulse_var = "log_pibryoy" , 
                              resp_var = "cred_pib", 
                              acumulado = T)

irf_ggplot(pib_a_cred, 
           "Respuesta del Cred a un impulso en el PIB",
           "% del PIB", 
           intervalo = T)

apertcomer_a_pib <- irf_comparativo(list(mod3), 
                              nombres = paste("mod", c("3"), sep = ""), 
                              impulse_var = "apert_comercial" , 
                              resp_var = "log_pibryoy", 
                              acumulado = T)

irf_ggplot(apertcomer_a_pib, 
           "Respuesta del Cred a un impulso en el PIB",
           "% del PIB", 
           intervalo = T)
