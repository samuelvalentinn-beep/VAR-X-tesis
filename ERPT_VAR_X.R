#Modelo VAR-X: Exchange Rate Pass-Through (ERPT) México 2003–2024
#Autor: Maldonado Chávez Samuel Valentín
#Frecuencia: Mensual, Muestra: Enero 2003 – Diciembre 2024

rm(list = ls())

library(readxl)
library(tseries)
library(forecast)
library(vars)
library(lmtest)
library(ggplot2)
library(zoo)

# 1. CARGA Y TRANSFORMACIÓN DE DATOS

ERPT_DF<-read_excel("VAR-X ERPT DF.xlsx")

# Parámetros de la muestra
START_YEAR <- 2003
END_YEAR   <- 2024
FREQ       <- 12

# Funciones auxiliares
to_ts    <- function(x) ts(x, start = c(START_YEAR, 1), end = c(END_YEAR, 12), frequency = FREQ)
difflog  <- function(x) diff(log(x))

# --- Variables en niveles ---
Y_end  <- to_ts(ERPT_DF$Y)
R_end  <- to_ts(ERPT_DF$R)
S      <- to_ts(ERPT_DF$S)
P_C    <- to_ts(ERPT_DF$PC)
PP_c   <- to_ts(ERPT_DF$PPc)
PP_nc  <- to_ts(ERPT_DF$PPnc)
PC_c   <- to_ts(ERPT_DF$PCc)
PC_nc  <- to_ts(ERPT_DF$PCnc)
P_ayc  <- to_ts(ERPT_DF$Payc)
Y_exo  <- to_ts(ERPT_DF$`Y exog`)
R_exo  <- to_ts(ERPT_DF$`R exog`)
P_exo  <- to_ts(ERPT_DF$`PC exog`)
P_comm <- to_ts(ERPT_DF$Pcomm)

#Primeras diferencias logarítmicas
d1Y_end  <- difflog(Y_end)
d1R_end  <- window(R_end,  start = c(START_YEAR, 2))   # tasa: solo alinear, no difflog
d1S      <- difflog(S)
d1P_C    <- difflog(P_C)
d1PP_c   <- difflog(PP_c)
d1PP_nc  <- difflog(PP_nc)
d1PC_c   <- difflog(PC_c)
d1PC_nc  <- difflog(PC_nc)
d1P_ayc  <- difflog(P_ayc)
d1Y_exo  <- difflog(Y_exo)
d1R_exo  <- window(R_exo,  start = c(START_YEAR, 2))
d1P_exo  <- difflog(P_exo)
d1P_comm <- difflog(P_comm)

#2. FIGURAS DESCRIPTIVAS

#Figura 1: Inflación vs. Depreciación nominal (var. % anual) ---

var_tc_anual   <- (window(S,   start = c(START_YEAR, 1)) / lag(S,   -12) - 1) * 100
var_inpc_anual <- (window(P_C, start = c(START_YEAR, 1)) / lag(P_C, -12) - 1) * 100

fechas_fig1 <- seq(as.Date("2003-01-01"), by = "month", length.out = length(var_tc_anual))

df_tc_inf <- data.frame(
  Fecha       = fechas_fig1,
  Depreciacion = as.numeric(var_tc_anual),
  Inflacion    = as.numeric(var_inpc_anual)
)

ggplot(df_tc_inf, aes(x = Fecha)) +
  geom_line(aes(y = Depreciacion, color = "Depreciación"), linewidth = 1) +
  geom_line(aes(y = Inflacion,    color = "Inflación"),    linewidth = 1) +
  scale_color_manual(values = c("Depreciación" = "blue", "Inflación" = "red")) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.3) +
  labs(
    title    = "Inflación y Tasa de Depreciación Nominal: 2003–2024",
    subtitle = "Variación porcentual anual",
    x        = "Horizonte (Años)",
    y        = "Porcentaje (%)",
    color    = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top"
  )

#Figura 2: Regresión móvil Inflación ~ Depreciación

dPC_z   <- zoo(diff(log(P_C), lag = 12), order.by = as.yearmon(time(diff(log(P_C), lag = 12))))
dS_z    <- zoo(diff(log(S),   lag = 12), order.by = as.yearmon(time(diff(log(S),   lag = 12))))
dPC_1_z <- lag(dPC_z, k = 1)

df_zoo <- na.omit(merge(dPC_z, dPC_1_z, dS_z))
colnames(df_zoo) <- c("dPC", "dPC_1", "dS")

#Regresión móvil con ventana de 48 meses
WINDOW_SIZE <- 48
n           <- nrow(df_zoo)
fechas_zoo  <- index(df_zoo)
n_roll      <- n - WINDOW_SIZE + 1

#Pre-alocar vectores 
coef_b2  <- numeric(n_roll)
lower_ci <- numeric(n_roll)
upper_ci <- numeric(n_roll)

for (i in seq_len(n_roll)) {
  data_window <- df_zoo[i:(i + WINDOW_SIZE - 1), ]
  mod         <- lm(dPC ~ dPC_1 + dS, data = data_window)
  beta2       <- coef(summary(mod))["dS", ]
  coef_b2[i]  <- beta2["Estimate"]
  se          <- beta2["Std. Error"]
  lower_ci[i] <- coef_b2[i] - qnorm(0.95) * se
  upper_ci[i] <- coef_b2[i] + qnorm(0.95) * se
}

df_result <- data.frame(
  Fecha = as.Date(as.yearmon(fechas_zoo[WINDOW_SIZE:n])),
  Coef  = coef_b2,
  Lower = lower_ci,
  Upper = upper_ci
)

ggplot(df_result, aes(x = Fecha)) +
  geom_line(aes(y = Coef),  color = "blue", linewidth = 1) +
  geom_line(aes(y = Lower), color = "blue", linetype = "dashed") +
  geom_line(aes(y = Upper), color = "blue", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title    = "Regresión Móvil entre Inflación y Depreciación Nominal",
    subtitle = "Coeficiente de regresión e intervalos al 90% de confianza",
    x        = "Horizonte (Años)",
    y        = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

cat("Correlación inflación–depreciación nominal:",
    round(cor(df_zoo$dPC, df_zoo$dS), 4), "\n")

#3. PRUEBAS DE ESTACIONARIDAD

vars_endogenas <- list(
  d1Y_end, d1R_end, d1S, d1P_C,
  d1Y_exo, d1R_exo, d1P_exo, d1P_comm
)
nombres_vars <- c(
  "d1Y_end", "d1R_end", "d1S", "d1P_C",
  "d1Y_exo", "d1R_exo", "d1P_exo", "d1P_comm"
)

for (i in seq_along(vars_endogenas)) {
  cat("\n--- Análisis de", nombres_vars[i], "---\n")
  acf(vars_endogenas[[i]],  main = paste("ACF de",  nombres_vars[i]))
  pacf(vars_endogenas[[i]], main = paste("PACF de", nombres_vars[i]))
  print(auto.arima(vars_endogenas[[i]]))
  print(adf.test(vars_endogenas[[i]]))
}

#4. ESTADÍSTICOS DESCRIPTIVOS Y PRUEBA ADF 

vars_list <- list(
  d1Y_end = d1Y_end, d1R_end = d1R_end, d1S = d1S,      d1P_C    = d1P_C,
  d1PP_c  = d1PP_c,  d1PP_nc = d1PP_nc, d1PC_c = d1PC_c, d1PC_nc  = d1PC_nc,
  d1P_ayc = d1P_ayc, d1Y_exo = d1Y_exo, d1R_exo = d1R_exo,
  d1P_exo = d1P_exo, d1P_comm = d1P_comm
)

#Estadísticos descriptivos
desc_stats <- function(x) {
  x <- as.numeric(na.omit(x))
  c(Media = mean(x), DesvStd = sd(x), N = length(x))
}
stats_df <- as.data.frame(do.call(rbind, lapply(vars_list, desc_stats)))
stats_df$Variable <- rownames(stats_df)
rownames(stats_df) <- NULL
stats_df <- stats_df[, c("Variable", "Media", "DesvStd", "N")]
print(stats_df)

#Prueba ADF
run_adf <- function(x) {
  x    <- as.numeric(na.omit(x))
  test <- adf.test(x, alternative = "stationary")
  c(round(test$statistic[[1]], 4), round(test$p.value, 4))
}
adf_results <- as.data.frame(do.call(rbind, lapply(vars_list, run_adf)))
colnames(adf_results) <- c("Estadístico", "P.valor")
adf_results$Variable  <- rownames(adf_results)
rownames(adf_results) <- NULL
print(adf_results[, c("Variable", "Estadístico", "P.valor")])

#5. MODELO VAR-X BASE (4 variables endógenas)

Y_end_b <- cbind(d1Y_end, d1R_end, d1S, d1P_C)
colnames(Y_end_b) <- c("d1Y_end", "d1R_end", "d1S", "d1P_C")

X_exo_b <- cbind(d1Y_exo, d1R_exo, d1P_exo, d1P_comm)
colnames(X_exo_b) <- c("d1Y_exo", "d1R_exo", "d1P_exo", "d1P_comm")

#Selección de rezagos
lagselect_b <- VARselect(Y_end_b, lag.max = 12, type = "const")
cat("Rezagos óptimos (modelo base):\n"); print(lagselect_b$selection)

#Estimación
Model.VARX_b <- VAR(Y_end_b, p = 2, type = "const", season = NULL, exog = X_exo_b)
summary(Model.VARX_b)

#Estabilidad
stopifnot("Modelo base inestable — revisar especificación" = all(roots(Model.VARX_b) < 1))

#Causalidad de Granger
cat("\nCausalidad de Granger (d1S → sistema, modelo base):\n")
print(causality(Model.VARX_b, cause = "d1S")$Granger)

#IRF
IRF_INF <- irf(Model.VARX_b, impulse = "d1S", response = "d1P_C",
               cumulative = TRUE,  n.ahead = 23, boot = TRUE, ci = 0.90, ortho = TRUE)
IRF_TC  <- irf(Model.VARX_b, impulse = "d1S", response = "d1S",
               cumulative = FALSE, n.ahead = 23, boot = TRUE, ci = 0.90, ortho = TRUE)

#Función auxiliar para graficar IRF
plot_irf <- function(irf_obj, var_name, titulo) {
  df <- data.frame(
    horizon = 1:24,
    irf     = irf_obj$irf$d1S[, var_name],
    lower   = irf_obj$Lower$d1S[, var_name],
    upper   = irf_obj$Upper$d1S[, var_name]
  )
  ggplot(df, aes(x = horizon, y = irf)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    geom_line(color = "steelblue4", linewidth = 1.2) +
    geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
    labs(title = titulo, x = "Horizonte (meses)", y = "Respuesta impulso (%)") +
    scale_x_continuous(breaks = seq(1, 24, 2), expand = c(0, 0)) +
    coord_cartesian(xlim = c(1, 24)) +
    theme_minimal(base_size = 14)
}

plot_irf(IRF_INF, "d1P_C", "IRF: Respuesta de la inflación a un choque en el tipo de cambio")
plot_irf(IRF_TC,  "d1S",   "IRF: Respuesta del tipo de cambio a su propio choque")


#6. ELASTICIDAD DE TRASPASO (PT) ACUMULADA

#Función unificada para calcular y graficar PT acumulada
generar_PT <- function(IRF_precio, IRF_tc, nombre_var, titulo = NULL) {
  
  shock_1pct <- IRF_tc$irf$d1S[1, "d1S"]
  
  #Sumas acumuladas
  cs_precio <- cumsum(IRF_precio$irf$d1S[,   nombre_var])
  cs_tc     <- cumsum(IRF_tc$irf$d1S[,       "d1S"])
  cs_p_lo   <- cumsum(IRF_precio$Lower$d1S[,  nombre_var])
  cs_p_up   <- cumsum(IRF_precio$Upper$d1S[,  nombre_var])
  cs_tc_lo  <- cumsum(IRF_tc$Lower$d1S[,      "d1S"])
  cs_tc_up  <- cumsum(IRF_tc$Upper$d1S[,      "d1S"])
  
  #Elasticidad y bandas
  UMBRAL <- 1e-6
  PT       <- ifelse(abs(cs_tc)    < UMBRAL, NA, (cs_precio / cs_tc)    * shock_1pct)
  PT_lower <- ifelse(abs(cs_tc_up) < UMBRAL, NA, (cs_p_lo  / cs_tc_up) * shock_1pct)
  PT_upper <- ifelse(abs(cs_tc_lo) < UMBRAL, NA, (cs_p_up  / cs_tc_lo) * shock_1pct)
  
  df <- data.frame(
    horizonte = seq_along(PT),
    PT        = PT,
    lower     = PT_lower,
    upper     = PT_upper
  )
  
  ggplot(df, aes(x = horizonte, y = PT)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    geom_line(color = "steelblue4", linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(title = titulo, x = "Horizonte (meses)", y = "PT acumulada") +
    scale_x_continuous(breaks = seq(1, 24, 2), expand = c(0, 0)) +
    coord_cartesian(xlim = c(1, 24)) +
    theme_minimal(base_size = 14)
}

#PT modelo base (INPC)
generar_PT(IRF_INF, IRF_TC, "d1P_C", "Elasticidad de traspaso acumulada (INPC)")


#7. MODELO VAR-X COMPLETO (8 variables endógenas)

Y_end_c <- cbind(d1Y_end, d1R_end, d1S, d1PP_c, d1PP_nc, d1PC_c, d1PC_nc, d1P_ayc)
colnames(Y_end_c) <- c("d1Y_end", "d1R_end", "d1S", "d1PP_c", "d1PP_nc",
                       "d1PC_c", "d1PC_nc", "d1P_ayc")

X_exo_c <- X_exo_b   # mismas variables exógenas

#Selección de rezagos
lagselect_c <- VARselect(Y_end_c, lag.max = 12, type = "const")
cat("Rezagos óptimos (modelo completo):\n"); print(lagselect_c$selection)

#Estimación
Model.VARC <- VAR(Y_end_c, p = 2, type = "const", season = NULL, exog = X_exo_c)
summary(Model.VARC)

#Estabilidad
stopifnot("Modelo completo inestable — revisar especificación" = all(roots(Model.VARC) < 1))

#Causalidad de Granger
cat("\nCausalidad de Granger (d1S → sistema, modelo completo):\n")
print(causality(Model.VARC, cause = "d1S")$Granger)

#IRF modelo completo (reutiliza IRF_TC del modelo base como referencia del TC)
IRF_PP_c  <- irf(Model.VARC, impulse = "d1S", response = "d1PP_c",  cumulative = TRUE,
                 n.ahead = 23, boot = TRUE, ci = 0.90, ortho = TRUE)
IRF_PP_nc <- irf(Model.VARC, impulse = "d1S", response = "d1PP_nc", cumulative = TRUE,
                 n.ahead = 23, boot = TRUE, ci = 0.90, ortho = TRUE)
IRF_PC_c  <- irf(Model.VARC, impulse = "d1S", response = "d1PC_c",  cumulative = TRUE,
                 n.ahead = 23, boot = TRUE, ci = 0.90, ortho = TRUE)
IRF_PC_nc <- irf(Model.VARC, impulse = "d1S", response = "d1PC_nc", cumulative = TRUE,
                 n.ahead = 23, boot = TRUE, ci = 0.90, ortho = TRUE)
IRF_P_ayc <- irf(Model.VARC, impulse = "d1S", response = "d1P_ayc", cumulative = TRUE,
                 n.ahead = 23, boot = TRUE, ci = 0.90, ortho = TRUE)

#IRF del TC en el modelo completo (necesario para normalizar PT)
IRF_TC_c <- irf(Model.VARC, impulse = "d1S", response = "d1S",
                cumulative = FALSE, n.ahead = 23, boot = TRUE, ci = 0.90, ortho = TRUE)

#Elasticidades modelo completo
generar_PT(IRF_PP_c,  IRF_TC_c, "d1PP_c",  "Elasticidad de traspaso acumulada: PP comerciables")
generar_PT(IRF_PP_nc, IRF_TC_c, "d1PP_nc", "Elasticidad de traspaso acumulada: PP no comerciables")
generar_PT(IRF_PC_c,  IRF_TC_c, "d1PC_c",  "Elasticidad de traspaso acumulada: PC comerciables")
generar_PT(IRF_PC_nc, IRF_TC_c, "d1PC_nc", "Elasticidad de traspaso acumulada: PC no comerciables")
generar_PT(IRF_P_ayc, IRF_TC_c, "d1P_ayc", "Elasticidad de traspaso acumulada: Precios administrados y concertados")
