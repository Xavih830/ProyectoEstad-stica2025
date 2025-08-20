install.packages(c("ggplot2", "dplyr", "ggpubr", "UsingR"))

library(UsingR)
library(ggplot2)
library(ggpubr)
library(dplyr)

agua <- read.csv("EFICIENCIA_AGUA.csv")

# Escalar la eficiencia del agua para visualización
agua <- agua %>% 
  mutate(eficiencia_escalada = eficiencia_agua * 1000)

# ---------------------------------------------------------------
# 1. GRÁFICOS ORIGINALES (HIPÓTESIS INICIAL)
# ---------------------------------------------------------------

# Gráfico 1: Hipótesis inicial de relación lineal (recta roja simple)
plot_hipotesis <- ggplot(data = agua, aes(densidad_siembra, eficiencia_escalada)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_abline(color = "red", size = 1) +
  labs(title = "Hipótesis Inicial: Relación Lineal",
       subtitle = "Recta teórica inicial (sin ajuste real)",
       x = "Área cultivada (ha)",
       y = "Eficiencia del agua (ton/m³) × 1000") +
  theme_pubr()

# Gráfico 2: Histograma original de eficiencia del agua
hist_efa_orig <- ggplot(agua, aes(x = eficiencia_agua)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 10) +
  labs(title = "Histograma de Eficiencia del Agua (Original)",
       subtitle = "Distribución inicial observada",
       x = "Eficiencia del agua (ton/m³)", 
       y = "Frecuencia") +
  theme_pubr()

# Gráfico 3: Histograma original de área cultivada
hist_ac_orig <- ggplot(agua, aes(x = densidad_siembra)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 10) +
  labs(title = "Histograma de Área Cultivada (Original)",
       subtitle = "Distribución inicial observada",
       x = "Área cultivada (ha)", 
       y = "Frecuencia") +
  theme_pubr()

# ---------------------------------------------------------------
# 2. MODELO REAL AJUSTADO
# ---------------------------------------------------------------

# Ajustar modelo lineal
modAE <- lm(eficiencia_agua ~ densidad_siembra, data = agua)

# Gráfico 4: Comportamiento real de la regresión
plot_real <- ggplot(data = agua, aes(densidad_siembra, eficiencia_escalada)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_abline(color = "red", size = 1, linetype = "dashed") +  # Hipótesis original
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", fill = "lightgreen") +  # Modelo real
  labs(title = "Modelo Real: Regresión Lineal Ajustada",
       subtitle = "Línea roja: Hipótesis inicial | Área verde: Modelo real con intervalo de confianza",
       x = "Área cultivada (ha)",
       y = "Eficiencia del agua (ton/m³) × 1000") +
  theme_pubr()

# ---------------------------------------------------------------
# 3. PRUEBAS DE NORMALIDAD Y DIAGNÓSTICOS (MEJORADOS)
# ---------------------------------------------------------------

# Gráfico 5: QQ-Plot para eficiencia del agua
qq_efa <- ggplot(agua, aes(sample = eficiencia_agua)) +
  stat_qq(color = "#1f77b4", size = 2) +
  stat_qq_line(color = "red", size = 1) +
  labs(title = "QQ-Plot: Eficiencia del Agua",
       subtitle = "Prueba de normalidad",
       x = "Cuantiles Teóricos",
       y = "Cuantiles Muestrales") +
  theme_pubr()

# Gráfico 6: Boxplot para eficiencia del agua (outliers)
box_efa <- ggplot(agua, aes(y = eficiencia_agua)) +
  geom_boxplot(fill = "#1f77b4", outlier.color = "red", outlier.size = 3) +
  labs(title = "Diagrama de Caja: Eficiencia del Agua",
       subtitle = "Detección de outliers",
       y = "Eficiencia (ton/m³)") +
  theme_pubr()

# Gráfico 7: QQ-Plot para área cultivada
qq_ac <- ggplot(agua, aes(sample = densidad_siembra)) +
  stat_qq(color = "#2ca02c", size = 2) +
  stat_qq_line(color = "red", size = 1) +
  labs(title = "QQ-Plot: Área Cultivada",
       subtitle = "Prueba de normalidad",
       x = "Cuantiles Teóricos",
       y = "Cuantiles Muestrales") +
  theme_pubr()

# Gráfico 8: Boxplot para área cultivada (outliers)
box_ac <- ggplot(agua, aes(y = densidad_siembra)) +
  geom_boxplot(fill = "#2ca02c", outlier.color = "red", outlier.size = 3) +
  labs(title = "Diagrama de Caja: Área Cultivada",
       subtitle = "Detección de outliers",
       y = "Hectáreas (ha)") +
  theme_pubr()

# ---------------------------------------------------------------
# 4. DIAGNÓSTICOS DEL MODELO
# ---------------------------------------------------------------

# Datos para diagnóstico
diag_data <- data.frame(
  .fitted = fitted(modAE),
  .resid = residuals(modAE)
)

# Gráfico 9: Residuos vs Ajustados
res_vs_fitted <- ggplot(diag_data, aes(x = .fitted, y = .resid)) +
  geom_point(color = "#1f77b4", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "#e15759") +
  labs(title = "Residuos vs Valores Ajustados",
       subtitle = "Diagnóstico del modelo de regresión",
       x = "Valores Ajustados",
       y = "Residuos") +
  theme_pubr()

# Gráfico 10: QQ-Plot de residuos
qq_res <- ggplot(diag_data, aes(sample = .resid)) +
  stat_qq(color = "#1f77b4", size = 2) +
  stat_qq_line(color = "red", size = 1) +
  labs(title = "QQ-Plot de Residuos",
       subtitle = "Normalidad de residuos",
       x = "Cuantiles Teóricos",
       y = "Cuantiles Muestrales") +
  theme_pubr()

# ---------------------------------------------------------------
# MOSTRAR TODOS LOS GRÁFICOS INDIVIDUALMENTE
# ---------------------------------------------------------------

# Hipótesis inicial y datos crudos
print(hist_efa_orig)
print(hist_ac_orig)
print(plot_hipotesis)

# Pruebas de normalidad y outliers
print(qq_efa)
print(box_efa)
print(qq_ac)
print(box_ac)

# Modelo real y diagnóstico
print(plot_real)
print(res_vs_fitted)
print(qq_res)

# Resultados del modelo
summary(modAE)
