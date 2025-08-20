install.packages(c("ggplot2", "dplyr", "ggpubr", "patchwork", "broom"))

library(ggplot2)
library(dplyr)
library(patchwork)
library(ggpubr)

agua <- read.csv("EFICIENCIA_AGUA.csv")

# -------------------------------------------------------------
# 1. HISTOGRAMA MEJORADO: VOLUMEN DE AGUA
# -------------------------------------------------------------
hist_va <- ggplot(agua, aes(x = volumen_agua)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30,
                 fill = "#1f77b4", 
                 color = "white",
                 alpha = 0.8) +
  geom_density(color = "#e15759", linewidth = 1) +
  labs(title = "Distribución del Volumen de Agua Utilizado",
       subtitle = "Parcelas agrícolas en la Costa ecuatoriana",
       x = "Volumen de Agua (m³)",
       y = "Densidad") +
  theme_pubr() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"))

# Boxplot con identificación de outliers
# Calcular estadísticas necesarias
estadisticas_volumen <- agua %>%
  summarise(
    Media = mean(volumen_agua, na.rm = TRUE),
    Mediana = median(volumen_agua, na.rm = TRUE),
    Q1 = quantile(volumen_agua, 0.25, na.rm = TRUE),
    Q3 = quantile(volumen_agua, 0.75, na.rm = TRUE)
  )

# Crear el diagrama de caja con estadísticas
box_plot_vol <- ggplot(agua, aes(y = volumen_agua)) +
  geom_boxplot(
    fill = "#1f77b4", 
    color = "black",
    outlier.color = "red", 
    outlier.size = 3
  ) +
  # Punto para la media
  geom_point(
    data = estadisticas_volumen,
    aes(y = Media, x = 0),
    shape = 18,  # Diamante
    size = 4,
    color = "yellow",
    fill = "yellow"
  ) +
  # Etiquetas de estadísticas
  geom_text(
    data = estadisticas_volumen,
    aes(y = Media, x = 0.15, label = sprintf("Media: %.2f", Media)),
    size = 3.5,
    fontface = "bold",
    color = "white"
  ) +
  geom_text(
    data = estadisticas_volumen,
    aes(y = Mediana, x = -0.15, label = sprintf("Mediana: %.2f", Mediana)),
    size = 3.5,
    fontface = "bold",
    color = "white"
  ) +
  geom_text(
    data = estadisticas_volumen,
    aes(y = Q1, x = -0.15, label = sprintf("Q1: %.2f", Q1)),
    size = 3.2,
    color = "black"
  ) +
  geom_text(
    data = estadisticas_volumen,
    aes(y = Q3, x = -0.15, label = sprintf("Q3: %.2f", Q3)),
    size = 3.2,
    color = "black"
  ) +
  labs(
    title = "Diagrama de Caja del Volumen de Agua",
    subtitle = "Detección de outliers",
    y = "Volumen de agua"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )

# Gráfico QQ-Plot
qq_plot_vol <- ggplot(agua, aes(sample = volumen_agua)) +
  stat_qq(color = "#1f77b4", size = 2) +
  stat_qq_line(color = "red", linewidth = 0.8) +
  labs(title = "QQ-Plot del Volumen de Agua",
       subtitle = "Prueba de normalidad",
       x = "Cuantiles Teóricos",
       y = "Cuantiles Muestrales") +
  theme_pubr()

# -------------------------------------------------------------
# 2. GRÁFICO DE BARRAS MEJORADO: NIVELES DE TECNIFICACIÓN
# -------------------------------------------------------------
bar_nt <- agua %>%
  count(nivel_tecnificacion) %>%
  mutate(nivel_tecnificacion = factor(nivel_tecnificacion, 
                                      levels = c("bajo", "medio", "alto"),
                                      labels = c("Bajo", "Medio", "Alto"))) %>%
  ggplot(aes(x = nivel_tecnificacion, y = n, fill = nivel_tecnificacion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Bajo" = "#1f77b4", "Medio" = "#ff7f0e", "Alto" = "#2ca02c")) +
  labs(title = "Distribución de Niveles de Tecnificación",
       x = "Nivel de Tecnificación",
       y = "Número de Parcelas") +
  theme_pubclean() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

# -------------------------------------------------------------
# 3. DIAGRAMA DE CAJA MEJORADO: VOLUMEN POR NIVEL DE TECNIFICACIÓN
# -------------------------------------------------------------
box_va <- agua %>%
  filter(!is.na(nivel_tecnificacion)) %>%
  mutate(nivel_tecnificacion = factor(nivel_tecnificacion, 
                                      levels = c("bajo", "medio", "alto"),
                                      labels = c("Bajo", "Medio", "Alto"))) %>%
  ggplot(aes(x = nivel_tecnificacion, y = volumen_agua, fill = nivel_tecnificacion)) +
  geom_boxplot(alpha = 0.8, outlier.color = "#e15759", outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "yellow") +
  scale_fill_manual(values = c("Bajo" = "#1f77b4", "Medio" = "#ff7f0e", "Alto" = "#2ca02c")) +
  labs(title = "Volumen de Agua por Nivel de Tecnificación",
       subtitle = "Comparación entre niveles (cajas: IQR, diamantes: medias)",
       x = "Nivel de Tecnificación",
       y = "Volumen de Agua (m³)") +
  theme_pubr() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"))

# -------------------------------------------------------------
# 4. GRÁFICOS Q-Q MEJORADOS (NORMALIDAD POR NIVEL)
# -------------------------------------------------------------
create_qq_plot <- function(data, nivel, color) {
  ggplot(data, aes(sample = volumen_agua)) +
    stat_qq(color = color, size = 2, alpha = 0.7) +
    stat_qq_line(color = "#e15759", linewidth = 1) +
    labs(title = paste("Normalidad: Nivel", nivel),
         x = "Cuantiles Teóricos",
         y = "Cuantiles Muestrales") +
    theme_pubr() +
    theme(plot.title = element_text(size = 12, face = "bold"))
}

qq_bajo <- create_qq_plot(agua %>% filter(nivel_tecnificacion == "bajo"), "Bajo", "#1f77b4")
qq_medio <- create_qq_plot(agua %>% filter(nivel_tecnificacion == "medio"), "Medio", "#ff7f0e")
qq_alto <- create_qq_plot(agua %>% filter(nivel_tecnificacion == "alto"), "Alto", "#2ca02c")

combined_qq <- (qq_bajo | qq_medio | qq_alto) +
  plot_annotation(title = "Verificación de Normalidad del Volumen de Agua",
                  subtitle = "Por nivel de tecnificación",
                  theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                                plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40")))

# -------------------------------------------------------------
# 5. GRÁFICOS DE DIAGNÓSTICO DE ANOVA MEJORADOS
# -------------------------------------------------------------
# Convertir resultados de ANOVA a formato tidy
library(broom)
rendimiento <- c(vaNa, vaNm, vaNb)
tratamiento <- factor(c(rep("Alto", 467),rep("Medio",1100),
                        rep("Bajo",667)))
anova <- data.frame(rendimiento, tratamiento)
vaNt <- aov(rendimiento ~ tratamiento, data=anova)
diag_data <- augment(vaNt)
print(diag_data)

# Residuos vs Ajustados
res_vs_fitted <- ggplot2::ggplot(diag_data, ggplot2::aes(x = .fitted, y = .resid)) +
  ggplot2::geom_point(color = "#1f77b4", alpha = 0.7) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggplot2::geom_smooth(se = FALSE, color = "#e15759") +
  ggplot2::labs(
    title = "Residuos vs Valores Ajustados",
    x = "Valores Ajustados",
    y = "Residuos"
  ) +
  ggpubr::theme_pubr()

# Q-Q de Residuos
qq_res <- ggplot(diag_data, aes(sample = .resid)) +
  stat_qq(color = "#1f77b4", alpha = 0.7) +
  stat_qq_line(color = "#e15759") +
  labs(title = "Normalidad de Residuos",
       x = "Cuantiles Teóricos",
       y = "Cuantiles de Residuos") +
  theme_pubr()

# Escala-Locación
scale_loc <- ggplot2::ggplot(diag_data, aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
  geom_point(color = "#1f77b4", alpha = 0.7) +
  geom_smooth(se = FALSE, color = "#e15759") +
  labs(title = "Homocedasticidad",
       x = "Valores Ajustados",
       y = expression(sqrt("|Residuos Estandarizados|"))) +
  theme_pubr()

# Residuos vs Orden
res_vs_order <- ggplot(diag_data, aes(x = seq_along(.resid), y = .resid)) +
  geom_point(color = "#1f77b4", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Independencia de Residuos",
       x = "Orden de Observación",
       y = "Residuos") +
  theme_pubr()

# Combinar diagnósticos
diag_plots <- (res_vs_fitted | qq_res) / (scale_loc | res_vs_order) +
  plot_annotation(title = "Diagnóstico de Supuestos del ANOVA",
                  theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)))

# -------------------------------------------------------------
# 6. GRÁFICO DE TUKEY HSD
# -------------------------------------------------------------
tukey_results <- TukeyHSD(vaNt)$tratamiento %>%
  as.data.frame() %>%
  tibble::rownames_to_column("comparacion")

tukey_plot <- ggplot(tukey_results, aes(x = comparacion, y = diff)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), 
                  color = "#1f77b4", 
                  size = 1,
                  fatten = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Comparaciones Múltiples (Tukey HSD)",
       subtitle = "Diferencias en volumen de agua entre niveles",
       x = "Comparación",
       y = "Diferencia en Medias (m³)",
       caption = "Intervalos de confianza al 95%") +
  theme_pubr() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"))

# -------------------------------------------------------------
# EJECUTAR TODOS LOS GRÁFICOS
# -------------------------------------------------------------
print(hist_va)
print(box_plot_vol)
print(qq_plot_vol)
print(bar_nt)
print(box_va)
print(combined_qq)
print(diag_plots)
print(tukey_plot)
