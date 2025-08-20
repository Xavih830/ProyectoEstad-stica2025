install.packages(c("ggplot2", "dplyr", "ggpubr", "patchwork"))

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(patchwork)  # Para combinar gráficos
library(ggpubr)     # Para temas profesionales

# Leer datos
agua <- read.csv("EFICIENCIA_AGUA.csv")

# 1. GRÁFICO DE BARRAS MEJORADO: MÉTODOS DE RIEGO
bar_plot_riego <- agua %>%
  count(metodo_riego) %>%
  ggplot(aes(x = reorder(metodo_riego, n), y = n, fill = metodo_riego)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 4, color = "black") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  labs(title = "Distribución de Métodos de Riego",
       subtitle = "Muestras analizadas en parcelas de la Costa ecuatoriana",
       x = "Método de Riego",
       y = "Número de Parcelas") +
  theme_pubclean() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"))

# 2. HISTOGRAMA MEJORADO: EFICIENCIA DEL AGUA
hist_plot <- ggplot(agua, aes(x = eficiencia_agua)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 20, 
                 fill = "#4e79a7", 
                 color = "white", 
                 alpha = 0.8) +
  geom_density(color = "#e15759", linewidth = 1) +
  labs(title = "Distribución de la Eficiencia en el Uso del Agua",
       subtitle = "Toneladas por m³ de agua en parcelas de la Costa ecuatoriana",
       x = "Eficiencia del Agua (ton/m³)",
       y = "Densidad") +
  theme_pubr() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"))

print(hist_plot)

# 2.1 DIAGRAMA DE CAJA MEJORADO: EFICIENCIA DEL AGUA
eficiencia_data <- agua %>%
  filter(!is.na(eficiencia_agua)) %>%
  select(eficiencia_agua)

# Calcular estadísticas necesarias
estadisticas <- eficiencia_data %>%
  summarise(
    Media = mean(eficiencia_agua),
    Mediana = median(eficiencia_agua),
    Q1 = quantile(eficiencia_agua, 0.25),
    Q3 = quantile(eficiencia_agua, 0.75),
    Min = min(eficiencia_agua),
    Max = max(eficiencia_agua),
    IQR = IQR(eficiencia_agua)
  )

# Crear gráfico
eficiencia_agua_plot <- ggplot(eficiencia_data, aes(y = eficiencia_agua)) +
  geom_boxplot(
    fill = "#4e79a7",          # Color azul
    alpha = 0.8,               # Transparencia
    outlier.color = "#e34a33", # Color rojo para outliers
    outlier.size = 2.5,        # Tamaño de outliers
    width = 0.4                # Ancho del boxplot
  ) +
  # Punto para la media
  geom_point(
    data = estadisticas,
    aes(y = Media, x = 0),
    shape = 23,                # Forma de diamante
    size = 4,
    fill = "yellow",
    color = "black"
  ) +
  # Etiqueta para la media
  geom_text(
    data = estadisticas,
    aes(y = Media, x = 0, label = sprintf("Media: %.2f", Media)),
    vjust = -2.5,
    size = 4,
    fontface = "bold"
  ) +
  # Etiqueta para la mediana
  geom_text(
    data = estadisticas,
    aes(y = Mediana, x = 0, label = sprintf("Mediana: %.2f", Mediana)),
    vjust = 1.5,
    size = 4,
    fontface = "bold",
    color = "white"
  ) +
  # Etiqueta para Q1
  geom_text(
    data = estadisticas,
    aes(y = Q1, x = 0, label = sprintf("Q1: %.2f", Q1)),
    vjust = 1.5,
    size = 3.5,
    color = "black"
  ) +
  # Etiqueta para Q3
  geom_text(
    data = estadisticas,
    aes(y = Q3, x = 0, label = sprintf("Q3: %.2f", Q3)),
    vjust = -1,
    size = 3.5,
    color = "black"
  ) +
  labs(
    title = "Distribución de la Eficiencia del Agua",
    subtitle = "Análisis de cuartiles y valores atípicos",
    y = "Eficiencia del Agua (ton/m³)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size = 12),
    axis.text.x = element_blank(),  # Ocultar etiquetas del eje X
    axis.title.x = element_blank(), # Ocultar título del eje X
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.5)
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))

print(eficiencia_agua_plot)

# 2.2 GRÁFICO Q-Q DE EFICIENCIA DEL AGUA (NORMALIDAD)
qq_plot <- ggplot(agua, aes(sample = eficiencia_agua)) +
  # Agregar puntos de los cuantiles
  stat_qq(color = "#1f77b4", size = 3, alpha = 0.7) +
  
  # Agregar línea de referencia para distribución normal
  stat_qq_line(color = "#e15759", linewidth = 1.2, linetype = "dashed") +
  
  # Añadir título y etiquetas
  labs(
    title = "Evaluación de Normalidad: Eficiencia del Agua",
    subtitle = "Gráfico Q-Q para todas las parcelas agrícolas",
    x = "Cuantiles Teóricos de la Distribución Normal",
    y = "Cuantiles Muestrales",
    caption = "Línea roja: Distribución normal teórica"
  ) +
  
  # Usar tema profesional
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.caption = element_text(face = "italic", color = "gray50")
  )

# Mostrar el gráfico
print(qq_plot)

# 3. DIAGRAMA DE CAJA MEJORADO: COMPARACIÓN DE EFICIENCIAS
# Preparar datos
eficiencia_riego_data <- agua %>%
  filter(!is.na(metodo_riego)) %>%
  dplyr::select(metodo_riego, eficiencia_agua)

# Calcular promedios
promedios_riego <- eficiencia_riego_data %>%
  group_by(metodo_riego) %>%
  summarise(Media = mean(eficiencia_agua))

box_plot_riego <- ggplot(eficiencia_riego_data, 
                         aes(x = metodo_riego, y = eficiencia_agua, 
                             fill = metodo_riego)) +
  geom_boxplot(alpha = 0.8, outlier.color = "#e15759", outlier.size = 2) +
  geom_point(data = promedios_riego, aes(y = Media), 
             shape = 23, size = 4, fill = "yellow", color = "black") +
  geom_text(data = promedios_riego, 
            aes(y = Media, label = sprintf("Media: %.2f", Media)),
            vjust = -2, hjust = 0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("aspersión" = "#1f77b4", 
                               "goteo" = "#ff7f0e", 
                               "gravedad" = "#2ca02c")) +
  labs(title = "Comparación de Eficiencia en el Uso del Agua",
       subtitle = "Por método de riego",
       x = "Método de Riego",
       y = "Eficiencia del Agua (ton/m³)") +
  theme_pubr() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"))

# 4. GRÁFICOS Q-Q MEJORADOS (NORMALIDAD)
# Crear función para generar gráficos Q-Q individuales
create_qq_plot <- function(data, metodo, color) {
  ggplot(data, aes(sample = eficiencia_agua)) +
    stat_qq(color = color, size = 2) +
    stat_qq_line(color = "#e15759", linewidth = 1) +
    labs(title = paste("Gráfico Q-Q:", metodo),
         x = "Cuantiles Teóricos",
         y = "Cuantiles Muestrales") +
    theme_pubr()
}

# Generar gráficos individuales
qq_aspersion <- create_qq_plot(
  agua %>% filter(metodo_riego == "aspersión"),
  "Riego por Aspersión",
  "#1f77b4"
)

qq_goteo <- create_qq_plot(
  agua %>% filter(metodo_riego == "goteo"),
  "Riego por Goteo",
  "#ff7f0e"
)

qq_gravedad <- create_qq_plot(
  agua %>% filter(metodo_riego == "gravedad"),
  "Riego por Gravedad",
  "#2ca02c"
)

# Combinar gráficos Q-Q
combined_qq <- (qq_aspersion | qq_goteo | qq_gravedad) +
  plot_annotation(title = "Verificación de Normalidad",
                  subtitle = "Análisis de eficiencia del agua por método de riego",
                  theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                                plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40")))

# 5. GRÁFICOS DE VERIFICACIÓN DE SUPUESTOS DEL ANOVA
# Crear gráficos de diagnóstico con mejor presentación
par(mfrow = c(2, 2))
plot(efaMr, 
     pch = 16, 
     col = "#1f77b4",
     main = "Diagnóstico de Residuos del Modelo ANOVA",
     cex.main = 1.2)
  
print(bar_plot_riego)
print(box_plot_riego)
print(combined_qq)
