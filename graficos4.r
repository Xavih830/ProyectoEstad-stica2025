install.packages(c("ggplot2", "dplyr", "ggpubr", "scales"))

library(ggplot2)
library(dplyr)
library(ggpubr)
library(scales)  # Para porcentajes en etiquetas

agua <- read.csv("EFICIENCIA_AGUA.csv")

# -------------------------------------------------------------
# 1. GRÁFICO DE BARRAS MEJORADO: TIPO DE SUELO
# -------------------------------------------------------------
suelo_plot <- agua %>%
  count(tipo_suelo) %>%
  ggplot(aes(x = reorder(tipo_suelo, n), y = n, fill = tipo_suelo)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), 
            hjust = -0.2, 
            size = 4, 
            color = "black", 
            fontface = "bold") +
  scale_fill_manual(values = c("franco" = "#1f77b4", 
                               "arcilloso" = "#ff7f0e", 
                               "arenoso" = "#2ca02c")) +
  labs(title = "Distribución de Tipos de Suelo",
       subtitle = "Parcelas agrícolas en la Costa ecuatoriana",
       x = "Tipo de Suelo",
       y = "Número de Parcelas") +
  coord_flip() +
  theme_pubclean() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        axis.text.y = element_text(size = 11))

# -------------------------------------------------------------
# 2. GRÁFICO DE BARRAS MEJORADO: NIVEL DE TECNIFICACIÓN
# -------------------------------------------------------------
tecnificacion_plot <- agua %>%
  count(nivel_tecnificacion) %>%
  ggplot(aes(x = reorder(nivel_tecnificacion, n), 
             y = n, 
             fill = nivel_tecnificacion)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), 
            vjust = -0.5, 
            size = 4, 
            color = "black", 
            fontface = "bold") +
  # Paleta que se ajusta automáticamente al número de categorías
  scale_fill_brewer(palette = "Set2", 
                    name = "Nivel de Tecnificación") +
  scale_x_discrete(labels = function(x) tools::toTitleCase(tolower(x))) +
  labs(title = "Distribución de Niveles de Tecnificación",
       subtitle = "Parcelas agrícolas en la Costa ecuatoriana",
       x = "Nivel de Tecnificación",
       y = "Número de Parcelas") +
  theme_pubclean() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"))

# -------------------------------------------------------------
# 3. GRÁFICO DE RELACIÓN MEJORADO: TIPO DE SUELO VS TECNIFICACIÓN
# -------------------------------------------------------------
relacion_plot <- agua %>%
  count(tipo_suelo, nivel_tecnificacion) %>%
  ggplot(aes(x = tipo_suelo, y = n, fill = nivel_tecnificacion)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5) +
  scale_fill_brewer(palette = "Set2", name = "Nivel de Tecnificación") +
  labs(title = "Relación entre Tipo de Suelo y Tecnificación",
       subtitle = "Conteo de parcelas por combinación",
       x = "Tipo de Suelo",
       y = "Número de Parcelas") +
  theme_pubr() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )

print(relacion_plot)
print(suelo_plot)
print(tecnificacion_plot)
              