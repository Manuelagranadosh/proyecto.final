# ================================
# 1. Cargar librerías
# ================================
library(tidyverse)   # Para manipulación y gráficos
library(ggplot2)     # Visualización
library(readr)       # Lectura del archivo

# ================================
# 2. Importar base de datos
# ================================
# Cambia la ruta al archivo según tu entorno
data <- read_csv("sentimentdataset.csv")

# Ver estructura general
glimpse(data)

# ================================
# 3. Seleccionar variables relevantes
# ================================
# Suponiendo que las columnas se llamen algo como:
# "followers" y "engagement_rate"
# (ajusta los nombres exactos según tu archivo)
data_clean <- data %>%
  select(followers, engagement_rate) %>%
  filter(!is.na(followers), !is.na(engagement_rate))

# Ver primeras filas
head(data_clean)

# ================================
# 4. Estadísticas descriptivas
# ================================
summary(data_clean)

# ================================
# 5. Correlación entre seguidores y engagement
# ================================
correlation <- cor(data_clean$followers, data_clean$engagement_rate, method = "pearson")
print(paste("Correlación Pearson:", round(correlation, 3)))

# ================================
# 6. Modelo de regresión lineal
# ================================
modelo <- lm(engagement_rate ~ followers, data = data_clean)
summary(modelo)

# ================================
# 7. Visualización del modelo
# ================================
ggplot(data_clean, aes(x = followers, y = engagement_rate)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Relación entre número de seguidores y tasa de engagement",
    x = "Número de seguidores",
    y = "Tasa de engagement (%)"
  ) +
  theme_minimal()

# ================================
# 8. Predicción (opcional)
# ================================
# Ejemplo: predecir engagement para 500,000 seguidores
nuevo <- data.frame(followers = 500000)
prediccion <- predict(modelo, newdata = nuevo)
print(paste("Predicción de engagement para 500k seguidores:", round(prediccion, 2), "%"))
