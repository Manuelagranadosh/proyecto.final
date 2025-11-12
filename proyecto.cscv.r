
library(tidyverse)   # Para manipulación y gráficos
library(ggplot2)     # Visualización
library(readr)       # Lectura del archivo
data <- read_csv("sentimentdataset.csv")

glimpse(data)

data_clean <- data %>%
  select(followers, engagement_rate) %>%
  filter(!is.na(followers), !is.na(engagement_rate))

head(data_clean)

summary(data_clean)
correlation <- cor(data_clean$followers, data_clean$engagement_rate, method = "pearson")
print(paste("Correlación Pearson:", round(correlation, 3)))

modelo <- lm(engagement_rate ~ followers, data = data_clean)
summary(modelo)

ggplot(data_clean, aes(x = followers, y = engagement_rate)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Relación entre número de seguidores y tasa de engagement",
    x = "Número de seguidores",
    y = "Tasa de engagement (%)"
  ) +
  theme_minimal()

nuevo <- data.frame(followers = 500000)
prediccion <- predict(modelo, newdata = nuevo)
print(paste("Predicción de engagement para 500k seguidores:", round(prediccion, 2), "%"))
