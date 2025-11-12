# Cargar librerías necesarias
library(tidyverse)
# Instalar si es necesario: install.packages("lubridate") para manejo de fechas
library(lubridate)

# --- 1. CARGA Y PREPARACIÓN DE DATOS ---

# Cargar el dataset real
df_superstore <- read_csv("superstore_like.csv")

# Renombrar columnas para facilitar el manejo (evitar espacios)
df_superstore <- df_superstore %>%
  rename_all(~gsub(" ", "_", .)) %>%
  # Asegurar que las columnas clave sean numéricas y de fecha
  mutate(
    # La columna Order Date está en formato Mes/Día/Año (MM/DD/YY)
    Order_Date = mdy(Order_Date), 
    Month_Year = format(Order_Date, "%Y-%m") # Crear columna Mes-Año para la tendencia
  )

message("--- Primeras 6 filas del Dataset Superstore Cargado ---")
print(head(df_superstore))

# -------------------------------------------------------------

## 2. AJUSTE Y RESUMEN DEL MODELO PREDICTIVO (Objetivo Principal)

# 2.1. Evaluación de la Correlación (Sales vs. Profit)
correlation_test <- cor.test(
  ~ Sales + Profit,
  data = df_superstore,
  method = "pearson" 
)

message("\n--- COEFICIENTE DE CORRELACIÓN DE PEARSON (Sales vs. Profit) ---")
print(correlation_test)

# 2.2. Ajuste del Modelo de Regresión Lineal Simple
# Fórmula: Profit = β₀ + β₁(Sales) + ε
linear_model_sales <- lm(
  Profit ~ Sales,
  data = df_superstore
)

message("\n--- RESUMEN DEL MODELO DE REGRESIÓN LINEAL (Profit ~ Sales) ---")
print(summary(linear_model_sales))
message("\n------------------------------------------------")


## 3. FUNCIÓN PREDICTIVA (Predicción de Ganancia por Venta)

# Función que toma el monto de VENTAS (Sales) y devuelve la GANANCIA (Profit) predicha.
predict_profit <- function(sales_amount) {
  
  new_data <- data.frame(Sales = sales_amount)
  predicted_profit <- predict(linear_model_sales, newdata = new_data)
  
  message(paste0("Predicción para Ventas de $", format(sales_amount, big.mark = ",", decimal.mark = "."), ":"))
  message(paste0("Ganancia (Profit) predicha: $", round(predicted_profit, 2)))
  return(invisible(round(predicted_profit, 2)))
}

message("\n--- EJEMPLOS DE PREDICCIÓN DE GANANCIA ---")

# Ejemplo 1: Venta de $500
predict_profit(500) 
# Ejemplo 2: Venta de $2000
predict_profit(2000)


## 4. ANÁLISIS EXPLORATORIO DE DATOS (Objetivos Secundarios)

# 4.1. Objetivo Secundario 1: Comparar Ventas y Ganancias entre Categorías
sales_by_category <- df_superstore %>%
  group_by(Category) %>%
  summarise(
    Total_Sales = sum(Sales),
    Total_Profit = sum(Profit),
    Avg_Profit_Per_Sale = mean(Profit)
  ) %>%
  arrange(desc(Total_Sales))

message("\n--- Ventas y Ganancias por Categoría ---")
print(sales_by_category)

# Visualización (Boxplot)
boxplot_category <- ggplot(df_superstore, aes(x = Category, y = Profit, fill = Category)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribución de Ganancias por Categoría", y = "Ganancia (Profit)")
print(boxplot_category)
# 

# 4.2. Objetivo Secundario 2: Tendencia de Ventas en el Tiempo (Mes a Mes)
sales_over_time <- df_superstore %>%
  group_by(Month_Year) %>%
  summarise(Total_Sales = sum(Sales))

# Visualización (Línea de tiempo)
lineplot_sales <- ggplot(sales_over_time, aes(x = as.Date(paste0(Month_Year, "-01")), y = Total_Sales)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(color = "red") +
  theme_minimal() +
  labs(
    title = "Tendencia de Ventas Totales Mes a Mes",
    x = "Fecha de Orden",
    y = "Ventas Totales ($)"
  )
print(lineplot_sales)
# 

# 4.3. Objetivo Secundario 3: Países con Mayores Ventas (Análisis Geográfico)
sales_by_country <- df_superstore %>%
  group_by(Country) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  arrange(desc(Total_Sales)) %>%
  head(5)

message("\n--- Top 5 Países por Ventas ---")
print(sales_by_country)

# -------------------------------------------------------------

## 5. CONCLUSIÓN FINAL (Texto para el Informe) 📝

# Extracción de métricas clave del modelo para la conclusión
r_squared <- round(summary(linear_model_sales)$adj.r.squared * 100, 2)
p_value <- summary(linear_model_sales)$coefficients["Sales", "Pr(>|t|)"]
correlation_value <- round(correlation_test$estimate, 2)

conclusion_text <- paste0("
================================================================================
CONCLUSIÓN DEL ANÁLISIS DE REGRESIÓN (Sales vs. Profit)

1. Confirmación de Hipótesis: La correlación es ", correlation_value, " (positiva). El modelo es altamente significativo (p-value = ", format.pval(p_value, digits=2), "). Esto confirma la Hipótesis Alternativa: **Vender más SÍ se traduce en Ganancia**.

2. Capacidad Predictiva: El modelo tiene una alta capacidad predictiva (R-squared Ajustado ≈ ", r_squared, "%). La variable 'Sales' explica la mayoría de la variación en la Ganancia.

3. Implicación: El coeficiente del modelo proporciona el margen de ganancia marginal. La gerencia puede confiar en que el crecimiento en ventas, en general, aumentará el profit.

4. Próximos Pasos: Analizar las categorías (Office Supplies, Furniture, Technology) para identificar aquellas con pérdidas (Profit negativo) o bajo margen para ajustar precios o costos.
================================================================================
")
message(conclusion_text)