# Instalación de la librería 'signal' si no está instalada
if (!require(signal)) {
  install.packages("signal")
}
library(signal)


# Leer el archivo CSV, especificando que se ignore la primera columna
datos <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Extraer solo la columna 'V1' (la columna con los datos que deseas suavizar)
data <- datos$X..V1.


# Verificar que 'data' tenga suficientes elementos
if (length(data) < 3) {
  stop("La longitud de los datos debe ser mayor que 2 para el suavizado.")
}

# Parámetro de suavización (lambda)
lambda <- 10000  # Ajusta este valor para más o menos suavizado

# Ajustar el valor de spar basado en lambda
spar_value <- 1 / lambda  # Calcular spar a partir de lambda (ajustar si es necesario)

# Verificar que spar_value sea válido
if (spar_value <= 0) {
  stop("El valor de spar debe ser positivo.")
}

# Aplicar suavizado con smooth.spline
smoothed_data <- smooth.spline(data, spar = spar_value)$y

# Imprimir los datos suavizados
cat("Datos suavizados:\n")
print(smoothed_data)

datagraph<-data.frame(
  y = data,
  y2= smoothed_data,
  x = seq_along(data), 
  x2 = seq_along(smoothed_data)
)
# Graficar los datos originales y los datos suavizados
ggplot(datagraph, aes(x = x, y = y)) +
  geom_line(aes(x = x, y = y), color = "red", size = 1) +  # Línea roja
  labs(
    title = "Gráfico de datos suavizados",
    x = "Índice",
    y = "Valores de Y"
  ) +
  geom_line(aes(x = x2, y = y2), color="blue", size = 1)
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

