# Función para calcular la norma de un vector
norma <- function(v) {
  return(sqrt(sum(v^2)))
}

# Definición del sistema de ecuaciones no lineales
F <- function(x) {
  f1 <- x[1]^2 + x[2] - 37
  f2 <- x[1] - x[2]^2 - 5
  f3<- x[1] + x[2] + x[3] - 3
  return(c(f1, f2,f3))
}

# Método de Broyden para SENoL
broyden_senol <- function(F, x0, tol = 1e-6, max_iter = 100) {
  x <- x0
  n <- length(x0)
  A <- diag(n) # Matriz inicial (identidad)
  
  for (i in 1:max_iter) {
    Fx <- F(x)
    delta_x <- solve(A, -Fx)
    x_new <- x + delta_x
    
    # Verificar si la norma del cambio es menor a la tolerancia
    if (norma(delta_x) < tol) {
      message(sprintf("Error al invertir la matriz en la iteración %d", i))
      return(x_new)
    }
    
    # Actualizar la matriz A usando la fórmula de Broyden (ajuste para evitar errores de dimensiones)
    delta_F <- F(x_new) - Fx
    A <- A + ( (delta_F - A %*% delta_x) %*% t(delta_x) ) / sum(delta_x^2)
    
    # Actualizar x
    x <- x_new
  }
  cat("No se alcanzó la convergencia después de", max_iter, "iteraciones\n")
  return(x)
}

# Ejemplo de uso del método de Broyden
x0 <- c(0,0,0)
sol <- broyden_senol(F, x0)
cat("Solución encontrada:", sol, "\n")
