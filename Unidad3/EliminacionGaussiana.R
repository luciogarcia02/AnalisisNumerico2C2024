# Función para resolver el sistema de ecuaciones
resolver_sistema <- function(A) {
  n <- nrow(A)  # Número de incógnitas / ecuaciones
  
  # Paso 1: Para i = 1,..., n - 1 haga los pasos 2–4.
  for (i in 1:(n - 1)) {
    # Paso 2: Encontrar el entero p más pequeño con i ≤ p ≤ n tal que api ≠ 0
    found_p <- FALSE
    for (p in i:n) {
      if (A[p, i] != 0) {
        found_p <- TRUE
        # Paso 3: Si p = i, entonces intercambiar las filas Ep ↔ Ei
        if (p != i) {
          A[c(i, p), ] <- A[c(p, i), ]
        }
        break
      }
    }
    
    if (!found_p) {
      return("no existe una solución única, hay una columna de ceros.")  # SALIDA si no se puede encontrar un p
    }
    
    # Paso 4: Para j = i + 1,..., n
    for (j in (i + 1):n) {
      # Paso 5: Determinar mji = aji / aii
      m_ji <- A[j, i] / A[i, i]
      # Paso 6: Ejecutar (Ej - mji Ei) → (Ej)
      A[j, ] <- A[j, ] - m_ji * A[i, ]
    }
  }
  
  # Paso 7: Verificar si ann = 0
  if (A[n, n] == 0) {
    return("no existe una solución única")  # SALIDA si ann = 0
  }
  
  # Paso 8: Determinar xn = an,n+1 / ann
  x <- numeric(n)
  x[n] <- A[n, n + 1] / A[n, n]
  
  # Paso 9: Para i = n - 1,..., 1
  for (i in (n - 1):1) {
    suma <- A[i, n + 1]
    for (j in (i + 1):n) {
      suma <- suma - A[i, j] * x[j]
    }
    x[i] <- suma / A[i, i]
  }
  
  # Paso 10: SALIDA (x1,..., xn)
  return(x)
}


resolver_sistema(A)