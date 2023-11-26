library(pwr)

# Parámetros del análisis de poder
effect_size <- 0.16  # Tamaño del efecto
alpha <- 0.05      # Nivel de significancia
power <- 0.8       # Poder deseado

# Calcular el tamaño de la muestra necesario
n <- pwr.t.test(d = effect_size, sig.level = alpha, power = power)$n


effect_size <- 0.05  # Tamaño del efecto
alpha <- 0.05      # Nivel de significancia
power <- 0.08       # Poder deseado

# Calcular el tamaño de la muestra necesario
n <- pwr.t.test(d = effect_size, sig.level = alpha, power = power)$n
