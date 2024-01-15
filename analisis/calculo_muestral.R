# Ojo que puede cambiar a 14 o 15 la minima diferencia clinicamente relevante -> "@VanDerVlist2020" // "@Lagas2021" 
# En el proyecto se mantiene el valor viejo de 102 pacientes

# Cargar librerias
library(pwr)

# Definir las variables, "V" es la diferncia entre la medicion 1 y 2. Deberia serl edelta o la diferncia minima clinicamente significativa.

# m1 <- 80
# m2 <- 70    valores viejos donde la DMCS era de 10 puntos
v  <- 14
sd <- 16    # es el desvio estandar

valor = (v)/sd

# Calculo muestral con un poder del 80% y nivel de significacina en 0.05

pwr.t.test(n = NULL, d = valor, sig.level = 0.05, power = 0.8, 
           type = c("two.sample"),
           alternative = c("two.sided")
)
