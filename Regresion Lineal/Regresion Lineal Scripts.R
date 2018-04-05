load("psub(1).RData")
library(ggplot2)

# Crear set de datos
dtrain <- subset(psub, ORIGRANDGROUP >= 500)
dtest <- subset(psub, ORIGRANDGROUP < 500)

# Creando modelo usando logaritmo de la variable que queremos sacar
model <- lm(log(PINCP, base = 10) ~ AGEP + SEX + COW + SCHL, data = dtrain)

# Creando nueva columna con predicciones
dtest$predLogPINCP <- predict(model, newdata = dtest)
dtrain$predLogPINCP <- predict(model, newdata = dtrain)

# Graficar en x la prediccion y en Y el valor real
ggplot(data=dtest,aes(x=predLogPINCP,y=log(PINCP,base=10))) + 
  # Pintar los puntos de cada x & y
  geom_point(alpha=0.2,color="black") + 
  # Pintar la linea que se ajusta a esos puntos
  geom_smooth(aes(x=predLogPINCP, y=log(PINCP,base=10)),color="green") + 
  # Pintar la linea que intersecta a ese modelo, creo que esta es la linea del valor real
  geom_line(aes(x=log(PINCP,base=10), y=log(PINCP,base=10)),color="red",linetype=2) + 
  # Recortar la grafica
  scale_x_continuous(limits=c(4,5)) + scale_y_continuous(limits=c(3.5,5.5))


# Graficar mas adecuadamente, usando el erro residuial promedio: ver slide 12 en Regresion Lineal
ggplot(data=dtest,aes(x=predLogPINCP,y=predLogPINCP-log(PINCP,base=10))) +
  # Pintar puntos
  geom_point(alpha=0.2,color="black") +
  # Pintar error residual promedio
  geom_smooth(aes(x=predLogPINCP,y=predLogPINCP-log(PINCP,base=10)),color="black")

# Obteniendo R2, entre mas cercana a 1 mas accurate,
rsq <- function(y,f) {
  1 - sum((y-f)^2) / sum((y-mean(y))^2)
}
# Checar ambos resultados para train y test, ver slide 13 de Regresion Lineal
rsq(log(dtrain$PINCP, base=10), predict(model, newdata = dtrain))
rsq(log(dtest$PINCP, base=10), predict(model, newdata = dtest))


# Obtener coeficientes
coefficients(model)


