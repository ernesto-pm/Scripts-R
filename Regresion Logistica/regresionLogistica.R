### Regresion Logistica
### Materiales > 4. Analisis... > Regresion Logistica > Regresion_logistica.pdf
### Se usa Un archivo que se llama NatalRiskData.rData
### Cargarlo y ahora esta dentro de la variable "sdata"

# Dividirlo basado en cierto parametro
train <- sdata[sdata$ORIGANDGROUP <= 5,]
test <- sdata[sdata$ORIGANDGROUP > 5,]

complications <- c("ULD_MECO", "ULD_PRECIP", "ULD_BREECH")
riskfactors <- c("URF_DIAB", "URF_CHYPER", "URF_PHYPER", "URF_ECLAM")

# Especificando x & y, notar que solo son Strings no valores reales
y <- "atRisk"
x <- c("PWGT", "UPREVIS", "CIG_REC", "GESTREC3", "DPLURAL", complications, riskfactors)

## Creando Modelo
formula <- paste(y, paste(x, collapse = "+"), sep = "~")
model <- glm(formula, data = train, family = binomial(link = "logit"))

# Creando nueva columna dentro de train
# Ver como se usa predict, se utiliza type "response" por que si no no da la prediccion
train$pred <- predict(model, newdata = train, type = "response")
test$pred <- predict(model, newdata = train, type = "response")

######### GRAFICANDO #########
library(ggplot2)
ggplot(train, aes(x = pred, color=atRisk, linetype = atRisk)) + geom_density()

## Falta mejorar el modelo y sacar coeficientes, a partir de la diapositiva #13
