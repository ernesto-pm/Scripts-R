############# Primer Example #############

load("Arboles de decision/GCDData(1).RData")

# Crear el arbol
model <- rpart(Good.Loan ~ Duration.in.month + installment.rate.in.percentage.of.disposable.income +
                 Credit.amount + Other.installment.plans,
                 data = d, 
                 control = rpart.control(maxdepth = 4),
                 method = "class")

# Graficar el arbol
rpart.plot(model, type = 1, extra = 100, cex = .7, box.col=c("gray99", "gray88")[model$frame$yval])

# Hacer un resumen de Good.Loan y proposito
# Aqui saca la cuenta de cuantos purpose hay por goodloan
table(d$Purpose, d$Good.Loan) 

# Crear Matriz de confusion
resultframe <- data.frame(Good.Loan = creditdata$Good.Loan, pred = predict(model, type="class"))
rtab <- table(resultframe)
rtab

# Precision general del modelo
sum(diag(rtab))/sum(rtab)

# Precision del modelo
sum(rtab[1,1]) / sum(rtab[,1])

# El modelo encontro 14% de los prestamos morosos
sum(rtab[1,1]) / sum(rtab[1,])

# Tasa de falsos positivos
sum(rtab[2,1]) / sum(rtab[2,])


############# Segunda Example #############

library(C50)
library(rpart)
library(rpart.plot)

# Cargar tablas
data(churn)

# Variables elegidas
Variables <- c(4,7,16,19,17,20)

# Scando las variables basado en numeros
Entrenamiento <- churnTrain[,Variables]

Test <- churnTest[,Variables]

# https://stackoverflow.com/questions/13446256/meaning-of-tilde-dot-argument
ModeloArbol<-rpart(churn ~.,data=Entrenamiento,parms=list(split="information"))
ModeloArbol

# Graficar
rpart.plot(ModeloArbol, type = 1, extra = 100, cex = .7, box.col = c("gray99", "gray88")[ModeloArbol$frame$yval])

# Predecir
prediccion <- predict(ModeloArbol, Test, type = "class")

# Matriz de confusion
MC <- table(Test[, "churn"],prediccion) 

# Validacion cruzada
Variables <- c(4,7,16,19,17,20)
datos <- churnTrain[, Variables]
datos <- rbind(datos, churnTest[,Variables])

# Folds
set.seed(1)
Folds <- 10
datos$kfold <- sample(1:Folds, nrow(datos), replace = T)

# Iteracion modelos + prediccion
Iter <- data.frame(iteracion = NULL, aciertos = NULL)
for(i in 1:Folds) {
  Test <- subset(datos, kfold == i)
  Entrenamiento <- subset(datos, !kfold == i)
  Modelo <- rpart(churn ~., data = Entrenamiento)
  Prediccion <- predict(Modelo, Test, type = "class")
  MC <- table(Test[,"churn"], Prediccion)
  Aciertos <- MC[1,1]/ (MC[1,1] + MC[2,1])
  Iter <- rbind(Iter, data.frame(Iter = i, acierto = Aciertos))
}


# Graficar todas las iteraciones
promedio <- format(mean(Iter$acierto, na.rm = TRUE)*100, digits = 4)
plot(Iter, type = "b", main = "% Prediccion en Cada Iteracion", cex.axis = .7, cex.lab = .7, cex.main = .8,
     xlab = "No de iteraciones", ylab = "% Prediccion")
abline(h = mean(Iter$acierto), col="blue", lty=2)
legend("topright", legend = paste("Eficiencia de prediccion =", promedio, "%"), col="blue", lty = 2, lwd = 1, cex = .7, bg = NULL)

