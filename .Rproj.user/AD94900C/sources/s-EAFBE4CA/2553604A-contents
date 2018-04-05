library(mlbench)
library(e1071)

data('HouseVotes84')

# Plotting
plot(as.factor(HouseVotes84[,2]))
title(main = "Votes cast for issue", xlab = "vote", ylab = "# reps")

# Por partido
plot(as.factor(HouseVotes84[HouseVotes84$Class == "republican", 2]))
title(main = "Republican votes for issue 1", xlab = "vote", ylab = "# reps")

# Regresa el numero de nas en la clase y columna especificados
na_by_col_class <- function(col, cls) {
  return (sum(is.na(HouseVotes84[,col]) & HouseVotes84$Class == cls))
}

# Calcular probabilidad que el miembro haga un voto "y" en la clase especificada
p_y_col_class <- function(col, cls) {
  sum_y <- sum(HouseVotes84[,col] == "y" & HouseVotes84$Class==cls, na.rm = TRUE)
  sum_n <- sum(HouseVotes84[,col] == "n" & HouseVotes84$Class==cls, na.rm = TRUE)
  
  return(sum_y/(sum_y + sum_n))
}

# Que un republicano vote como si en la columna 2
p_y_col_class(2, "republican")
na_by_col_class(2, "democrat")
na_by_col_class(2, "republican")

# Basado en la informacion de arriba llenamos los NAs usando esta funcion
# Poner y en un NA si el numero aleatorio devuelto es menor a la probabilidad de un voto afirmativo
# Poner en n en un NA si el numero aleatorio devuelto es mayor a la probabilidad de un voto afirmativo
for(i in 2:ncol(HouseVotes84)) {
  # Si hay NAs
  if(sum(is.na(HouseVotes84[,i]) > 0)){
    
    # Which saca que row indices de esa columna son NA y ademas la clase es democrata
    # Regresa algo asi: 3 105 130 144 179 181 184 391 429
    c1 <- which(is.na(HouseVotes84[,i]) & HouseVotes84$Class == "democrat", arr.ind = TRUE)
    
    # Que rows son NA y republicanos
    c2 <- which(is.na(HouseVotes84[,i]) & HouseVotes84$Class == "republican", arr.ind = TRUE)
    
    # Llenar nas para democratas
    HouseVotes84[c1,i] <-ifelse(runif(na_by_col_class(i,"democrat"))<p_y_col_class(i,"democrat"),"y","n")
    # Llenar nas para republicanos
    HouseVotes84[c2,i] <-ifelse(runif(na_by_col_class(i,"republican"))<p_y_col_class(i,"republican"),"y","n")
    
  }
}

# Dividir en datasets de entrenamiento y prueba
# Dividir uniformemente entre 80 y 20% los datos para set de pruebas y de entrenamiento
HouseVotes84[,'train'] <- ifelse(runif(nrow(HouseVotes84)) < 0.8, 1, 0)

# Saca que numero de indice es la columna que se llama train lol khe abanzado
trainColNum <- grep('train', names(HouseVotes84))

# Crear los dos set de datos
trainHouseVotes84 <- HouseVotes84[HouseVotes84$train == 1, -trainColNum] # 80%
testHouseVotes84 <- HouseVotes84[HouseVotes84$train == 0, -trainColNum] # 20%

# Crear modelo 
nb_model <- naiveBayes(Class~., data = trainHouseVotes84)
# Esto no sirve pa na
summary(nb_model)
str(nb_model)

# ..... y el momento de reckoning jijijijiji joojojoj khe mizteriozo
nb_test_predict <- predict(nb_model, testHouseVotes84[,-1]) # usar todas las columnas excepto la 1 (que es la clase)
table(pred = nb_test_predict, true = testHouseVotes84$Class)

# precision del modelo
# orale esta mas potente nuestro modelo, nuestro Ragnar ___100tbr00k____
mean(nb_test_predict == testHouseVotes84$Class)

# Mejorando los datos (?)
nb_multiple_runs <- function(train_fraction, n) {
  # replica NA, n veces -> regresa algo asi  NA NA NA si le pasas n como 3
  fraction_correct <- rep(NA,n)
  for(i in 1:n) {
    # desde 1 hasta n...
    # Osea repite el proceso de hacer todo N veces
    HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84)) < train_fraction, 1, 0)
    trainColNum <- grep("train", names(HouseVotes84))
    trainHouseVotes84 <- HouseVotes84[HouseVotes84$train == 1, -trainColNum]
    testHouseVotes84 <- HouseVotes84[HouseVotes84$train == 0, -trainColNum]
    nb_model <- naiveBayes(Class~.,data = trainHouseVotes84)
    nb_test_predict <- predict(nb_model, testHouseVotes84[,-1])
    fraction_correct[i] <- mean(nb_test_predict == testHouseVotes84$Class)
  }
  
  return(fraction_correct)
}

multiples_predicciones <- nb_multiple_runs(0.8, 20)
multiples_predicciones

summary(multiples_predicciones)

# Desviacion estandar
sd(multiples_predicciones)

