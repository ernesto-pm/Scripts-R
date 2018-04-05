# Forma de matrices aqui y en Python: cosa[row, column]

## Crear un dataset igual al original que cumple ciertas condiciones por fila
train <- sdata[sdata$ORIGANDGROUP <= 5,]

## Crear un dataset igual al original que cumple ciertas condiciones por columna
train <- sdata[,sdata$ORIGANDGROUP <= 5]

## Transformar datos dentro de una columna en otro valor
marco$sentiment[marco$sentiment == 'NONE'] <- 'N'

## Transformar NAs en 0 (Sin Quitarlos)
renta[is.na(renta)] <- 0

## Quitar NAs (Quitandolos)
na.omit(cosa) #creo que es asi

#obtener de la fila 1 a 300, en todas las columnas
mat[1:300,]

#obtener de la fila 1 a 300, en la columna 2
mat[1:300,2]

# Obtener todas las rows pero solo la columna 3
mat[,3]

# Tilde -> ~~~~~
# Formula object

## Table: crea una tabla con cuantos valores tienes por cada columna en tu set de datos
# http://www.dummies.com/programming/r/how-to-use-data-tables-in-r/
table(d$Purpose, d$Good.Loan) 

# Scando las variables basado en numeros
Variables <- c(4,7,16,19,17,20)
Entrenamiento <- churnTrain[,Variables]

# Sacar cuantas rows y cuantas columnas
dim(churnTrain)

# Recorrer todas las columnas, desde la columna 2 hasta el final
for(i in 2:ncol(HouseVotes84)) {
  
}

# Que rows son democratas y ademas son NA
which(is.na(HouseVotes84[,2]) & HouseVotes84$Class == "democrat", arr.ind = TRUE)

# Scar el nombre de todas las columnas en el dataset
names(HouseVotes84)

# Copiar todas las columnas excepto alguna (la que tiene el -)
trainHouseVotes84 <- HouseVotes84[HouseVotes84$train == 1, -10]

# COmo partir dataset en 80-20% para entreanmiento y pruebas
HouseVotes84[,'train'] <- ifelse(runif(nrow(HouseVotes84)) < 0.8, 1, 0)
trainColNum <- grep('train', names(HouseVotes84))
trainHouseVotes84 <- HouseVotes84[HouseVotes84$train == 1, -trainColNum] # 80%
testHouseVotes84 <- HouseVotes84[HouseVotes84$train == 0, -trainColNum] # 20%
