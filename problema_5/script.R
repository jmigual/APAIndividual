# Problema 11 Pràctica amb la xarxa MLP
rm(list=ls())

library(datasets)
library(nnet)
library(caret)

normalize <- function(x) {
  min = min(x)
  max = max(x)
  (x - min)/(max - min)
}

# Funció per crear el gràfic per visualitzar les dades i l'error
plot_error <- function(real, predicted) {
  n = length(real)
  error = (predicted - real)^2
  plot(seq(n), predicted, ylab = "Valor", xlab = "Dada", col='red', ylim=c(0,1))
  points(seq(n), error, col = 'blue')
  points(seq(n), real, col = 'green')
  legend("topleft", legend=c("Predicted", "Original", "Error"), fill=c("red", "green", "blue"))
}

data(rock)
?rock

# Dades a predir
rock.x <- data.frame(area = normalize(rock$area), peri = normalize(rock$peri), shape = normalize(rock$shape))
rock.y <- normalize(log(rock$perm))

# Funciói usada per comprova una sola xarxa neuronal
calc_network <- function(neurons, maxit=200, decay=0.1) {
  error = c()
  predicted = c()
  
  for (i in 1:nrow(rock.x)) {
    print(i)
    test = rock.x[i,]
    test$y = rock.y[i]
    
    model.nnet <- nnet(rock.y[-i] ~., data = rock.x[-i,], size=neurons, maxit=maxit, decay=0.1, MaxNWts=100000)
    pred = predict(model.nnet, newdata = test)
    error[i] = (pred - test$y)^2
    predicted[i] = pred
  }  
  plot_error(rock.y, predicted)
  return(error)
}

rocks = rock.x
rocks$y = rock.y

# Utilitzar Leave One Out Cross Validation per buscar l'error de la xarxa i així trobar la que millor s'ajusta
# a les dades. Mitjançant train() es pot trobara quest paràmetre
tc <- trainControl(method = "LOOCV")
fit <- train(y ~., data = rocks, method="nnet", maxit=200, trControl=tc, trace=FALSE)
print(fit)

pred = predict(fit)
plot_error(rock.y, pred)

# Un cop entrenada la xarxa es veu que el millor resultat és amb 1 neurona i no usar regularització
