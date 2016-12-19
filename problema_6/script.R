rm(list = ls())

library(e1071)

printf <- function(fmt, ...) {
  cat(sprintf(fmt, ...))
}

numberize = function(x) {
  if (x == "a") return(0)
  else return(1)
}

colorize = function(x) {
  if (x == "a") return("red")
  else return("green")
}

probValue = function(x) {
  if (x > 1) return(1)
  else return(0)
}

data = seq(1,10)
classes = c('b', 'b', 'b', 'b', 'a', 'a', 'a', 'a', 'b', 'b')

minerror = Inf
mini = Inf
for (i in 1:6) {
  # Calcular el model
  svm.model = svm(x = data, y = classes, type = "C-classification", kernel = "polynomial", degree = i)
  svm.pred = factor(predict(svm.model), levels = c('a', 'b'))
  
  # Calcular l'error
  svm.table = table(svm.pred, classes)
  #print(svm.table)
  error = (1 - sum(diag(svm.table))/length(svm.pred))*100
  printf("Error for iteration %s: %s%%\n", i, error)
  
  if (minerror > error) {
    minerror = error
    mini = i
  }
}

printf("Best degree is: %s\n\n", mini)

dataF = data.frame(x = data, y = as.factor(sapply(classes, numberize)))
glm.model = glm(y ~ x, data = dataF, family = quasibinomial)
glm.pred = factor(lapply(exp(predict(glm.model)), probValue), levels = 0:1)

glm.tab = table(glm.pred, dataF$y)
error = (1 - sum(diag(glm.tab))/length(glm.pred))*100
printf("Error for glm regression: %s%%", error)


# Plot data
svm.model = svm(x = data, y = classes, type = "C-classification", kernel = "polynomial", degree = 2)
svm.pred = factor(predict(svm.model), levels = c('a', 'b'))

par(mfrow = c(2,1))
colors = sapply(classes, colorize)
plot(x = data, y = rep(0, length(data)), main = "Dades reals", col = colors, pch = 16)

colors = sapply(svm.pred, colorize)
plot(x = data, y = rep(0, length(data)), main = "Dades predites", col = colors, pch = 16)
par(mfrow = c(1,1))

# Aixi doncs es pot veure que amb una maquina de vectors suport utilitzant
# un kernel polinomic de grau 2 es la millor manera de reduir l'error de 
# training