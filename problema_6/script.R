rm(list = ls())

library(e1071)

printf <- function(fmt, ...) {
  cat(sprintf(fmt, ...))
}

data = seq(1, 10)
classes = c('b', 'b', 'b', 'b', 'a', 'a', 'a', 'a', 'b', 'b')

for (i in 1:10) {
  printf("Iteration: %s\n", i)
  # Calcular el model
  svm.model = svm(x = data, y = classes, type = "C-classification", kernel = "polynomial", degree = i)
  svm.pred = factor(predict(svm.model), levels = c('a', 'b'))
  
  # Calcular l'error
  svm.table = table(svm.pred, classes)
  print(svm.table)
  error = (1 - sum(diag(svm.table))/length(svm.pred))*100
  printf("Error for iteration %s: %s%%\n", i, error)
  printf("\n")
}
