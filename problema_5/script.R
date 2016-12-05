# Problema 11 Pràctica amb la xarxa MLP
rm(list=ls())

library(datasets)
library(nnet)
library(caret)

data(rock)
?rock

normalize <- function(x) {
  min = min(x)
  max = max(x)
  (x - min)/(max - min)
}

rock.x <- data.frame(area = normalize(rock$area), peri = normalize(rock$peri), shape = normalize(rock$shape))
rock.y <- normalize(log(rock$perm))

calc_network <- function(neurons, maxit=200, decay=0.1) {
  error = c()
  predicted = c()
  n = nrow(rock.x)
  
  for (i in 1:nrow(rock.x)) {
    print(i)
    test = rock.x[i,]
    test$y = rock.y[i]
    
    model.nnet <- nnet(rock.y[-i] ~., data = rock.x[-i,], size=neurons, maxit=maxit, decay=0.1, MaxNWts=100000)
    pred = predict(model.nnet, newdata = test)
    error[i] = (pred - test$y)^2
    predicted[i] = pred
  }  
  error
  plot(c(seq(n), seq(n)), c(predicted, rock.y), col=c(rep('red', n),rep('green', n)))
}

rocks = rock.x
rocks$y = rock.y

tc <- trainControl(method = "LOOCV")
fit <- train(y ~., data = rocks, method="nnet", maxit=200, trControl=tc, trace=FALSE)#, tuneGrid=expand.grid(size=seq(10)*2, decay=0))

predict(fit)
