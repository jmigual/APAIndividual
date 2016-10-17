# Netegem totes les variàbles anteriors
rm(list=ls())

# Útil per imprimir paràmetres per pantalla
printf <- function(...) { cat(sprintf(...), "\n") }

X <- matrix(c(c(1, 2), c(3, 3), c(3, 5), c(5, 4), c(5, 6), c(6, 5), c(8, 7), c(9, 8)), nrow = 2, byrow = FALSE)
print(X)

# Apartat 1
S <- matrix(nrow = 2, ncol = 2)

S[1,1] = var(X[1,])
S[2,2] = var(X[2,])
S[1,2] = S[2,1] = cov(X[1,], X[2,])

print(S)

# Apartats 2 i 3
vals <- eigen(S)
print(vals)

a1 <- vals$vectors[,1]
a2 <- vals$vectors[,2]
print(a1)
print(a2)

# Apartat 4
N <- ncol(X)
par(mfrow=c(1,1),pty="s")
plot(X[1,], X[2,],xlab='',ylab='',asp = 1)
abline(0,a1[1]/a1[2],col='black',lwd=2)
abline(8,a2[1]/a2[2],col='black',lwd=2)

# Apartat 5

percen = vals$values[1] / sum(vals$values) * 100
print(sum(vals$values))
print(percen)