# Introduim les dades
X = matrix(c(rep(1,5),seq(5)),ncol=2)
t = c(7.97, 10.2, 14.2, 16.0, 21.2)
print(X)
print(t)

# Calculem la matriu pseudo-inversa
pseudo = solve(t(X) %*% X) %*% t(X)
w.pseudo = pseudo %*% t
print(pseudo)
print(w.pseudo)

# Calcular svd de la matriu de disseny
svd = svd(X)
print(svd)

D = diag(2)*(1/svd$d)

