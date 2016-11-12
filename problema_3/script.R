# Introduim les dades
X = matrix(c(rep(1,5),seq(5)),ncol=2)
t = c(7.97, 10.2, 14.2, 16.0, 21.2)

# Calculem la matriu pseudo-inversa
pseudo = solve(t(X) %*% X) %*% t(X)
w.pseudo = pseudo %*% t
