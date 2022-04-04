# 02 Moindres carrés
# Résolution du système linéaire correspondant à la matrice de Gram pour un
# polynôme de degré fixé.
polyreg2 <- function(data, degre) {
  xs <- c(data$X)
  A = outer(xs, 0:degre, "^")
  gram = t(A) %*% A
  solve(gram, as.vector(t(A) %*% data$Y))
}
