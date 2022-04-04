# 03 Tikhonov
# Résolution d'un système linéaire correspondant à la matrice de Gram pour
# un polynôme de degré fixé et avec l'ajout d'un facteur de régularisation en
# norme L2 dont l'importance est contrôlée par l'hyperparamètre alpha.
ridge <- function(alpha, data, degre) {
  A <- scale(outer(c(data$X), 1:degre, "^"))
  Y <- data$Y
  Ym <- mean(Y)
  Y <- Y - Ym
  gram <- t(A) %*% A
  diag(gram) <- diag(gram) + alpha
  coef <- solve(gram, as.vector(t(A) %*% Y))
  coef <- coef / attr(A,"scaled:scale")
  inter <- Ym - coef %*% attr(A,"scaled:center")
  coef <- c(inter, coef)
}
