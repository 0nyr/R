# 12 Factorisation QR

# Norm 2
Norm <- function(x) {
  stopifnot(is.numeric(x))
  sqrt(sum(x^2))
}

# Modified Gram-Schmidt
mgs <- function(A, tol = .Machine$double.eps^0.5) {
  stopifnot(is.numeric(A), is.matrix(A))
  M <- nrow(A); N <- ncol(A)
  if(M<N) stop("Le nombre de lignes de 'A' doit être supérieur ou égal au nombre
               de colonnes")
  Q <- matrix(0, M, N)
  R <- matrix(0, N, N)
  for (i in 1:N) Q[,i] <- A[,i]
  for (i in 1:N) {
    R[i,i] <- Norm(Q[,i])
    if (abs(R[i,i]) <= tol) stop("La matrice 'A' n'est pas de plein rang.")
    Q[,i] <- Q[,i] / R[i,i]
    j <- i+1
    while(j<=N) {
      R[i,j] <- t(Q[,i]) %*% Q[,j]
      Q[,j] <- Q[,j] - R[i,j] * Q[,i]
      j <- j+1
    }
  }
  return(list(Q = Q, R = R))
}
