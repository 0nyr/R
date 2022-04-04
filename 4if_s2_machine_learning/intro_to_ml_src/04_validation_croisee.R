# 04 Validation croisée
# Séparer le jeu de données en un jeu d'entraînement et un jeu de test
# INPUT : jeu de données initial et proportion des données conservées pour
#         l'entraînement.
splitdata <- function(data,p) {
  n <- nrow(data$X)
  nentr <- round(p*n)
  entridx <- sample(1:n, nentr, replace=FALSE)
  list(entr  = list(X = data$X[entridx,,drop=FALSE],  Y = data$Y[entridx]),
       test = list(X = data$X[-entridx,,drop=FALSE], Y = data$Y[-entridx]))
}

# lambdas[l] est une liste de valeurs pour l'hyperparamètre lambda.
# Notons Ridge[l] un modèle avec lambda <- lambdas[l].
# Découper aléatoirement le jeu d'entraînement en K plis F[i] disjoints.
# Pour l <- [1,...,L]
#   Pour i <- [1,...,K]
#     Apprendre Ridge[l] sur l'union des plis F[j] avec j!=i
#     Calculer le score de Ridge[l] sur le pli de validation F[i]
#   Conserver les résultats du modèle sur les plis de validation.
#   Soit Moy[l] la moyenne des résultats de Ridge[l] sur les plis de validation.
# Soit l' l'indice du maximum de Moy[l]
# Apprendre Ridge[l'] sur l'ensemble du jeu de données d'entraînement.
# Retourner ce modèle.
kfoldridge <- function(K, lambdas, data, degre) {
  N <- nrow(data$X)
  folds <- rep_len(1:K, N)
  folds <- sample(folds, N)
  maes <- matrix(data = NA, nrow = K, ncol = length(lambdas))
  colnames(maes) <- lambdas
  lambda_idx <- 1
  for(lambda in lambdas) {
    for(k in 1:K) {
      fold <- folds == k
      coef <- ridge(lambda, data, degre, fold)
      pred <- polyeval(coef, data$X[fold,])
      maes[k,lambda_idx] <- mean(abs(pred - data$Y[fold]))
    }
    lambda_idx <- lambda_idx + 1
  }
  mmaes <- colMeans(maes)
  minmmaes <- min(mmaes)
  bestlambda <- lambdas[which(mmaes == minmmaes)]
  fold <- folds == K+1 # vector of FALSE
  coef <- ridge(bestlambda, data, degre, fold)
  list(coef = coef, maes = maes, lambda = bestlambda)
}

# Résolution d'un système linéaire correspondant à la matrice de Gram pour
# un polynôme de degré fixé et avec l'ajout d'un facteur de régularisation en
# norme L2 dont l'importance est contrôlée par l'hyperparamètre lambda.
# Les éléments du jeu de données indiqués par le vecteur booléen fold ne sont
# pas utilisés pour l'apprentissage du modèle. Cela permet d'implémenter une
# validation croisée à plusieurs plis.
ridge <- function(lambda, data, degre, fold) {
  xs <- c(data$X[!fold,])
  A <- outer(xs, 0:degre, "^")
  gram <- t(A) %*% A
  diag(gram) <- diag(gram) + lambda
  solve(gram, as.vector(t(A) %*% data$Y[!fold]))
}
