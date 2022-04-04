# 14 Géométrie Ridge

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

# Résolution d'un système linéaire correspondant à la matrice de Gram pour
# un polynôme de degré fixé et avec l'ajout d'un facteur de régularisation en
# norme L2 dont l'importance est contrôlée par l'hyperparamètre alpha.
ridge.gram <- function(alpha, data, degre) {
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

ridge.svd <- function(data, degre, fold = FALSE) {
  if (length(fold) == 1 && fold == FALSE) {
    X <- data$X
    Y <- data$Y
  } else {
    X <- data$X[!fold,]
    Y <- data$Y[!fold]
  }
  A <- scale(outer(c(X), 1:degre, "^"))
  Ym <- mean(Y)
  Y <- Y - Ym
  As <- svd(A)
  d <- As$d
  function(alpha) {
    coef <- c(As$v %*% ((d / (d^2 + alpha)) * (t(As$u) %*% Y)))
    coef <- coef / attr(A,"scaled:scale")
    inter <- Ym - coef %*% attr(A,"scaled:center")
    coef <- c(inter, coef)
  }
}

# alphas[l] est une liste de valeurs pour l'hyperparamètre alpha.
# Notons Ridge[l] un modèle avec alpha <- alphas[l].
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
kfoldridge <- function(K, alphas, data, degre) {
  N <- nrow(data$X)
  folds <- rep_len(1:K, N)
  folds <- sample(folds, N)
  maes <- matrix(data = NA, nrow = K, ncol = length(alphas))
  colnames(maes) <- alphas
  for(k in 1:K) {
    fold <- folds == k
    ridge <- ridge.svd(data, degre, fold)
    alpha_idx <- 1
    X <- data$X[fold,]
    Y <- data$Y[fold]
    for(alpha in alphas) {
      coef <- ridge(alpha)
      pred <- polyeval(coef, X)
      maes[k,alpha_idx] <- mean(abs(pred - Y))
      alpha_idx <- alpha_idx + 1
    }
  }
  mmaes <- colMeans(maes)
  minmmaes <- min(mmaes)
  bestalpha <- alphas[which(mmaes == minmmaes)]
  ridge <- ridge.svd(data, degre)
  coef <- ridge(bestalpha)
  list(coef = coef, maes = maes, alpha = bestalpha)
}
