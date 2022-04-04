# ML 1 01 Intro

# Exemple de fonction non-linéaire pour générer un jeu de données artificiel.
f <- function(x) {exp(x) * cos(2*pi*sin(pi*x))}

# Génération d'un jeu de données synthétique qui est l'image par f d'un
# échantillon uniforme de l'intervalle [0,1] auquel nous ajoutons un bruit
# gaussien de moyenne nulle et d'écart type sd.
gendat <- function(n, sd) {
  # n: nombre d'observations à générer
  # sd: écart type d'un bruit gaussien de moyenne nulle
  X = runif(n)
  Y = f(X) + rnorm(n, mean=0, sd=sd)
  dim(X) <- c(n,1) # en général chaque observation est décrite par plusieurs
  # variables et X est une matrice avec autant de lignes que
  # d'observations et autant de colonnes que de variables. Sur
  # notre exemple, chaque observation n'est décrite que par une
  # seule variable.
  list(X = X, Y = Y)
}

# Affichage simultanée du jeu de données bruité et de la courbe de la fonction
# utilisée pour le générer.
plt <- function(data, f, ...) {
  xs = seq(0,1,length.out=100)
  plot(xs, f(xs), type="l", ...)
  points(data$X, data$Y)
}

# Résolution du système linéaire correspondant à la matrice de Vandermonde.
# Autrement dit, découverte d'un polynôme qui passe par chaque point.
polyreg1 <- function(data) {
  xs <- c(data$X) # on transforme la matrice X, de dimension Nx1 sur notre
                  # exemple, en un vecteur
  vandermonde = outer(xs, 0:(length(xs)-1), "^")
  solve(vandermonde, data$Y)
}

# Evaluation d'un polynôme en un point.
polyeval <- function(coef,x) {
  powers = 0:(length(coef)-1)
  f = function(y) { sum(coef * y^powers) }
  sapply(x,f)
}

# Affichage de la courbe d'un polynôme défini par ses coefficients.
pltpoly <- function(coef) {
  xs = seq(0,1,length.out=100)
  lines(xs, polyeval(coef,xs), lty="dotted")
}
