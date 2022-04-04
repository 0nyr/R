# chargement du package pixmap qui peut être installé avec la commande
# install.packages("pixmap");
library(pixmap) ;

read_grey_img <- function(file) {
  img = read.pnm("images/hortensia.pgm");
  # chaque entrée de img@grey est une intensité de gris comprise entre 0 et 1
  img@grey;
}

compress_SVD <- function(X, nd) {
  svdX <- svd(X);
  svdX$u[,1:nd] %*% diag(svdX$d[1:nd]) %*% t(svdX$v[,1:nd]);
}

print_grey_img <- function(Y,...) {
  Y[Y<0]<-0; # après reconstruction approchée à partir du résultat du svd le
  Y[Y>1]<-1; # domaine [0,1] de la matrice initiale peut ne plus être respecté
  image(t(apply(Y,2,rev)), col=grey(seq(0,1,length=256)), axes=FALSE, ...)
}
