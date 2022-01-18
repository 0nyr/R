# this file contains test functions

# example of a function
rosace <- function(a,b,absolue=FALSE) { # FALSE by default
  # cette fonction sert a tracer des rosaces
  theta <- seq(0,4*pi*a,0.01)
  if(absolue==TRUE)
  {
    rho <- 1+b*abs(cos(a*theta))
  }else{
    rho <- 1+b*cos(a*theta)
  }
  plot(rho*exp(1i*theta),type='l') # type='l' sert a tracer des lignes continues et non des points
  return(list(angle=theta,rayon=rho))
}

suite <- function(a,n=100)
{
  # une suite constante egale a 1
  x <- 1
  for(i in 1:n)
  {
    x <- (a+1)*x-a
  }
  # on affiche la valeur obtenue
  4}

cat('valeur de x_n :', x, '\n')