# operation element par element
8*c(8,88,888,8888)+13

(x <- c(2,3,0))
(A <- cbind(c(1,8,7),c(0,1,7),c(8,4,0))) 
(A <- matrix(c(1,8,7,0,1,7,8,4,0),ncol=3,nrow=3)) 
A[1,2]
A[1,]
A[(A>2)]

A*x
# operations matricielles
A%*%x
solve(A)%*%(A%*%x)

########################################################

# listes
L <- list(nom='toto',age=21,notes=c(14,7,12),vect=seq(1,4,by=0.5))
L$vect
str(L)

########################################################

# fonction
aha <- function(a){
  a <- a+1
  return(a+1)
}
a <- 4
aha(a)
a


##################################################

rosace <- function(a,b,abs=FALSE)
{
  # le # sert a mettre des commentaires
  # cette fonction sert a tracer des rosaces
  
  theta <- seq(0,4*pi*a,0.01)
  
  if(abs==TRUE)
  {
    rho <- 1+b*abs(cos(a*theta))
  }else{
    rho <- 1+b*cos(a*theta)
  } 
  
  plot(rho*exp(1i*theta),type='l') # type='l' sert a tracer des lignes continues et non des points
}

rosace(9/4,1)
rosace(9/4,1,TRUE)
rosace(8.7,10)
#####################################################

suite <- function(a,n=100)
{
  # une suite constante egale a 1
  x <- 1
  for(i in 1:n)
  {
    x <- (a+1)*x-a 
  }
  
  # on affiche la valeur obtenue
  cat('valeur de x_n :', x, '\n')
}
# on essaie ? (avec des nombres pas pris au hasard ici !)
suite(1.3)
suite(127.8)
# conclusion ?

