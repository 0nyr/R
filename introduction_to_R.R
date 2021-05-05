# execute a cmd in a R script file : CTRL+ENTER on the line

## basic syntax

# basis computations
cos(pi/3)^2*sqrt(2)

# variable assignment
a <- cos(pi/3)^2*sqrt(2)

# erase variables in memory
rm(list=ls())

# create vectors
x <- c(0,2,4)
y <- c(1,7,4)

# put vectors end to end
c(x,y)

# put vectors side by side in a matrix
cbind(x, y)

# create a constant vector
z <- rep(4,8)

# create matrix of size 2x4
M <- matrix(x,2,4)

# create matrix of dim higher than 2
T <- array(x, dim=c(2,2,2))

# access elements inside matrix
x[2]
M[1,4]
M[(M>2)]
M[,4] # give the 4th column of the matrix 

# create a list
L <- list(nom='toto',age=21,notes=c(14,7,12),vect=seq(1,4,by=0.5))

# access elements from a list
L$vect

# get names inside the list
names(L)

# get a summary of what a list contains
str(L)

# normal operations are done term by term
8*c(8,88,888,8888)+13

# to specify matrix operations, surround operators with %
x <- c(2,3,0)
A <- cbind(c(1,8,7),c(0,1,7),c(8,4,0))
A*x # term by term
A%*%x # real matrix multiplication

# inversion of matrix
solve(A)

# get help of a function f 
?f
help(f)

## graphs

# open a new graph window
x11()

# cut the window in 2 rows, 3 columns, graphs filled successively
par(mfrow=c(2,3))

# syntax of a graph
plot(x,y,main='Titre principal',xlab='axe des x',ylab='axe des y')

# NB - other characteristics are available like 
type='1' # give a continuous representation of y with respect to x

# close all graphical windows
graphics.off()

## programming

# call another file for functions
source('introduction_to_R_functions.R')

# call of a function from the file
res <- rosace(9/4,1,FALSE)
res$angle
res$rayon
rosace(8.7,10)
suite(1.3)
suite(127.8)

# R passes the arguments as a copy, here a local != a global
aha <- function(a=0){
  a <- a+1
  cat('valeur de a dans la fonction :', a, '\n')
}
a <- 4
aha(a)
cat('valeur de a :', a, '\n')

## packages

# install a packet
install.packages

# install the 'MASS' packet
install.packages('MASS')

# load the 'MASS' packet into the session
library('MASS')

# error : replacement has length zero
## this happens when using a[0] which is always numeric(0)
## instead, a rep(7,10) starts at index 1 (not 0) and ends at 10
test1 <- rep(7,10)
cat(test1[11])

