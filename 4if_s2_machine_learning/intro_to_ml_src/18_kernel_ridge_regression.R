gausskernel <-
function(X, sigma2)
{
  return(exp(-1*as.matrix(dist(X)^2)/sigma2))
}

# For X a square matrix, efficient impl of X %*% diag(d)
multdiag <-
function(X,d)
{
  R <- matrix(NA, nrow=dim(X)[1], ncol=dim(X)[2])
  for (i in 1:dim(X)[2]) { R[,i]=X[,i]*d[i] }
  return(R)
}

krr <-
function(X, y, sigma2=NULL, lambdas=NULL)
{
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  if(is.null(lambdas)) { lambdas <- 10^seq(-8, 2,by=0.5) }
  if(is.null(sigma2)) { sigma2 <- p }

  X <- scale(X)
  y <- scale(y)

  K <- gausskernel(X, sigma2=sigma2)
  eig <- eigen(K, symmetric=TRUE)

  qty <- matrix(NA,n,n)
  qty <- crossprod(eig$vectors, y)

  looe <- double(length(lambdas))
  coef <- matrix(data = NA, nrow = n, ncol = length(lambdas))
  i <- 1
  for(lambda in lambdas) {
    diag <- 1/(eig$values + lambda)
    qdiag <- multdiag(eig$vectors, diag)
    coef[,i] <- qdiag %*% qty
    ginvdiag <- rowSums(multdiag(eig$vectors^2, diag))
    looe[i] <- mean((coef[,i]/ginvdiag)^2)
    i <- i+1
  }
  looe.min <- min(looe)
  lambda <- lambdas[which(looe == looe.min)]
  coef <- coef[,which(looe == looe.min)]
  yh <- K%*%coef
  yh <- yh * attr(y,"scaled:scale") + attr(y,"scaled:center")

  r <- list(K=K,
            X=X,
            y=y,
            sigma2=sigma2,
            coef=coef,
            looe=looe.min,
            lambda=lambda,
            yh=yh
           )
  class(r) <- "krr"
  return(r)
}

predict.krr <-
function(o, newdata)
{
  if(class(o) != "krr") {
    warning("Object is not of class 'krr'")
    UseMethod("predict")
    return(invisible(NULL))
  }
  newdata <- as.matrix(newdata)
  if(ncol(o$X)!=ncol(newdata)) {
    stop("Not the same number of variables btwn fitted krr object and new data")
  }
  newdata <- scale(newdata,center=attr(o$X,"scaled:center"),
                   scale=attr(o$X,"scaled:scale"))
  n <- nrow(o$X)
  nn <- nrow(newdata)
  K <- gausskernel(rbind(newdata,o$X),sigma2=o$sigma2)[1:nn,(nn+1):(nn+n)]
  K <- matrix(K,nrow=nn,byrow=FALSE)
  yh <- K%*%o$coef
  yh <- (yh * attr(o$y,"scaled:scale")) + attr(o$y,"scaled:center")
}


# Tests

test.multdiag <-
function()
{
  A <- matrix(seq(from=1, to=5*5, by=1), nrow=5)
  d <- seq(from=1, to=5, by=1)
  B <- multdiag(A,d)
  C <- A %*% diag(d)
  identical(B,C)
}
