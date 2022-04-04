# 15 LOOCV

ridgeSVD <-
function(X, y)
{
  n <- nrow(X)
  X.init <- X
  y.init <- y
  X <- scale(X)
  ym <- mean(y)
  y <- y - ym
  Xs <- svd(X)
  d <- Xs$d
  function(lambda) {
    coef <- c(Xs$v %*% ((d / (d^2 + lambda)) * (t(Xs$u) %*% y)))
    coef <- coef / attr(X,"scaled:scale")
    inter <- ym - coef %*% attr(X,"scaled:center")
    coef <- c(inter, coef)
    trace.H <- sum(d^2 / (d^2 + lambda))
    yh <- coef[1] + X.init%*%coef[-1]
    gcv <- sum( ((y.init - yh) / (1 - (trace.H / n))) ^ 2 ) / n
    list(coef = coef, gcv = gcv)
  }
}

ridge <-
function(X, y, lambdas=NULL)
{
  X <- as.matrix(X)
  p <- ncol(X)
  if(is.null(lambdas)) { lambdas <- 10^seq(-8,8,by=0.5) }
  errs <- double(length(lambdas))
  coefs <- matrix(data = NA, nrow = length(lambdas), ncol = p+1)
  ridge <- ridgeSVD(X, y)
  idx <- 1
  for(lambda in lambdas) {
    res <- ridge(lambda)
    coefs[idx,] <- res$coef
    errs[idx] <- res$gcv
    idx <- idx + 1
  }
  err.min <- min(errs)
  lambda.best <- lambdas[which(errs == err.min)]
  coef.best <- coefs[which(errs == err.min),]
  yh <- coef.best[1] + X%*%coef.best[-1]
  mae <- mean(abs(yh - y))
  r <- list(coef = coef.best,
            lambda = lambda.best,
            mae = mae,
            coefs=coefs,
            lambdas=lambdas)
  class(r) <- "ridge"
  return(r)
}

predict.ridge <-
function(o, newdata)
{
  newdata <- as.matrix(newdata)
  if(length(o$coef)-1!=ncol(newdata)) {
    stop("Not the same number of variables btwn fitted ridge object and new data")
  }
  yh <- o$coef[1] + newdata%*%o$coef[-1]
}
