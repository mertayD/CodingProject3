#' Title
#'
#' @param X.mat
#' @param y.vec
#' @param max.iterations
#' @param step.size
#' @param n.hidden.units
#' @param is.train
#'
#' @return
#' @export
#'
#' @examples
#'
#' data(ozone, package="ElemStatLearn")
#' X.mat <- as.matrix(ozone[,-1])
#' y.vec <- as.vector(ozone[,1])
#' n.hidden.units <- 5
#' max.iterations <- 100
#' is.train <- TRUE
#' step.size <- 1
#' res <- NNetIterations( X.mat, y.vec, max.iterations, step.size, n.hidden.units, is.train)
#'
NNetIterations <- function(
  X.mat,
  y.vec,
  max.iterations,
  step.size,
  n.hidden.units, #u
  is.train
){
  if(!is.matrix(X.mat))
  {
    stop("Feature matrix is not a matrix")
  }
  
  X.unscaled.mat <- as.matrix(X.mat[,-1])
  X.scaled.mat <- scale(X.unscaled.mat)
  
  v <- matrix(rnorm(ncol(X.scaled.mat) * n.hidden.units), ncol(X.scaled.mat), n.hidden.units)
  w <- rnorm(n.hidden.units)

  is.binary <- all(y.vec %in% c(1, -1))

  pred.mat <- matrix(,nrow(X.scaled.mat), max.iterations)
  
  sigmoid <- function(a){
    1/(1 + exp(-a))
  }
  A <- X.scaled.mat %*% v #1
  z <- sigmoid(A) #2
  b <- as.numeric(z %*% w)
  pred.mat[, 1] <- b
  
  for(i in 2:max.iterations)
  {
    delta.w <- if(is.binary)
    {
      -y.train * sigmoid(-y.train * b)
    }
    else
    {
      b - y.vec
    }

    A.deriv <- z * (rep(1,n.hidden.units) - z)
    delta.v <- diag(delta.w) %*% A.deriv %*% diag(w)

    grad.w <- t(z) %*% delta.w / nrow(X.scaled.mat)
    grad.v <- t(X.scaled.mat) %*% delta.v / nrow(X.scaled.mat)
    
    ## take a step
    w <- as.numeric(w - step.size * grad.w)
    v <- v - step.size * grad.v
    
    A <- X.scaled.mat %*% v #1
    z <- sigmoid(A) #2
    b <- as.numeric(z %*% w)
    pred.mat[, i] <- b
  }
  v.unsc <- v/attr(X.scaled.mat, "scaled:scale")
  b.orig <- -t(v/attr(X.scaled.mat, "scaled:scale")) %*% attr(X.scaled.mat, "scaled:center")
  v.with.intercept <- rbind(intercept=as.numeric(b.orig), v.unsc)
  
  list(
    pred.mat=pred.mat,
    V.mat=v.with.intercept,
    w.vec=w,
    predict=function(testX.mat) {
      str(cbind(1, testX.mat))
      A.mat <- testX.mat %*% v.with.intercept
      Z.mat <- sigmoid(A.mat)
      pred.vec <- Z.mat %*% w
      return(pred.vec)
    }
  )
}
