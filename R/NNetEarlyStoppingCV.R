#' NNetEarlyStoppingCV
#'
#' This function uses cross fold validatoion to find the percision of the 
#' NNetEarlyStoppingCV function
#'
#' @param X.mat numeric input feature matrix [n x p]
#' @param Y.vec numeric input label vetor [n]
#' @param fold.vec a vector of fold ids
#' @param max.iterations scalar integer, max number of iterations
#' @param n.hidden.units The number of hidden units, U
#'
#' @return Output: list with named elements:
#' pred.mat               n_observations x max.iterations matrix of predicted values (real number for regression, probability for binary classification).
#' V.mat                  final weight matrix (n_features+1 x n.hidden.units). The first row of V.mat should be the intercept terms.
#' w.vec                  final weight vector (n.hidden.units+1). The first element of w.vec should be the intercept term.
#' predict(testX.mat)     a function that takes an unscaled test feature matrix and 
#'                        returns a vector of predictions (real numbers for regression, probabilities for binary classification).
#' mean.validation.loss   
#' mean.train.loss.vec    (for plotting train/validation loss curves)
#' selected.steps
#' @export
#' 
#' @examples
#'    library(CodingProject3)
#'
#'    data(ozone , package = "ElemStatLearn")
#'    X.mat<-as.matrix(ozone [,-1])
#'    y.vec<-as.numeric(ozone [, 1])
#'    max.iterations <- 100
#'    fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
#'    step.size <- 0.1
#'    n.hidden.units <- 2
#'    result <- NNetEarlyStoppingCV(X.mat, y.vec, fold.vec, max.iterations, step.size, n.hidden.units)
NNetEarlyStoppingCV <- function(
  X.mat,
  y.vec,
  fold.vec=sample(rep(1:4, l=nrow(X.mat))),
  max.iterations,
  step.size,
  n.hidden.units,
  n.folds=4)
{
  
  if(!is.matrix(X.mat))
  {
    stop("Feature matrix is not a matrix")
  }
  
  if(nrow(X.mat) <= 0 | ncol(X.mat) <= 0)  
  {
    stop("Feature matrix has unexpected dimensions")
  }
  
  if(length(y.vec) <= 0)  
  {
    stop("Output matrix has unexpected dimensions")
  }
  
  train.loss.mat <- matrix(,max.iterations, n.folds)
  validation.loss.mat <- matrix(,max.iterations, n.folds)
  # n.folds <- max(fold.vec)
  for(fold.i in 1:n.folds)
  {
    fold_data <- which(fold.vec %in% c(fold.i))
    
    X.train <- X.mat[-fold_data ,]
    X.valid <- X.mat[fold_data ,]
    
    Y.train <- y.vec[-fold_data]
    Y.valid <- y.vec[fold_data]
    
    # n_rows_validation_set <- nrow(validation_set)
    # n_rows_train_set <- nrow(train_set)
    
    for(prediction.set.name in c("train", "validation")){
      if(identical(prediction.set.name, "train")){
        W <- NNetIterations(X.train, Y.train, max.iterations, step.size, n.hidden.units, fold.vec)
        pred.mat <- W$pred.mat
        train.loss.mat[,fold.i] = colMeans((pred.mat - Y.train)^2)
      }
      else{
        W <- NNetIterations(X.valid, Y.valid, max.iterations, step.size, n.hidden.units, fold.vec)
        pred.mat <- W$pred.mat
        validation.loss.mat[,fold.i] = colMeans((pred.mat - Y.valid)^2)
      }
    }
  }
  mean.validation.loss.vec <- rowMeans(validation.loss.mat)
  mean.train.loss.vec <- rowMeans(train.loss.mat)
  selected.steps = which.min(mean.validation.loss.vec)
  best_model <- NNetIterations(X.train,Y.train, max.iterations, step.size, n.hidden.units, fold.vec)
  weight_vec <- best_model$pred.mat[,selected.steps]
  
  list(
    mean.validation.loss = mean.validation.loss.vec,
    mean.train.loss.vec =  mean.train.loss.vec,
    selected.steps = selected.steps,
    pred.mat=best_model$pred.mat,
    V.mat= best_model$V.mat,
    w.vec=weight_vec,
    predict=function(testX.mat) {
      str(cbind(1, testX.mat))
      A.mat <- testX.mat %*% best_model$V.mat
      Z.mat <- sigmoid(A.mat)
      pred.vec <- Z.mat %*% weight_vec
    return(pred.vec)
  })
}
