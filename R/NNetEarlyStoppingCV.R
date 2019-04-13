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
#'    X.mat<-as.matrix(ozone [1:50,-1])
#'    y.vec<-as.matrix(ozone [1:50, 1])
#'    max.iterations <- 100
#'    fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
#'    step.size <- .5
#'    n.hidden.units <- 2
#'    result <- NNetEarlyStoppingCV(X.mat, y.vec, fold.vec, max.iterations, n.hidden.units,5)
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
    fold_data <- which(fold.vec == fold.i)
    
    X.train <- X.mat[-fold_data ,]
    X.valid <- X.mat[fold_data ,]
    
    Y.train <- y.vec[-fold_data]
    Y.valid <- y.vec[fold_data]
    
    # n_rows_validation_set <- nrow(validation_set)
    # n_rows_train_set <- nrow(train_set)
    
    W <- NNetIterations(X.train, Y.train, max.iterations, step.size, n.hidden.units, fold.vec)
    
    for(prediction.set.name in c("train", "validation")){
      if(identical(prediction.set.name, "train")){
        to.be.predicted <- X.train
      }
      else{
        to.be.predicted <- X.valid
      }
      
      pred.mat <- W$pred.mat
      print(pred.mat)

      if(identical(prediction.set.name, "train")){
        train.loss.mat[,fold.i] = colMeans((pred.mat - Y.train)^2)
      }
      else{
        pred.valid.mat <- W$predict(X.valid)
        validation.loss.mat[,fold.i] = colMeans((pred.valid.mat - Y.valid)^2)
      }
    }
  }
  mean.validation.loss.vec <- rowMeans(validation.loss.mat)
  mean.train.loss.vec <- rowMeans(train.loss.mat)
  selected.steps = which(mean.validation.loss.vec == min(mean.validation.loss.vec), arr.ind = TRUE)
  best_model <- NNetIterations(X.train,Y.train, max.iterations, step.size, n.hidden.units, fold.vec)
  weight_vec <- best_model$pred.mat[,selected.steps]
  
  list(
    mean.validation.loss = mean.validation.loss.vec,
    mean.train.loss.vec =  mean.train.loss.vec,
    selected.steps = selected.steps)
}