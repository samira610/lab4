#' @title linreg
#' @details Multiple regression model.
#' @description The code for a multiple regression model. 
#' Linreg have the two arguments formula and data.
#' The function return an object with of class linreg either as an S3 class or an RC class.
#' @param formula A formula.
#' @param data A data.
#' @return An object with of class linreg either as an S3 class or an RC class \code{a}.
#' @export

linreg <- function(formula,data){
  
  #matrix X(independent variables) created
  #y(dependent variable) picked out
  X <- model.matrix(formula,data)
  y <- data[all.vars(formula)]
  y <- unname(as.matrix(y))
  
  #RC is Regression Coefficient
  RC <- (t(X) %*% y)/(t(X)%*%X)
  #Fv is The fitted values
  Fv <- X %*% RC
  #Res is The residuals
  Res <- y - Fv
  #Dof is The degrees of freedom
  Dof <- nrow(X)-ncol(X)
  #ResV is The Residual variance
  Resv <- (t(Res)%*% Res)/Dof
  # Vrc is the variance of the regression coefficients
  Vrc <- Resv/(t(X)%*%X)
  #tvc the t-values for each coefficient
  tvc <- RC/sqrt(Vrc)
  
  lrplist <- list(RC,Fv,Res,Dof,Resv,Vrc,tvc)
  attr(lrplist, "class") <- linreg()
  
  return(lrplist)
}
