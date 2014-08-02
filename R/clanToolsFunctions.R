#' @title Extract coefficients from fitted linear models (lm)
#' @description This functions helps get access to the uncertainties associated with the fit.
#' @usage 
#' fm <- lm(x~y, data=df)
#' cc <- coefficents.ca(fm)
#' print( cc$coeff )
#' print( cc$u.coeff )
#' @name coefficients.ca
#' @author Claus E. Andersen
#' @return A dataframe with coefficients and uncertainties. coeff is the
#' fitted values and u.coeff is the accociated uncertainties.  
#' @param fm is a fitted model from lm
#' @export coefficients.ca
coefficients.ca <- function(fm){
  # Created: July 28, 2012
  # Revised: July 28, 2012
  # Name: Claus E. Andersen
  # Extract fitted paarmeters and uncertainties from a fitted lm-model.
  return(data.frame(coeff=summary(fm)$coefficients[,1],u.coeff=summary(fm)$coefficients[,2]))
}

#' @title Find the first element in a vector
#' @description This functions also handles NULL vectors and NA-values.
#' @usage 
#' xx <- c(NA,1:100,NA)
#' first.element(xx)
#' @name first.element
#' @author Claus E. Andersen
#' @return The first element
#' @param x is the vector
#' @param na.ignore controls if NA-values should be ignored
#' @export
first.element <- function(x,na.ignore=TRUE){
  # Library: clan
  # just a note
  # Created: March 22, 2002
  # Revised: March 22, 2002
  # Name   : Claus E. Andersen
  # Task:    Find the last element in a single vector.
  #          As default, NA's are ignored, so the call
  #          first.element(c(NA, 3:100, NA)) will return 3.
  if(is.null(x) |  sum(!is.na(x))==0| length(x)==0){
    res <- NA
  } else
  {N <- length(x)
   index <- 1:N
   ok <- rep(T,N)
   if(na.ignore) ok <- !is.na(x)
   index <- index[ok]
   res <- x[min(N,index)]}
  return(res)
}

#' @title Find the last element in a vector
#' @description This functions also handles NULL vectors and NA-values.
#' @usage 
#' xx <- c(NA,1:100,NA)
#' last.element(xx)
#' @name last.element
#' @author Claus E. Andersen
#' @return The last element
#' @param x is the vector
#' @param na.ignore controls if NA-values should be ignored
#' @export
last.element <- function(x,na.ignore=TRUE){
  # Library: clan
  # Created: March 22, 2002
  # Revised: March 22, 2002
  # Name   : Claus E. Andersen
  # Task:    Find the last element in a single vector
  #          As default NA's are ignored, so the call
  #          last.element(c(NA, 1:100, NA)) will return the 100.
  if(is.null(x) |  sum(!is.na(x))==0| length(x)==0){
    res <- NA
  } else
  {N <- length(x)
   index <- 1:N
   ok <- rep(T,N)
   if(na.ignore) ok <- !is.na(x)
   index <- index[ok]
   res <- x[max(1,index)]}
  return(res)
}
