coefficients.ca <- function(fm){
  # Created: July 28, 2012
  # Revised: July 28, 2012
  # Name: Claus E. Andersen
  # Extract fitted paarmeters and uncertainties from a fitted lm-model.
  data.frame(coeff=summary(fm)$coefficients[,1],u.coeff=summary(fm)$coefficients[,2])
}

first.element <- function(x,na.ignore=T){
  # Library: clan
  # Created: March 22, 2002
  # Revised: March 22, 2002
  # Name   : Claus E. Andersen
  # Task:    Find the first element in a single vector
  #          As default NA's are ignored, so the call
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
  res
}

last.element <- function(x,na.ignore=T){
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
  res
}
