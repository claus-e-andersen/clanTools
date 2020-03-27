#' @title  Improved function for computation of row means
#' @description 
#' The R base function rowMeans does not work if there is only one column!
#' This function mitigates that problem. vec is a vector of column names in df such as, 
#' e.g. c("degC.W1","degC.W2").
#' @usage  rowMeans.ca(df,c("degC.W1","degC.W2"))
#' @name rowMeans.ca
#' @author Claus E. Andersen
#' @return string vector
#' @param i vector with numbers between 1 and 12.
#' @export rowMeans.ca
rowMeans.ca <- function(df,vec){
  # vec is a vector of column names in df such as, e.g. c("degC.W1","degC.W2")
  # Notice that rowMeans does not work if there is only one column!
  done <- FALSE
  if(!done && length(vec)==0){done <- TRUE; res <- rep(NA,nrow(df))}
  if(!done && is.na(vec))    {done <- TRUE; res <- rep(NA,nrow(df))}
  if(!done && length(vec)==1){done <- TRUE; res <- df[,vec]}
  if(!done && length(vec)>1) {done <- TRUE; res <- rowMeans(df[,vec], na.rm=TRUE)} 
  res
}# rowMeans.ca