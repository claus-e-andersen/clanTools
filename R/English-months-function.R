#' @title  Print names of months in English
#' @description 
#' Convert numbers from 1 to 12 into "January", "February" etc.
#' This version handles vectorized input, and NA's and NULL.
#' @usage  English.months(1)
#' @name English.months
#' @author Claus E. Andersen
#' @return string vector
#' @param i vector with numbers between 1 and 12.
#' @export English.months
English.months <- function(i=1){
  mvec<-c("January","February","March","April","May","June","July","August","September","October","November","December")
  if(is.null(i)) {i <- NA}
  res <- rep("Not.a.valid.month",length(i)) 
  
  ok <- !is.na(i) & (i>=1) & (i<=12)
  if(sum(ok)>0){
    j <- 1:length(i)
    res[j[ok]] <- mvec[i[ok]] 
  }
  
  res
}# English.months


