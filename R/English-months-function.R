#' @title  Print names of months in English
#' @description 
#' Convert numbers 1 to 12 into "January", "February" etc.
#' @usage  English.months(1)
#' @name English.months
#' @author Claus E. Andersen
#' @return string
#' @param i number between 1 and 12.
#' @export English.months
English.months <- function(i=1){
  res <- "Not.a.valid.month"
  if(is.null(i) || is.na(i)){} else {
    mvec<-c("January","February","March","April","May","June","July","August","September","October","November","December")
    i <- as.integer(i)
    if(i<1|i>12){} else {
      res <- mvec[i]}
  }
  res
}# English.months


