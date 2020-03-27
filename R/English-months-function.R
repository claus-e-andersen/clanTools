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



#' @title  Make string with range of dates (eg. for calibration certificates)
#' @description 
#' Compute the range of dates and present results using English months, like
#' "January 01, 2020 -- January 31, 2020". If there is only one date, then
#' the output will be something like "January 01, 2020 (one day only)".
#'  
#' Example:
#' 
#' get.date.range(x="31-01-2020")
#' 
#' get.date.range(x=c("01-01-2020","01-01-2020"))
#' 
#' get.date.range(x=c("01-01-2020","31-01-2020"))
#' 
#' @usage  #' get.date.range(x=c("01-01-2020","31-01-2020"))
#' @name get.date.range
#' @author Claus E. Andersen
#' @return string with human readable text.
#' @param x vector with dates in the format "31-01-2020".
#' @export get.date.range
get.date.range <- function(x){
  xxx <- paste(unique(as.character(x)),sep=" ", collapse=" ")
  yyy <-  unlist(strsplit(xxx,split=" "))
  #zzz <- paste(unique(as.character(yyy)),collapse=" ", sep=" ")
  #x <- zzz
  x <- yyy
  x <- unique(x)
  x <- sort(x)
  x1 <- first.element(x)
  x2 <- last.element(x)
  xx.value1 <- paste(English.months(as.numeric(substring(x1,4,5)))," ",substring(x1,1,2),", ",substring(x1,7,10),sep="")
  xx.value2 <- paste(English.months(as.numeric(substring(x2,4,5)))," ",substring(x2,1,2),", ",substring(x2,7,10),sep="")
  xx.value0 <- paste(xx.value1," -- ",xx.value2,"",sep="")
  if(x1==x2){
    xx.value0 <- paste(xx.value1," (one day only)",sep="")
  }
  xx.value0 
} # get.date.range

