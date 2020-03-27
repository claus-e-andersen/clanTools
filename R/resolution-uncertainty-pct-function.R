#' @title  Compute the resolution uncertainty (last digit influence)
#' @description 
#' We compute the standard uncertainty from resolution limitations
#' as half the last digit. So, with four digits, a reading starting
#' with 1 (like 1.002 or -1.192e-88) will lead to an estimated
#' resolution uncertainty of about 0.05\%. Note that we assume that
#' the instrument always provides the given number of digits.
#' 0.05\% : resolution.uncertainty.pct(1004,digits=4,last.digit.step=1)
#' @usage  resolution.uncertainty.pct(1004,digits=4,last.digit.step=1)
#' @name resolution.uncertainty.pct
#' @author Claus E. Andersen
#' @return numeric (pct uncertainty)
#' @param x raw reading.
#' @param digits number of digits on the instrument display.
#' @param last.digit.step resolution of last digit (e.g. 1 or 5).
#' @param min.value (e.g. 0.001).
#' @param max.value (e.g. 50).
#' @export resolution.uncertainty.pct
resolution.uncertainty.pct <- function(x,digits=4,last.digit.step=1,min.value=0.001,max.value=50){
  # Created: May 6, 2019
  # Revised: June 13, 2019 (vectorized and better NA-handling)
  # Name   : Claus E. Andersen
  # We compute the standard uncertainty from resolution limitations
  # as half the last digit. So, with four digits, a reading starting
  # with 1 (like 1.002 or -1.192e-88) will lead to an estimated
  # resolution uncertainty of about 0.05%. Note that we assume that
  # the instrument always provides the given number of digits.
  # 0.05% : resolution.uncertainty.pct(1004,digits=4,last.digit.step=1)
  x <- abs(x)
  x <- x * 1e9
  
  ok <- !is.na(x) && x > 0 
  if(sum(ok)>0){x[ok] <- x[ok] * 10^(-trunc(log10(x))) * 1e9} 
  
  z <- rep("",length(x))
  
  ok <- !is.na(x)
  if(sum(ok)>0){ 
    z[ok] <- sprintf("%.6f",round(x[ok],6))
  }
  
  ok <- is.na(x)
  if(sum(ok)>0){ 
    z[ok] <- rep(NA,sum(ok))
  }
  
  res <- last.digit.step/as.numeric(substring(z,1,digits))*100
  res <- res/2
  res <- pmin(res,max.value,na.rm=!TRUE)
  res <- pmax(res,min.value,na.rm=!TRUE)
  res
} # resolution.uncertainty.pct
