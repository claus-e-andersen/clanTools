#' @title  Compute the resolution uncertainty (last digit influence) - relative version (\%)
#' @description 
#' This function compute the relative standard uncertainty (k=1) for instruments
#' with a given number of digits (say, 4) that is always used to their fullest
#' extend (i.e. a reading of 1 would actually be 1.000 and a 
#' reading of 1.1E-9 would actually be 1.100 x 1E-9). 
#' 
#' As the function gives the relative uncertainty, it cannot handle
#' a reading which is zero (the function returns NA)..
#' 
#' We compute the standard uncertainty from resolution limitations
#' as the last digit divided by the squareroot of 12 (i.e. u = 0.29 * step). 
#' This is consistent with GUM F.2.2.1 and the assumption of a uniform
#' probability distribution. So, with four digits, a reading starting
#' with 1 (like 1.002 or -1.192e-88) will lead to an estimated
#' resolution uncertainty of about 0.029\%. Note that we assume that
#' the instrument always provides the given number of digits.
#' 
#' See also: \link{resolution.uncertainty.abs} 
#' 
#' #' Example of a 0.029\% resolution uncertainty:
#' 
#' resolution.uncertainty.pct(1004,digits=4,last.digit.step=1)
#'
#' resolution.uncertainty.pct(1.1e-20,digits=4,last.digit.step=1)
#'
#' resolution.uncertainty.pct(1.1e20,digits=4,last.digit.step=1)
#'
#' Note that the function is vectorized, such that calls like the following is possible:
#' 
#' resolution.uncertainty.pct(c(1.2, 1.3, 0, 0, NA, NA, 2), digits=4,last.digit.step=1)
#' 
#' @usage  resolution.uncertainty.pct(1004,digits=4,last.digit.step=1)
#' @name resolution.uncertainty.pct
#' @author Claus E. Andersen
#' @return numeric value (pct uncertainty). May be vectorixed.
#' @param x raw reading (can be vectorized).
#' @param digits number of digits on the instrument display.
#' @param last.digit.step resolution of last digit (e.g. 1 or 5).
#' @param min.value do not report an uncertainty below this value (e.g. 0.001 \%).
#' @param max.value do not report an uncertainty above this value (e.g. 50 \%).
#' @export resolution.uncertainty.pct
resolution.uncertainty.pct <- function(x,digits=4,last.digit.step=1,min.value=0.001,max.value=50){
  # Created: May 6, 2019
  # Revised: June 13, 2019 (vectorized and better NA-handling)
  # Revised: FollowingGUM F.2.2.1, we assume a uniform (rectangular) prob. distribution
  # and u = 0.29 x step (rather than 0.5 x step).
  # Name   : Claus E. Andersen
  # We compute the standard uncertainty from resolution limitations
  # as the last digit x 0.29... So, with four digits, a reading starting
  # with 1 (like 1.002 or -1.192e-88) will lead to an estimated
  # resolution uncertainty of about 0.029%. Note that we assume that
  # the instrument always provides the given number of digits.
  # 0.029% : resolution.uncertainty.pct(1004,digits=4,last.digit.step=1)
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
  res <- res * (1/12)^0.5
  res <- pmin(res,max.value,na.rm=!TRUE)
  res <- pmax(res,min.value,na.rm=!TRUE)
  res
} # resolution.uncertainty.pct

#' @title  Compute the resolution uncertainty (last digit influence) - absolute version
#' @description 
#' This function compute the standard uncertainty (k=1) for instruments
#' with a given number of digits (say, 4) that is always used to their fullest
#' extend (i.e. a reading of 1 would actually be 1.000 and a 
#' reading of 1.1E-9 would actually be 1.100 x 1E-9). 
#' 
#' The function handles zero-readings.
#'  
#' We compute the standard uncertainty from resolution limitations
#' as the last digit divided by the squareroot of 12 (i.e. u = 0.29 * step). 
#' This is consistent with GUM F.2.2.1 and the assumption of a uniform
#' probability distribution. So, with four digits, a reading starting
#' with 1 (like 1.002) will lead to an estimated
#' standard uncertainty of about 2.9e-4. Note that we assume that
#' the instrument always provides the given number of digits.
#' 
#' See also: \link{resolution.uncertainty.pct} 
#' 
#' #' Example:
#' 
#' resolution.uncertainty.abs(0,digits=4,last.digit.step=1)
#'
#' resolution.uncertainty.abs(1.1e-20,digits=4,last.digit.step=1)
#'
#' resolution.uncertainty.abs(1.1e20,digits=4,last.digit.step=1)
#' 
#' Note that the function is vectorized, such that calls like the following is possible:
#' 
#' resolution.uncertainty.abs(c(1.2, 1.3, 0, 0, NA, NA, 2), digits=4,last.digit.step=1)
#' 
#' @usage  resolution.uncertainty.abs(1004,digits=4,last.digit.step=1)
#' @name resolution.uncertainty.abs
#' @author Claus E. Andersen
#' @return numeric value (absolute uncertainty). May be vectorixed.
#' @param x raw reading (can be vectorized).
#' @param digits number of digits on the instrument display.
#' @param last.digit.step resolution of last digit (e.g. 1 or 5).
#' @param min.value do not report an uncertainty below this value (e.g. 0.001 in abs. units).
#' @param max.value do not report an uncertainty above this value (e.g. 50 in abs. units).
#' @resolution.uncertainty.abs
resolution.uncertainty.abs <- function(x,digits=4,last.digit.step=1,min.value=1e-99,max.value=1e99){
  # Created: April 25, 2020
  ok.NA <- is.na(x)
  ok.nonzero <- !is.na(x) & !x==0
  ok.zero <- !is.na(x) &  x==0
  
  res <- rep(NA,length(x))
  
  ok <- ok.nonzero 
  if(sum(ok,na.rm=TRUE)>0){  
    res[ok] <- x[ok]/100*resolution.uncertainty.pct(x=x[ok],
                                                    digits=digits,last.digit.step=last.digit.step,
                                                    min.value=min(min.value/x[ok]*100,na.rm=TRUE), max.value=max(max.value/x[ok],na.rm=TRUE)*100)
  } # nonzero case
  
  ok <- ok.zero 
  if(sum(ok,na.rm=TRUE)>0){  
    # We treat the zero-results like unity, and call the pct-version!
    x.zero <- 1
    res[ok] <- x.zero/100*resolution.uncertainty.pct(x=x.zero,digits=digits,
                                                     last.digit.step=last.digit.step,min.value=min.value/x.zero*100,max.value=max.value/x.zero*100)
  } # zero case
  
  ok <- ok.NA
  if(sum(ok,na.rm=TRUE)>0){  
    res[ok] <- NA
  } # NA case
  
  return(res)
  } # resolution.uncertainty.abs
