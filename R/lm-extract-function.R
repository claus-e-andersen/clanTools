#' @title Extract data from fitted lm-model 
#' @description  Extract data from fitted lm-model
#' @usage  
#'   df <- data.frame(x=1:10, y=rnorm(10))
#'   fm <- lm(y~x,data=df) 
#'   cc <- lm.extract(fm)
#' @name lm.extract 
#' @author Claus E. Andersen
#' @return list of data
#' @param trace controls output during execution
#' @export lm.extract
lm.extract <- function(fm,trace=FALSE){
  # Extract information from lm-model
  # Created: June 7, 2014
  # Revised: June 7, 2014
  # Name   : Claus E. Andersen
  ss <- summary(fm)
  tt <- ss$coefficients
  if(trace){
    print(ss)
    print(summary(ss))
  }
  list(call=ss$call,
       terms=ss$terms,
       residuals=ss$residuals,
       coefficients=ss$coefficients,
       aliased=ss$aliased,
       sigma=ss$sigma,
       df=ss$df,
       r.squared=ss$r.squared,
       adj.r.squared=ss$adj.r.squared,
       fstatistic=ss$fstatistic,
       cov.unscaled=ss$cov.unscaled,
       coeff =tt[,1], stderr = tt[,2], t.value = tt[,3], p.value =tt[,4])
}
