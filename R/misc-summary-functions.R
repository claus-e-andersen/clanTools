#' @title Find first element and return as character.
#' @description 
#' Find first element and return as character.
#' See also \link{first.element.character}, \link{last.element.character}, \link{mean.for.numerics}, 
#' Use case: see CBASE-cobalt-cal-single-100.R
#' #' \link{min.for.numerics}, \link{max.for.numerics}, and \link{number.of.nas}.
#' @usage  first.element.character(1:10=
#' @name first.element.character
#' @author Claus E. Andersen
#' @return string (single value)
#' @param x input vector
#' @export first.element.character
first.element.character <- function(x){as.character(x[1])}

#' @title Find last element and return as character.
#' @description 
#' Find last element and return as character.
#' See also \link{first.element.character}, \link{mean.for.numerics}, 
#' \link{min.for.numerics}, \link{max.for.numerics}, and \link{number.of.nas}.
#' @usage  last.element.character(1:10)
#' @name first.element.character
#' @author Claus E. Andersen
#' @return string (single value)
#' @param x input vector
#' @export last.element.character
last.element.character <- function(x){as.character(rev(rev(x)[1]))}

#' @title Find mean element, if numeric
#' @description 
#' Find mean element, if numeric.
#' See also \link{first.element.character}, 
#' \link{min.for.numerics}, \link{max.for.numerics}, and \link{number.of.nas}.
#' Use case: see CBASE-cobalt-cal-single-100.R
#' @usage  mean.for.numerics(1:10)
#' @name mean.for.numerics
#' @author Claus E. Andersen
#' @return numeric with given number of significant digits (single value)
#' @param x input vector
#' @param digits number of digits in result
#' @export mean.for.numerics
mean.for.numerics <- function(x,digits=5){ifelse(is.numeric(x),signif(mean(x,na.rm=TRUE),digits),'no number')}

#' @title Find minimum element, if numeric
#' @description 
#' Find minimum element, if numeric.
#' See also \link{first.element.character}, \link{last.element.character}, \link{mean.for.numerics}, 
#' \link{min.for.numerics}, \link{max.for.numerics}, and \link{number.of.nas}.
#' Use case: see CBASE-cobalt-cal-single-100.R
#' @usage  min.for.numerics(1:10)
#' @name min.for.numerics
#' @author Claus E. Andersen
#' @return numeric with given number of significant digits (single value)
#' @param x input vector
#' @param digits number of digits in result
#' @export min.for.numerics
min.for.numerics <- function(x,digits=5){ifelse(is.numeric(x),signif(min(x,na.rm=TRUE),digits),'no number')}

#' @title Find maximum element, if numeric
#' @description 
#' Find maximum element, if numeric.
#' See also \link{first.element.character}, \link{last.element.character}, \link{mean.for.numerics}, 
#' \link{min.for.numerics}, and \link{number.of.nas}.
#' Use case: see CBASE-cobalt-cal-single-100.R
#' @usage  max.for.numerics(1:10)
#' @name max.for.numerics
#' @author Claus E. Andersen
#' @return numeric with given number of significant digits (single value)
#' @param x input vector
#' @param digits number of digits in result
#' @export max.for.numerics
max.for.numerics <- function(x,digits=5){ifelse(is.numeric(x),signif(max(x,na.rm=TRUE),digits),'no number')}

#' @title Find number of NAs in vector
#' @description 
#' Find number of NA-values in vector.
#' See also \link{first.element.character}, \link{last.element.character}, \link{mean.for.numerics}, 
#' \link{min.for.numerics}, and \link{max.for.numerics}.
#' Use case: see CBASE-cobalt-cal-single-100.R
#' @usage  number.of.nas(1:10)
#' @name number.of.nas
#' @author Claus E. Andersen
#' @return number (single value)
#' @param x input vector
#' @export number.of.nas
number.of.nas <- function(x){sum(is.na(x))}