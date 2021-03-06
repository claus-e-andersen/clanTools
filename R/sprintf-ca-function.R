#' @title  Print number to fit certain format. An alternative is used if this requires too many characters.
#' @description 
#' This is a variation of sprintf, that checks if the formated result is too long. 
#' This can happen, for example, with electrometer output, where normal results are 
#' 0.004 pC and then an overflow gives a charge of 6e47 C, which produces an output 
#' that ruins txtplot table output. 
#' @usage  sprintf.ca("\%.5f", c(1.23, 2.33, 1e99), max.chars=7)
#' @name sprintf.ca 
#' @author Claus E. Andersen
#' @return formated number.
#' @param format is the main format (e.g. "\%.5f") which is used if the output does not require too many characters.
#' @param number to print (e.g. 1.334 or 1.4e99).
#' @param format.alt is the alternative format (e.g. "\%.4e") which is used for output that require too many characters.
#' @param max.char is the maximum number of characters (e.g. 10) that the main format needs to fit (otherwise the alternative format will be used).
#' @export sprintf.ca 
sprintf.ca <- function(format,number,format.alt="%.4e",max.chars=10){
  # This is a variation of sprintf, that checks if the formated
  # results is too long. This can happen, for example, with electrometer
  # output, where normal results are 0.004 pC and then an overflow
  # gives a charge of 6e47 C, which produces an output that ruins
  # txtplot table output.
  # Created: January 12, 2019
  # Claus E. Andersen
  # Sample calls:
  # x <- 3.4557e-3
  # sprintf.ca("%.5f", round(x,0))
  # x <- 6.6e42
  # sprintf.ca("%.5f", round(x,0))
  # x <- 6.6e42
  # sprintf.ca("%.5f", round(x,0),max.chars=9999)
  y.main <- sprintf(format, number)
  y.alt  <- sprintf(format.alt, number)
  res <- y.main
  ok <-nchar(y.main)>max.chars
  if(sum(ok)>0){res[ok]<-y.alt[ok]}
  res
}# sprintf.ca 