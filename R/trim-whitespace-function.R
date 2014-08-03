#' @title Remove write space at the beginning and end of a string 
#' @description  Trim a string for white space.
#' @usage  x <- trim.whitespace("    uyiyy uiyiyiuy y        ")
#' @name trim.whitespace 
#' @author Claus E. Andersen
#' @return trimmed string
#' @export trim.whitespace
trim.whitespace <- function (x) gsub("^\\s+|\\s+$", "", x)