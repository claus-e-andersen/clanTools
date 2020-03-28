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


#' @title Compute dayno from a written date like March 22, 2018
#' @description 
#' Convert date strings of the type March 22, 2018 to day numbers
#' since 01.01.2014 or some other zero date (reference). 
#' Sample call: 
#' 
#'  conv.ref.date("March 22, 2018, 12:00")
#'   
#'  conv.ref.date(c("March 22, 2018", "March 21, 2018", "March 22, 2019"),c("12:00"),
#'   zero.time="00:00",zero.date="22.03.2018") 
#'  
#'  with this output:
#'  
#'         dstr.org  d m    y       dstr dayno
#'  
#'         March 22, 2018 22 3 2018 22.03.2018   0.5
#'         
#'         March 21, 2018 21 3 2018 21.03.2018  -0.5
#'         
#'         March 22, 2019 22 3 2019 22.03.2019 365.5 
#' 
#' @usage  #' get.date.range(x=c("01-01-2020","31-01-2020"))
#' @name conv.ref.date
#' @author Claus E. Andersen
#' @return dateframe with detailed info (see example)
#' @param ref.date.vec vector with dates in the format "March 22, 2018".
#' @param ref.time.vec vector (or just a single value) with times in the format "12:00".
#' @param zero.date in the format "01.01.2014".
#' @param zero.time in the format "12:00".
#' @export conv.ref.date
conv.ref.date <- function(ref.date.vec = "March 22, 2018", ref.time.vec = "12:00", zero.date="01.01.2014", zero.time="12:00"){
  # Created: May 14, 2018
  # Revised: May 15, 2018
  # Revised: March 28, 2020
  # Name: Claus E. Andersen
  # Convert date strings of the type March 22, 2018, 12:00 to day numbers
  # since 01.01.2014. 
  # Sample call: conv.ref.date("March 22, 2018, 12:00") 
  #   conv.ref.date(c("March 22, 2018", "March 21, 2018", "March 22, 2019"),c("12:00"),
  #   zero.time="00:00",zero.date="22.03.2018") 
  #         dstr.org  d m    y       dstr dayno
  # 1 March 22, 2018 22 3 2018 22.03.2018   0.5
  # 2 March 21, 2018 21 3 2018 21.03.2018  -0.5
  # 3 March 22, 2019 22 3 2019 22.03.2019 365.5
  #
  print(ref.date.vec)
  ref.date.vec.org <- ref.date.vec
  ref.date.vec <- toupper(ref.date.vec)
  English.months.vec <- toupper(English.months(1:12)) 
  yy <- regexpr("[A-Z]+",ref.date.vec)
  xx <- substring(ref.date.vec,1,attr(yy,"match.length"))
  mm <- match(substring(xx,1,3),substring(English.months.vec,1,3))
  dd <- extract.first.number(ref.date.vec)
  yy <- extract.first.number(dd$rest.string)
  df <- data.frame(dstr.org=ref.date.vec.org,d=dd$number,m=mm,y=yy$number)
  df$dstr=paste(leading.zeros(df$d,2),leading.zeros(df$m,2),leading.zeros(df$y,4),sep=".")
  df$dayno <- dayno.calc(df$dstr,ref.time.vec,ref.date=zero.date,ref.time=zero.time)
  df}# conv.ref.date

