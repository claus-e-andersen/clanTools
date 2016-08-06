#' @title Extract coefficients from a fitted linear model (lm)
#' @description This function helps get easy access to the uncertainties associated with the fit.
#' @usage 
#' fm <- lm(x~y, data=df)
#' cc <- coefficents.ca(fm)
#' print( cc$coeff )
#' print( cc$u.coeff )
#' @name coefficients.ca
#' @author Claus E. Andersen
#' @return A dataframe with coefficients and uncertainties. coeff is the
#' fitted values and u.coeff is the accociated uncertainties.  
#' @param fm is a fitted model from lm
#' @export coefficients.ca
coefficients.ca <- function(fm){
  # Created: July 28, 2012
  # Revised: July 28, 2012
  # Name: Claus E. Andersen
  # Extract fitted paarmeters and uncertainties from a fitted lm-model.
  return(data.frame(coeff=summary(fm)$coefficients[,1],u.coeff=summary(fm)$coefficients[,2]))
}

#' @title Find the first element in a vector
#' @description This functions also handles NULL vectors and NA-values.
#' @usage 
#' xx <- c(NA,1:100,NA)
#' first.element(xx)
#' @name first.element
#' @author Claus E. Andersen
#' @return The first element
#' @param x is the vector
#' @param na.ignore controls if NA-values should be ignored
#' @param N (default = 1) is the number of elements that should be returned
#' @export
first.element <- function(x,na.ignore=TRUE,N=1){
  # Library: clan
  # just a note
  # Created: March 22, 2002
  # Revised: March 22, 2002
  # Revised: August 5, 2016
  # Name   : Claus E. Andersen
  # Task:    Find the last element in a single vector.
  #          As default, NA's are ignored, so the call
  #          first.element(c(NA, 3:100, NA)) will return 3.
  if(is.null(x) |  sum(!is.na(x))==0| length(x)==0){
    res <- NA
  } else {
  if(na.ignore){x <- x[!is.na(x)]}
  N <- min(length(x),N)
  res <- x[1:N]
  }
  return(res)
}

#' @title Find the last element in a vector
#' @description This functions also handles NULL vectors and NA-values.
#' @usage 
#' xx <- c(NA,1:100,NA)
#' last.element(xx)
#' @name last.element
#' @author Claus E. Andersen
#' @return The last element
#' @param x is the vector
#' @param na.ignore controls if NA-values should be ignored
#' @param N (default = 1) is the number of elements that should be returned
#' @export
last.element <- function(x,na.ignore=TRUE,N=1){
  # Library: clan
  # Created: March 22, 2002
  # Revised: March 22, 2002
  # Revised: August 5, 2016
  # Name   : Claus E. Andersen
  # Task:    Find the last element in a single vector
  #          As default NA's are ignored, so the call
  #          last.element(c(NA, 1:100, NA)) will return the 100.
  # The argument N can be used to extract more than one elemet.
  # So, last.element(1:30,N=6) 
  # returns:
  # 30 29 28 27 26 25
  if(is.null(x) |  sum(!is.na(x))==0| length(x)==0){
    res <- NA
  } else
  {
    if(na.ignore){x <- x[!is.na(x)]}
    N <- min(length(x),N)
    res <- rev(x)[1:N]}
  return(res)
}

#' @title  Find the most common element(s) in a single vector.
#' @description 
#' Find the most common element(s) in a single vector
# Many elements may occur the same number of times (equal to the max number of times).
# For example, if a vector of unique elements are given to the function, then all
# elements occur exactly once. To limit the number of elements that is returned, a
# value for max.number.of.elements may be given.
#' This functions also handles NULL vectors and NA-values.
#' @usage 
#' xx <- c(NA,1:100,NA)
#' most.common.element(xx,1)
#' @name most.common.element
#' @author Claus E. Andersen
#' @return The most common element in the vector
#' @param x is the vector
#' @param max.number.of.elements is the limit elements in the return vector
#' @export
most.common.element <- function(x,max.number.of.elements=NA){
  # Library: clan
  # Created: April 13, 2002
  # Revised: April 13, 2002
  # Name   : Claus E. Andersen
  # Task:    Find the most common element(s) in a single vector
  # Many elements may occur the same number of times (equal to the max. number of times).
  # For example, if a vector of unique elements are given to the function, then all
  # elements occur exactly once. To limit the number of elements that is returned, a
  # value for max.number.of.elements may be given.
  xxx <- table(x)
  ii <- xxx==max(xxx)
  res <- names(xxx)[ii]
  if (!is.na(max.number.of.elements)) res <- res[1:min(length(res),max.number.of.elements)]
  res
}

#' @title  Replace a character in a string with a new character
#' @description 
#' Find the most common element(s) in a single vector
#' Many elements may occur the same number of times (equal to the max number of times).
#' For example, if a vector of unique elements are given to the function, then all
#' elements occur exactly once. To limit the number of elements that is returned, a
#' value for max.number.of.elements may be given.
#' This functions also handles NULL vectors and NA-values.
#' @usage  replacechar("AbE","b","B")
#' @name replacechar
#' @author Claus E. Andersen
#' @return The string with the replaced character
#' @param x is the string
#' @param char is the character to be replaced
#' @param newchar is the new character
#' @export
replacechar <- function(x, char = " ", newchar = "")
{
  # Modified from S-plus mailing list response by Rod Tjoelker (1996)
  # Library: clan
  # Claus E. Andersen
  # August 1, 1999
  # SEE THE NEWER IMPLEMENTATION: substitute.char
  # Sample call:
  #   replacechar("AbE","b","B")
  under <- 1:length(x)
  # The original seems not to work under Windows:  under<-grep(char, x)
  for(i in under) {
    nc <- nchar(x[i])
    ch <- substring(x[i], 1:nc, 1:nc)
    ch <- ifelse(ch == char, newchar, ch)
    x[i] <- paste(ch, collapse = "")
  }
  return(x)
}

#' @title  Replace a character in a string with a new character
#' @description Replace a single character. 
#' @usage  substitute.char("abxdefg","x","c")
#' @name substitute.char
#' @author Claus E. Andersen
#' @return The string with the replaced character
#' @param st is the string
#' @param find.char is the character to be replaced
#' @param new.char is the new character
#' @export
substitute.char <- function(st, find.char = ",", new.char = ".")
{
  # Created: April 15, 2002
  # Revised: April 15, 2002
  # Revised: September 17, 2011
  # Name   : Claus E. Andersen
  # Note   : Vectorized
  # Sample call: substitute.char("abxdefg","x","c")
  #	all.chars <- strsplit(st, "")
  #  # Was in S-plus: string.to.chars(st)
  #	N <- length(all.chars)
  #	res <- rep(NA, N)
  #	for(ii in 1:N) {
  #		xx <- unlist(all.chars[ii])
  #		kk <- xx == find.char
  #		xx[kk] <- new.char
  #		xx <- paste(xx, sep = ",", collapse = "")
  #		res[ii] <- xx
  #	}
  #	res
  gsub(find.char,new.char,st)
}

#' @title  Add zeros in string representing a number
#' @description Add zeros when numbers occur in filenames and such
#' 009 comes before 111 (cf. 9 vs. 111) 
#' @usage  leading.zeros(c(9,111),3)
#' @name leading.zeros
#' @author Claus E. Andersen
#' @return The vecor of characters wher zeors have been added as necessary
#' @param x is a vector of numeric values
#' @param digits is the number of digits in the final results.
#' @export
leading.zeros <- function(x, digits = 5)
{
  # Convert 99 into "00099" etc.
  # From the s-mailing list.
  # [S] SUMMARY:  Formatting with Leading Zeroes
  # June 21, 2001, Mike.Prager@noaa.gov
  # The following solution is based on the one suggested by Bill Venables.
  # Library: clan
  # Created July 9, 2001, Revised: July 9, 2001, Claus E. Andersen
  # Test: leading.zeros(c(9,99,999,999)) 	
  mx <- paste("1", paste(rep("0", digits), sep = "", collapse = ""),
              sep = "")
  return(substring(as.numeric(mx) + x, 2))
}

#' @title  Add zeros in string representing a number 
#' @description Add zeros when numbers occur in filenames and such
#' 009 comes before 111 (cf. 9 vs. 111). The number of leading zeros
#' is camculated automatically.  
#' @usage  leading.zeros.to.fit(c(9,111))
#' @name leading.zeros.to.fit
#' @author Claus E. Andersen
#' @return The vecor of characters wher zeors have been added as necessary
#' @param x is a vector of numeric values
#' @export
leading.zeros.to.fit <- function(x)
{
  # Library : clan
  # Purpose : to add zeros to a vector of numbers such that the all have the character representation (e.g. "000", "001", ..."999")
  # Created : January 3, 2004
  # Name    : Claus E. Andersen
  # sample.call: leading.zeros.to.fit(1:999)
  digits <- log10(max(1, x, na.rm = T))
  return(leading.zeros(x, digits + 1))
}

#' @title  Extract the first number from a string 
#' @description Extract the first numbers from each individual string element in a vector.  
#' @usage  extract.first.number(c("aa+45", "hello9abe999"))
#' @name extract.first.number
#' @author Claus E. Andersen
#' @return A list with the numbers ($number) and the remaining strings after the number ($rest.string).
#' @param x is a vector of string values
#' @export
extract.first.number <- function(x)
{
  # Find the first number in a string (vectorized)
  # Created: July 13, 2006
  # Claus E. Andersen
  # Sample call:
  #   extract.first.number(c("aa+45", "ddddd ffghf gfg", "x   -0.001", ".", "+", "+33", "gfgfg -.33","+-4","-+5","-+0"))$number
  #   [1]  45.000      NA  -0.001      NA      NA  33.000  -0.330  -4.000   5.000   0.000
  w <- regexpr("[+-]?[0-9]*[\\.]*[0-9]+", x)
  subst <- substring(x, w, w + attr(w, "match.length") - 1)
  number <- as.numeric(subst)
  rest <- substring(x, w + attr(w, "match.length"), 999)
  list(number = number, rest.string = rest)
}

#' @title  Extract the N'th number from a string 
#' @description Extract the N'th numbers from each individual string element in a vector.  
#' @usage  extract.given.number(c("aa1","bbb tet 11 22 33"),3)
#' @name extract.given.number
#' @author Claus E. Andersen
#' @return A vector with the numbers.
#' @param x is a vector of string values
#' @param N = 1 means the first number, N = 2 means the second number etc.
#' @export
extract.given.number <- function(x, N = 1)
{
  # Find the N'th number in a string (vectorized)
  # Created: July 13, 2006
  # Claus E. Andersen
  # Sample call:
  #    extract.given.number(c("aa1","bbb tet 11 22 33"),3)
  #    [1] NA 33
  if(N >= 2) for(i in 1:(N - 1)) {
    x <- extract.first.number(x)$rest
  }
  extract.first.number(x)$number
}

#' @title  Round off to given resolution 
#' @description Round off to given resolution. Resolution = mininum difference between 
#' two values in the output that we want to her about.    
#' @usage  round.resolution(rnorm(100),0.05)
#' @name round.resolution
#' @author Claus E. Andersen
#' @return A vector with the rounded numbers.
#' @param x is a vector of numeric  values
#' @param resolution is the resolution. For example, 0.2 means that the output can be 
#' 0, 0.2, 0.4, 0.6 etc. Resolution equal to 0.001 means that be can have 1.001, 1.002 etc.
#' @param offset is controlling the output as follows:
#'   If offset=0  then all input from 0 to 1.999... will give 2 as output etc.
#'   If offset=0.5 then all input from -1 to 0.999... will give 2 as output etc.
#'   If offset=1.0 then all input from 0 to 1.999... will give 2 as output etc.
#' @export round.resolution
round.resolution <- function(x, resolution = 1., offset = 0.)
{
  # Purpose: To round off x to given resolution. For example, if
  # x is a time scale, and if resolution is equal to 2, then the function
  # returns values like: ...-4,-2,0,2,4,6,8...
  # If offset=0  then all input from 0 to 1.999... will give 2 as output etc.
  # If offset=0.5 then all input from -1 to 0.999... will give 2 as output etc.
  # If offset=1.0 then all input from 0 to 1.999... will give 2 as output etc.
  # Created: October 17, 2005
  # Revised: October 17, 2005
  # Name:    Claus E. Andersen
  # Sample call:
  #   round.resolution(rnorm(100),0.05)
return(  ((((1. * (x + offset))/resolution) %/% 1) * resolution) )
}

#' @title  Round off to given number of decimals 
#' @description Round off to given number of decimals.
#' @usage  round.ca(rnorm(100),1)
#' @name round.ca
#' @author Claus E. Andersen
#' @return A vector with the rounded numbers.
#' @param x is a vector of numeric  values
#' @param decimals is the number of requested decimals
#' @export round.ca
round.ca <- function(x, decimals = 3, ...)
{
  # See format for further details.
  # Created: July 18, 2005
  # Revised: July 18, 2005
  # A call like round.ca(1024,3) returns "1024.000"
  # The function works well in the create.latex.table
  format(round(x, decimals), nsmall = decimals,...)
}

#' @title  Add leading blanks.
#' @description Add leading blanks. This is to align data in Latex tables.
#' @usage  leading.blanks(xx.LATEX,"x") See example in body of function.
#' @name leading.blanks
#' @author Claus E. Andersen
#' @return A vector with the rounded numbers.
#' @param df is dataframe
#' @param blank.char is the character to be added
#' @param extra.char is an extra character
#' @param min.width is the minimum width
#' @param max.width is the maximum width
#' @param ignore.first is gives special treatment to first column
#' @param trace equal to TRUE produces output during execution 
#' @export leading.blanks
leading.blanks <-  function(df, blank.char = ".", extra.char = " ", min.width = 1, max.width = 99,
           ignore.first = FALSE, trace = FALSE)
  {
    # Created: April 10, 2005
    # Revised: April 10, 2005
    # Name   : Claus E. Andersen
    # Main purpose: to align data in Latex tables.
    # The ignore.first parameters controls the first row (often a row with names).
    # For example:
    #   xx <- data.frame(sample(c(1,1055500,10),10,replace=T),sample(c("a","aba","sfgsdfgfhgd"),10,replace=T))
    #   xx.LATEX <- create.latex.table(xx)
    #   yy <- leading.blanks(xx.LATEX,"x")
    #   yy
    #   write.table(yy,"xx.txt",sep=" ",dimnames.write = F)
    for(i in 1:ncol(df)) {
      cc <- df[, i]
      cc <- as.character(cc)
      cc.all <- cc
      if(ignore.first) cc <- cc[-1]
      cc.width <- nchar(as.character(cc)) #S-plus: string.bounding.box(cc)$columns
      cc.max.width <- min(max.width, max(cc.width, min.width))
      cc.pure <- paste(rep(blank.char, cc.max.width), sep = "",
                       collapse = "")
      if(trace)
        print(paste("Col = ", i, " class = ", class(cc), 
                    cc.max.width))
      if(ignore.first) {
        df[, i] <- c(cc.all[1], paste(extra.char, paste(
          substring(cc.pure, 1, cc.max.width - cc.width),
          cc, sep = ""), sep = ""))
      }
      else {
        df[, i] <- paste(extra.char, substring(cc.pure, 1,
                                               cc.max.width - cc.width), cc, sep = "")
      }
    }
    df
  }

#' @title  Write a double line 
#' @description This function can be used to separate report output
#' @usage  wrline("End")
#' @name wrline 
#' @author Claus E. Andersen
#' @return NULL (the side effect is that a line is printed)
#' @param txt is an optional txt message that will occur on the line.
#' @param N.blanks is the number of blanks lines before the first dashed line
#' @param N.blanks.after is the number of blanks before the second dashed line
#' @param N is the total number of characters in the line.
#' @export 
wrline <- function(txt="",N.blanks=0,N.blanks.after=1,N=80){
  # This function can be used to separate report output
  # Created: July 10, 2001, Revised: July 10, 2001, Claue E. Andersen
  # Sample call: wrline("End")
  if(N.blanks> -1){# Write some blanks line
    L.blanks <- paste(rep("\n",N.blanks),collapse="")
    cat(L.blanks)
  }
  L <- paste(rep("-",N),collapse="")
  if (!txt==""){cat(L,"\n",txt,"\n")} # Add a line and some text
  cat(L,"\n")
  if(N.blanks.after> -1){# Write some blanks line
    L.blanks.after <- paste(rep("\n",N.blanks.after),collapse="")
    cat(L.blanks.after)
  }
  return()
}#End wrline


#' @title  codes.ca 
#' @description This function will be deleted.
#' @usage  codes.ca
#' @name codes.ca
#' @author Claus E. Andersen
#' @return codes
#' @param object 
#' @export codes.ca
codes.ca <- function(object)
{
  # This is an improved version of the function the standard function "codes".
  # It returns a numerical vector with the same length as object.
  # That vector contains the positions of each element in the object relative to
  # levels(object). The vector object has to have the classe factor or
  # ordered.
  # Created: Dec. 7, 2005
  # Revised: Dec. 7, 2005
  # Claus E. Andersen
  if(inherits(object, "ordered"))
    return(as.vector(unclass(object)))
  if(!inherits(object, "factor"))
    return(as.vector(object))
  match(object, levels(object))
}


#' @title  Convert a dateframe into a Latex table
#' @description Generate a table for Latex (with &'s and \\-line endings).
#          A function pretty.func can be supplied in the call.
#' @usage  see example in body of function.
#' @name create.latex.table 
#' @author Claus E. Andersen
#' @return NULL (the side effect is that a line is printed)
#' @param df is the dataframe to be converted.
#' @param pretty.func is a function needed to get the desired resultion for each individual column.
#' @param NL is the characted indicatinf new line.
#' @param col.sign is the character for column seperation.
#' @export create.latex.table
create.latex.table <-
  function(df, pretty.func = function(x, col.num = 1)
  {
    x
  }
  , NL = "\\\\", col.sign = "&")
  {
    # Library: clan
    # Created: March 22, 2002
    # Revised: March 22, 2002
    # Revised: April 13, 2005 Now, the NL-character is a parameter.
    # Revised: July 10, 2005 Now, the pretty.func also works on the last coloumn and its knows about the coloumn
    #          being handled.
    # Revised: July 19, 2005 updated sample calls
    # Revised: July 28, 2006 added the col.sign argument
    # Revised: August 7, 2006 Fixed bug such that the function can handle data frames with only one column
    # Name   : Claus E. Andersen
    # Task   : Generate a table for Latex (with &'s and \\-line endings).
    #          A function pretty.func can be supplied in the call.
    #          For example:
    #          ca <- function(x,col){y<-x;  if(class(x)=="numeric"){y <- signif(x,2)}; y}
    #          ca <- function(x,col){y<-x;  if(class(x)=="numeric"){ if(col==4) y <- round(x,1)  else  y <- round(x,4)};  y}
    #          ca <- function(x,col){y<-x;  if(class(x)=="numeric"){if(is.element(col,c(1,2))) y <- round.ca(x,0,scientific=c(-9,9)); if(is.element(col,c(3)))
    ##### y <- round.ca(x,2)  };  y}
    #          create.latex.table(data.frame(x=1:10,y=rnorm(10)*1e6,rep("test",10),yy=10000*rnorm(10)),ca)
    if(!is.null(df)) {
      N.cols <- ncol(df)
      N.rows <- nrow(df)
      ca <- pretty.func
      # Prepare a command that can assemble the columns into a new dataframe:
      # data.frame(org.name1 = x1, org.name2 = x2 etc. )
      # print(N.cols)
      # print(N.rows)
      new.line <- as.character("NL")
      df2 <- NULL
      if(T) {
        text <- "data.frame("
        if(N.cols > 1) {
          # More than one column
          for(i in 1:(N.cols - 1)) {
            text <- paste(text, "x", i, 
                          "=as.character(ca(df$", names(
                            df)[i], ",", i, ")), y", i,
                          "=rep(col.sign,", N.rows,
                          "),", sep = "")
          }
          # Special treatment of last element (no comma):
          i <- N.cols
          text <- paste(text, "x", i, 
                        "=as.character(ca(df$", names(df)[
                          i], ",", i, ")), y", i, "=rep(", 
                        new.line, ",", N.rows, "))", sep = "")
        }
        else {
          # Only one column
          text <- paste(text, "x", 1, 
                        "=as.character(ca(df$", names(df)[
                          1], ",", 1, ")), y", 1, "=rep(", 
                        new.line, ",", N.rows, "))", sep = "")
        }
        # Finally evaluate the command:
        # print(text)
        df2 <- eval(parse(text = text))
      }
      # Create line with labels:
      df3 <- NULL
      if(T) {
        text <- "data.frame("
        if(N.cols > 1) {
          # More than one column
          for(i in 1:(N.cols - 1)) {
            text <- paste(text, "x", i, 
                          "=as.character( '", names(
                            df)[i], "'), y", i, 
                          "=col.sign,", sep = "")
          }
          # Special treatment of last element (no comma):
          i <- N.cols
          text <- paste(text, "x", i, "=as.character( '",
                        names(df)[i], "'), y", i, "=", 
                        new.line, ")", sep = "")
        }
        else {
          # Only one column
          text <- paste(text, "x", 1, "=as.character( '",
                        names(df)[1], "'), y", 1, "=", 
                        new.line, ")", sep = "")
        }
        # Finally evaluate the command:
        # print(text)
        df3 <- eval(parse(text = text))
      }
      # Finally return the joined table:
      rr <- rbind(df3, df2)
    }
    else {
      # df was NULL
      rr <- NULL
    }
    rr
  }

#' @title  Convert from wall-clock time to fraction of day
#' @description Convert from wall-clock time to fraction of day.
#' Note: 1) Any delimiter will do, 2) seconds are option
#' @usage  dayno.clock(c("12:00","23:59","00:00:59","12.00.30","23x30"))
#' @name dayno.clock 
#' @author Claus E. Andersen
#' @return vector with fractional day numbers
#' @param Ttime is the time to be converted
#' @export dayno.clock
dayno.clock <- function(Ttime="00:00"){
  # Convert from wall-clock time to fraction of day
  # Sample call: dayno.clock(c("12:00","23:59","00:00:59","12.00.30","23x30"))
  # Observe: 1) Any delimiter will do, 2) seconds are option
  # Revised: July 10, 2001, Claus E. Andersen
  hh <- as.numeric(substring(Ttime, 1, 2))
  mm <- as.numeric(substring(Ttime, 4, 5))
  ss <- rep(0,length(Ttime))
  ii <- nchar(Ttime)==8
  ss[ii] <- as.numeric(substring(Ttime[ii], 7, 8))
  return((hh + mm/60+ss/3600)/24)}

#' @title  Calculate the difference between two time stamps
#' @description Dates and wall-clock times are used as input. The output is in number of days .
#' Note: 1) Any delimiter will do, 2) seconds are option
#' @usage  dayno.clock(c("12:00","23:59","00:00:59","12.00.30","23x30"))
#' @name dayno.calc 
#' @author Claus E. Andersen
#' @return The difference between the given time stamp and the reference time stamp in number of days day numbers
#' @param Tday is the date (e.g. "31.01.2001")
#' @param Ttime is the clock (e.g. "00:00")
#' @param ref.date is the reference date (e.g. "31.12.2000")
#' @param ref.time is the reference time (e.g. "00:00")
#' @export dayno.calc
dayno.calc <- function(Tday="31.01.2001",Ttime="00:00",ref.date="31.12.2000",ref.time="00:00"){
  # Calculate the dayno relative to some reference
  # Sample call: dayno.calc(c("31.03.2001","13.05.2001","14.06.2001","12.09.2001","01.12.2001","04.12.2001"),ref.date="31.03.2001")
  # Sample call: dayno.calc("10.01.2001",ref.date="31.12.2000",ref.time="24:00:00")
  # Revised: July 10, 2001, Claus E. Andersen
  # Revised for R: June 2, 2012 (requires chron library)
  dayno.ref <- as.integer(dates(as.character(ref.date),format="d.m.y"))+as.double(dayno.clock(ref.time))
  dayno.day <- as.integer(dates(as.character(Tday),format="d.m.y"))-dayno.ref
  dayno.time <- as.double(dayno.clock(Ttime))
  dayno <- as.double(dayno.day)+dayno.time
  return(dayno)
}

#' @title  Convert from  fraction of day to wall-clock time.
#' @description Convert from fraction of day to wall-clock time.
#' Full days are ignored (so 0.5, 1.5, and 5573476.5 all convert to "12:00").
#' Likewise, for example, 0.0 ane 1.0 both convert to "00:00" (Not "24:00").
#' Sample call: dayno.clock.reversed(c(0.0,0.3,4444.5)) (returns: "00:00",""07:12","12:00")
#' @usage  dayno.clock.reversed(c(0.0,0.3,4444.5))
#' @name dayno.clock.reversed 
#' @author Claus E. Andersen
#' @return vector with fractional day numbers
#' @param Ttime is the time to be converted
#' @export dayno.clock.reversed
dayno.clock.reversed <- function(Ttime = 0.5){
  # Convert fraction-of-day number to wall-clock time
  # Full days are ignored (so 0.5, 1.5, and 5573476.5 all convert to "12:00").
  # Likewise, for example, 0.0 ane 1.0 both convert to "00:00" (Not "24:00").
  # Sample call: dayno.clock.reversed(c(0.0,0.3,4444.5)) (returns: "00:00",""07:12","12:00")
  # Created: Jan 6, 2005, Claus E. Andersen
  Ttime <- Ttime-trunc(Ttime)
  hh <- trunc(Ttime*24)
  mm <- round((Ttime-hh/24)*24*60)
  paste(leading.zeros(hh,2),":",leading.zeros(mm,2),sep="")
}

#' @title  Convert from NTC to temperature using three parameters
#' @description Conversion for NTC (e.g. 2 kohm) thermistors (Risoe parameterization)
# The order of the coefficients is identical to that provided
# in the calibration certificates.
# See also the inverse function: thermistor.ohm
#' @usage  see body of function.
#' @name thermistor.degC 
#' @author Claus E. Andersen
#' @return the temperature in degC
#' @param b a vector of three elements.
#' @export thermistor.degC
thermistor.degC <- function(ohm=2004.532, b=c(0*5.21570,0*4294.32,0*310)){
  # Created: July 28, 2012
  # Conversion for 2 kohm thermistors (Risoe parameterization)
  # The order of the coefficients is identical to that provided
  # in the calibration certificates.
  # See also the inverse function: thermistor.ohm
  # Sample call: b.coeff <- c(5.21570,4294.32,310); thermistor.degC(thermistor.ohm(25,b.coeff),b.coeff)
  b[2] / (log(ohm) + b[1]) - b[3] 
}

#' @title  Convert from temperature to resistance using three parameters
#' @description Conversion for NTC (e.g. 2 kohm) thermistors (Risoe parameterization)
# The order of the coefficients is identical to that provided
# in the calibration certificates.
# See also the inverse function: thermistor.ohm
#' @usage  see body of function.
#' @name thermistor.ohm 
#' @author Claus E. Andersen
#' @return the resistance in ohm
#' @param b a vector of three elements.
#' @export thermistor.ohm
thermistor.ohm <- function(degC=25.0, b=c(0*5.21570,0*4294.32,0*310)){
  # Created: July 28, 2012
  # Conversion for 2 kohm thermistors (Risoe parameterization)
  # The order of the coefficients is identical to that provided
  # in the calibration certificates.
  # See also the inverse function: thermistor.degC
  # Sample call: b.coeff <- c(5.21570,4294.32,310); thermistor.degC(thermistor.ohm(25,b.coeff),b.coeff)
  exp(b[2]/(degC+b[3]) - b[1])
}

#' @title  Template for graphical output 
#' @description  Template for graphical output using pdf, ps or png format.
#' # Recommended workflow for making PowerPoint reports:
#' (1) Print all files to consequtive png-files
#' (2) Import to PowerPoint using the photoalbum feature.
#'     This puts each plot in separate slides in the presentation.
#' @usage  Do not run - just view the body of the function and use it as a template.
#' @name workflow.ca 
#' @author Claus E. Andersen
#' @return Side effect is that selected iportant libraries are installed
#' @export workflow.ca
workflow.ca <- function(){
  # This function includes a recommended wrapper for priducing plots
  # on screen or to files.
  
  # This code can be pasted into other scripts whenever needed.
  # Revised: May 20, 2012 
  # Claus E. Andersen
  
  close.device.wanted <- FALSE
  
  # Select some name:
  plot.file <- "R-plot"
  
  if(FALSE){
    png.fac <- 1.5
    png(filename = paste(plot.file,"%03d.png",sep=""),
        width = png.fac*20, height = png.fac*13, units = "cm", pointsize = png.fac*10,
        bg = "white", res = 600, family = "arial", restoreConsole = TRUE,
        type = c("windows", "cairo", "cairo-png")[1])
    close.device.wanted <- TRUE
  }
  
  if(FALSE){
    inch.fac <- 2.54 / 1.2
    pdf(paste(plot.file,".pdf",sep=""), width = 29.7/inch.fac, height = 21/inch.fac,pointsize=19,family="Courier")
    #dev.off()
    close.device.wanted <- TRUE
  }
  
  if(FALSE){
    postscript(paste(plot.file,".ps",sep=""),onefile=TRUE) 
    close.device.wanted <- TRUE
  }
  
  ## Place your plots and other code here
  plot(1:10)
  plot(1:100)
  
  # Close device - if plotting to files
  if(close.device.wanted) dev.off()
}# workflow.ca


#' @title  Function to ease the installation of important R packages
#' @description  Function to ease the installation of important R packages
#' @usage  clan.install()
#' @name clan.install 
#' @author Claus E. Andersen
#' @return Side effect is that selected iportant libraries are installed
#' @export clan.install
clan.install <- function(repos="http://cran.r-project.org"){
  print("Install important packages.")
  getOption("repos")

  install.packages("devtools")
  install.packages("roxygen2",repos=repos)
  install.packages("testthat",repos=repos)
  install.packages("knitr",repos=repos)
  
  install.packages("stringr",repos=repos)
  install.packages("dplyr",repos=repos)
  
  install.packages("gridBase",repos=repos)
  install.packages("gridExtra",repos=repos)
  install.packages("grid",repos=repos)
  install.packages("lattice",repos=repos)
  install.packages("latticeExtra",repos=repos)
  install.packages("ggplot2",repos=repos)

  install.packages("chron",repos=repos)
  install.packages("mgcv",repos=repos)
  install.packages("digest",repos=repos)
  install.packages("MASS",repos=repos)
  install.packages("tidyr",repos=repos)
  install.packages("jpeg",repos=repos)
  
  install_github("claus-e-andersen/clanTools")  
  install_github("claus-e-andersen/clanLattice")
  install_github("claus-e-andersen/clanOptim")
  install_github("claus-e-andersen/clanEgsnrc")
  install_github("claus-e-andersenclanMEView")
  }


#' @title  Version function for the clanTools library
#' @description  Version function for the clanTools library
#' @usage  clanTools()
#' @name clanTools 
#' @author Claus E. Andersen
#' @return list with information about the version and the functions in the library
#' @export 
clanTools <- function(){
  list(name="clanTools",
       version=0.008,
       date="August 3, 2014",
       functions=sort(c("clanTools","clan.install","wrline",
                        "replacechar","substitute.char","extract.first.number","extract.given.number",
                        "leading.zeros","leading.zeros.to.fit",
                        "round.resolution","round.ca","leading.blanks",
                        "first.element","last.element","most.common.element",
                        "workflow.ca",
                        "dayno.clock","dayno.calc","dayno.clock.reversed",
                        "thermistor.degC","thermistor.ohm","coefficients.ca","trim.whitespace"
       )))
}