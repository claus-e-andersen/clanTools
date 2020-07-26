#' @title  To pad dots (or other text) to tables for improved readability.
#' @description 
#' Objective: To pad dots (or other text) to tables for improved readability. 
#' See also: \link{table.dots.vec} and \link{substring.with.dots}.
#' Sample call: 
#' 
#' table.dots("k.elec",N=40,before=!FALSE,st=".",st.space=" ")
#' 
#' table.dots("k.elec",N=40,before=FALSE,st="Error ",st.space=" = ")
#' 
#' Produces this output:
#' 
#' [1] "................................. k.elec"
#' 
#' [1] "k.elec = Error Error Error Error Error E"
#' 
#' @usage  table.dots("k.elec",N=40,before=!FALSE,st=".",st.space=" ")
#' @name table.dots
#' @author Claus E. Andersen
#' @return string vector
#' @param txt A single string (not a vector).
#' @param N number of dots (e.g. 40).
#' @param before should be set to TRUE if the dots should preceed the txt.
#' @param st is the dot-string (e.g. ".").
#' @param st.space (e.g. " = ").
#' @export table.dots
table.dots <- function(txt,N=40,before=TRUE,st=".",st.space=""){
  # Objective: To pad dots (or other text) to tables for improved readability.
  # Created: October 16, 2016
  # Revised: October 16, 2016
  # Revised: October 22, 2016 (handling of no txt)
  # Name   : Claus E. Andersen
  # Sample call: 
  # table.dots("k.elec",N=40,before=!FALSE,st=".",st.space=" ")
  # table.dots("k.elec",N=40,before=FALSE,st="Error ",st.space=" = ")
  # Produces this output:
  # [1] "................................. k.elec"
  # [1] "k.elec = Error Error Error Error Error E"
  if(is.null(txt)){txt <- "NA"}
  if(is.na(txt)){txt <- "NA"}
  dots <- paste(rep(st,N),collapse="")
  N.txt <- nchar(txt)
  N.space <- nchar(st.space)
  N.dots <- N - N.txt - N.space
  txt.out <- txt
  if(N.dots > 0){
    if(before){
      txt.out <- paste(substring(dots,1,N.dots),st.space,txt,sep="")
    } else {
      txt.out <- paste(txt,st.space,substring(dots,1,N.dots),sep="")
    }
  }
  txt.out
} # table.dots


#' @title  To pad dots (or other text) to tables for improved readability. This is a vectorized version.
#' @description 
#' Objective: To pad dots (or other text) to tables for improved readability.
#' This is a vectorized version. See also \link{table.dots} and \link{substring.with.dots}.
#' Sample call: 
#' 
#' table.dots.vec(c("k.elec","k.elec.pos","k.elec.mean"),N=40,before=FALSE,st=".",st.space=" ") 
#' 
#' Produces this output:
#' 
#' [1] "k.elec ................................."
#' 
#' [2] "k.elec.pos ............................."
#' 
#' [3] "k.elec.mean ............................"
#' 
#' @usage  table.dots.vec(c("k.elec","k.elec.pos","k.elec.mean"),N=40,before=FALSE,st=".",st.space=" ")
#' @name table.dots.vec
#' @author Claus E. Andersen
#' @return string vector
#' @param txt vector of strings.
#' @param N number of dots (e.g. 40).
#' @param before should be set to TRUE if the dots should preceed the txt. 
#' @param st is the dot-string (e.g. ".").
#' @param st.space (e.g. " = ").
#' @export table.dots.vec
table.dots.vec <- function(txt.vec,N=40,before=TRUE,st=".",st.space=""){
  # Objective: To pad dots (or other text) to tables for improved readability.
  #            This is a vectorized version
  # Created: October 16, 2016
  # Revised: October 16, 2016
  # Name   : Claus E. Andersen# Sample call:
  # table.dots.vec(c("k.elec","k.elec.pos","k.elec.mean"),N=40,before=FALSE,st=".",st.space=" ") 
  # Produces this output:
  # [1] "k.elec ................................."
  # [2] "k.elec.pos ............................."
  # [3] "k.elec.mean ............................"
  txt.vec.out <- txt.vec
  for(ii in 1:length(txt.vec)){
    txt.vec.out[ii] <- table.dots(txt.vec[ii],N=N,before=before,st=st,st.space=st.space)
  }
  txt.vec.out
}# table.dots.vec



#' @title  To pad dots (or other text) to tables for improved readability.
#' @description 
#' Objective: truncate string to n chars and pad with dots in front
#' behind depending on reverse.
#' Example:
#' 
#' substring.with.dots("Hello")
#' 
#' gives the following output:
#' 
#' "Hello .............."
#' 
#' See also \link{table.dots} and \link{substring.with.dots.adv}.
#' @usage substring.with.dots("Hello")
#' @name substring.with.dots
#' @author Claus E. Andersen
#' @return string (vectorized)
#' @param x is a vector of input values
#' @param n combined length (incl. dots)
#' @param reverse (FALSE = x then dots, TRUE = dots then x)
#' @param st is the dot-character (i.e. normally ".").
#' @export substring.with.dots
substring.with.dots <- function(x,n=20,reverse=FALSE,st="."){
  # Created: Feb. 22, 2015
  # revised: March 28, 2020
  # Objective: truncate string to n chars and pad with dots in front
  # or behind depending on reverse.
  x <- substring(x,1,n)
  pp <- paste(rep(st,n),collapse='')
  if(reverse){
    y <- paste(pp,x)
    nn <- nchar(y)
    res <- substring(y,pmax(1,nn-n),nn)
  } else {
    y <- paste(x,pp)
    nn <- nchar(y)
    res <- substring(y,1,n)
  }
  res
}# substring.with.dots



#' @title  Extract the tail part of a string
#' @description This function extracts the last n characters
#' from a string.
#' @usage 
#' extract.last.part.of.string(c("Hello","khdskhdsjkdkahdshkdhkahskaksdjh"),n=5)
#' @name extract.last.part.of.string 
#' @author Claus E. Andersen
#' @return string with given length taken from the tail
#' @param x string to be modified
#' @param n desired length of string 
#' @export extract.last.part.of.string
extract.last.part.of.string <- function(x="string",n=3){
  NN <- nchar(x)
  NN1 <- max(1,NN-n)
  NN2 <- NN
  return(substring(x,NN1,NN2))
}# extract.last.part.of.string



#' @title  Produce a string with given length including head, tail and midsection
#' @description Sometimes we need to summarize long strings, and we need 
#' a bit og the head and a bit of the tail, and some indication that
#' we are not seeing the full string. This function does that plus it can 
#' pad dots or other symbons before of after the string, if it is not
#' long.
#' See also \link{table.dots} and \link{substring.with.dots}.
#' @usage 
#' substring.with.dots.adv(c("Hello","khdskhdsjkdkahdshkdhkahskaksdjh"),n=15,reverse=!TRUE,n.last=5)
#' print(c("Hello ........." ,"khdskhd...aksdj"))
#' @name substring.with.dots.adv 
#' @author Claus E. Andersen
#' @return string with given length including head, tail and a midsection.
#' @param x string to be modified
#' @param n desired length of string after manipulations
#' @param reverse controls if the dots are added before of after the main string
#' @param n.last number of characters from the tail
#' @param mid.string what to put between the head and the tail to indicate that some part of the mid section has been cut out.
#' @export substring.with.dots.adv
substring.with.dots.adv <- function(x,n=30,reverse=FALSE,st=".",n.last=10,mid.string="..."){
  # n is the max length of the string
  # n.last is the minimum number of characters to be included from the end.
  # The selected strting will padded with dots (or some other character) to
  # have the length n. 
  # Sample call: substring.with.dots.adv(c("Hello","khdskhdsjkdkahdshkdhkahskaksdjh"),n=15,reverse=!TRUE,n.last=5)
  # Output: "Hello ........." "khdskhd...aksdj"
  n <- max(n,n.last)
  y <- clanTools::extract.last.part.of.string(x,n=n.last)
  NN <- nchar(x)
  y <- substring(x,1,n)
  
  # Too long (take first.part + mid.string + last.part
  ok <- NN > n
  if(sum(ok)>0){
    n.first <- n - n.last - nchar(mid.string)
    n.first <- max(n.first,1)
    y[ok] <- paste(substring(x[ok],1,n.first),mid.string,clanTools::extract.last.part.of.string(x[ok],n=n.last),sep="")
  }
  
  y <- clanTools::substring.with.dots(x=y,n=n+1,reverse=reverse)
  
  return(y)
}# substring.with.dots.adv


