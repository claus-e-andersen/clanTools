#' @title  To pad dots (or other text) to tables for improved readability.
#' @description 
#' Objective: To pad dots (or other text) to tables for improved readability. 
#' See also: \link{"table.dots.vec"}.
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
#' This is a vectorized version. See also \link{"table.dots"}.
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
#' @usage  table.dots("k.elec",N=40,before=!FALSE,st=".",st.space=" ")
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

