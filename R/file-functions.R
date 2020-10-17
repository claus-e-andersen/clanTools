#' @title  Extract drive letter from full path (not vectorized)
#' @description 
#' fn the   full path to a single file. This function identifies the drive letter (if there is one).
#' There is a vectorized version of this function (see link below).
#' Three situations
#' (1) None :  No drive is given.  Drive=""          Rest=x
#' (2) One  :  The drive is given. Drive=before      Rest=after
#' (3) Two+ :  Nonsense syntax.    Drive=before.last Rest=after.last
#' See also \link{extract.drive.vec} and \link{split.filename}.
#' @usage  none
#' @name extract.drive
#' @author Claus E. Andersen
#' @return  list(org=fn, drive=before, rest=after)
#' @param fn is the full path to a file such as "c:/data/catest.txt".
#' @param drive.delimeter is normally ":".
#' @export extract.drive
extract.drive <- function(fn="c:/data/catest.txt",drive.delimeter=":"){
  # Created: September 10, 2013
  # Revised: September 10, 2013
  # Name   : Claus E. Andersen
  # fn is a vector of length one
  # Three situations
  # (1) None :  No drive is given.  Drive=""          Rest=x
  # (2) One  :  The drive is given. Drive=before      Rest=after
  # (3) Two+ :  Nonsense syntax.    Drive=before.last Rest=after.last
  pos <- gregexpr(drive.delimeter,fn)
  nn  <- sum(unlist(pos)>0)
  mm <- nchar(drive.delimeter)
  if(!mm==1)print(paste("Warning from extract.drive. Delimeter is not one char:",drive.delimeter))
  last.pos <- last.element(unlist(pos))
  before   <- substring(fn,1,last.pos)
  after    <- substring(fn,last.pos+1,nchar(fn))
  return(list(org=fn,drive=before,rest=after))
}# end extract.drive 


############################################################################
#' @title  Extract drive letter from full path (vectorized)
#' @description 
#' fn is a vector full paths to files. This function identifies the drive letter (if there is one).
#' Three situations
#' (1) None :  No drive is given.  Drive=""          Rest=x
#' (2) One  :  The drive is given. Drive=before      Rest=after
#' (3) Two+ :  Nonsense syntax.    Drive=before.last Rest=after.last
#' See also \link{extract.drive.vec} and \link{split.filename}.
#' @usage  none
#' @name extract.drive.vec
#' @author Claus E. Andersen
#' @return  list(org=fn, drive=before, rest=after)
#' @param fn.vec is a vector of full paths to files such as "c:/data/catest.txt".
#' @export extract.drive.vec
extract.drive.vec <- function(fn.vec="c:/data/catest.txt",...){
  # vectorized version of extract.drive
  return(lapply(fn.vec,extract.drive,...))
} # extract.drive.vec


############################################################################
#' @title  Extract drive, path, file name and extensionfrom full path (vectorized)
#' @description 
#' fn the full path to a single file. This function identifies the drive letter (if there is one).
#' There is a vectorized version of this function (see link below).
#' Three situations
#' (1) None :  No drive is given.  Drive=""          Rest=x
#' (2) One  :  The drive is given. Drive=before      Rest=after
#' (3) Two+ :  Nonsense syntax.    Drive=before.last Rest=after.last
#' See also \link{extract.drive} and \link{extract.drive.vec}.
#' @usage  none
#' @name split.filename
#' @author Claus E. Andersen
#' @return list(drive , path ,name, ext)
#' @param fn is the full path to a file such as "c:/data/catest.txt".
#' @export split.filename
split.filename <- function(fn="catest.txt"){
  # Created: September 10, 2013
  # Revised: September 10, 2013
  # Name   : Claus E. Andersen
  xxx <- extract.drive.vec(fn)
  drive.vec <- sapply(xxx,function(x)x$drive)
  drive.vec.rest <- sapply(xxx,function(x)x$rest)
  
  parts0 <- strsplit(drive.vec.rest,"/",fixed="TRUE")
  ff <- function(x){
    nn <- length(x); 
    res <- ""
    if(nn>1){res <- paste(x[-nn],collapse="/")}
    return(res)
  }
  path.vec <- sapply(parts0,ff)
  rest.vec <- sapply(parts0,last.element)
  
  ok <- is.na(path.vec)
  if(sum(ok)>0) path.vec[ok] <- ""
  ok <- is.na(rest.vec)
  if(sum(ok)>0) rest.vec[ok] <- ""
  
  parts <- strsplit(rest.vec,".",fixed="TRUE")
  length.vec <- sapply(parts,length)
  
  ff2 <- function(x){
    nn <- length(x)
    if(nn==0){x <- ""}
    res <- x 
    if(nn>1){res <- paste(x[-nn],collapse=".")}
    return(res)
  }
  ff3 <- function(x){
    nn <- length(x); 
    res <- ""
    if(!x[1]=="" & nn>1){res <- x[nn]}
    return(res)
  }
  name.vec <- sapply(parts,ff2)
  ext.vec  <- sapply(parts,ff3)
  
  ok <- is.na(name.vec)
  if(sum(ok)>0) name.vec[ok] <- ""
  ok <- is.na(ext.vec)
  if(sum(ok)>0) ext.vec[ok] <- ""
  
  
  return(list(drive=drive.vec,path=path.vec,name=name.vec,ext=ext.vec))
}# split.filename



############################################################################
#' @title  dayno.from.fileinfo
#' @description 
#' # The function assumes the following format of the fileinfo
#' times: "2020-07-15 16:38:28 CEST"
#' Sample call: 
#'   xx <- file.info(c("alanine-mass-001.txt"))
#'   xx.dayno <- dayno.from.fileinfo(xx$mtime)
#' @usage  none
#' @name dayno.from.fileinfo
#' @author Claus E. Andersen
#' @return dayno
#' @param ftime is the output from file.info("some-file-name").
#' @param format is reserved for future use (currently there is only one format). 
#' @export dayno.from.fileinfo
dayno.from.fileinfo <- function(ftime="2020-07-15 16:38:28 CEST",format=1){
  # Created: July 27, 2020
  # Revised: July 27, 2020
  # Name: Claus E. Andersen
  # The function assumes the following format of the fileinfo
  # times: "2020-07-15 16:38:28 CEST"
  # Sample call: 
  #   xx <- file.info(c("alanine-mass-001.txt"))
  #   xx.dayno <- dayno.from.fileinfo(xx$mtime)
  xx <- ftime
  xx.day   <- substring(xx,9,10)
  xx.month <- substring(xx,6,7)
  xx.year  <- substring(xx,1,4)
  xx.hr    <- substring(xx,12,13)
  xx.min   <- substring(xx,15,16)
  xx.sec   <- substring(xx,18,19)
  
  xx.Tday <- paste(xx.day,".",xx.month,".",xx.year,sep="")
  xx.Ttime <- paste(xx.hr,":",xx.min,":",xx.sec,sep="")
  
  dayno <- dayno.calc(Tday=xx.Tday,Ttime=xx.Ttime)
  return(dayno)
}# dayno.from.fileinfo


