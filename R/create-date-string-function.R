#' @title Create date string 
#' @description  Create data string of different formats. The
#' function is vectorized.
#' @usage  
#'   create.date.string(x=c("05.05.2014","06.05.2014","07.05.2014"),format="6") 
#' @name create.date.string 
#' @author Claus E. Andersen
#' @return string or list 
#' @param format: "1","2" .. "7" and "99"
#' @export create.date.string

create.date.string <- function(x="06.05.2014",format="5"){
  # Created: 2014
  # Revised: August 4, 2014
  # Name : Claus E. Andersen
  
  # Sample call: 
  # create.date.string(x=c("05.05.2014","06.05.2014","07.05.2014"),format="6") 
  
  mm <- as.integer(substring(x,4,5))
  dd <- as.integer(substring(x,1,2))
  yy <- as.integer(substring(x,7,10))
  tt <- as.integer(day.of.week(mm,dd,yy))
  NN <- length(x)
  
  mmm <- rep("Unknown",NN)
  
  ok <- mm==1
  if(sum(ok,na.rm=TRUE)>0){ mmm[ok]  <- "January"}
  ok <- mm==2
  if(sum(ok,na.rm=TRUE)>0){ mmm[ok] <- "February"}
  ok <- mm==3
  if(sum(ok,na.rm=TRUE)>0){ mmm[ok]  <- "March"}
  ok <- mm==4
  if(sum(ok,na.rm=TRUE)>0){ mmm[ok]  <- "April"}
  ok <- mm==5
  if(sum(ok,na.rm=TRUE)>0){ mmm[ok]  <- "May"}
  ok <- mm==6
  if(sum(ok,na.rm=TRUE)>0){ mmm[ok]  <- "June"}
  ok <- mm==7
  if(sum(ok,na.rm=TRUE)>0){ mmm[ok]  <- "July"}
  ok <- mm==8
  if(sum(ok,na.rm=TRUE)>0){ mmm[ok]  <- "August"}
  ok <- mm==9
  if(sum(ok,na.rm=TRUE)>0){ mmm[ok]  <- "September"}
  ok <- mm==10
  if(sum(ok,na.rm=TRUE)>0){ mmm[ok]  <- "October"}
  ok <- mm==11
  if(sum(ok,na.rm=TRUE)>0){ mmm[ok]  <- "November"}
  ok <- mm==12
  if(sum(ok,na.rm=TRUE)>0){ mmm[ok]  <- "December"}
  
  ttt <- rep("Unknown",NN)
  ok <- tt==1
  if(sum(ok,na.rm=TRUE)>0){ ttt[ok]  <- "Monday"}
  ok <- tt==2
  if(sum(ok,na.rm=TRUE)>0){ ttt[ok]  <- "Tuesday"}
  ok <- tt==3
  if(sum(ok,na.rm=TRUE)>0){ ttt[ok]  <- "Wedensday"}
  ok <- tt==4
  if(sum(ok,na.rm=TRUE)>0){ ttt[ok]  <- "Thursday"}
  ok <- tt==5
  if(sum(ok,na.rm=TRUE)>0){ ttt[ok]  <- "Friday"}
  ok <- tt==6
  if(sum(ok,na.rm=TRUE)>0){ ttt[ok]  <- "Saturday"}
  ok <- tt==7
  if(sum(ok,na.rm=TRUE)>0){ ttt[ok]  <- "Sunday"}
  xx <- paste(ttt,sep="")
  if(format=="2"){xx <- paste(dd,sep="")}
  if(format=="3"){xx <- paste(mmm,sep="")}
  if(format=="4"){xx <- paste(yy,sep="")}
  if(format=="5"){xx <- paste(mmm," ",dd," (",ttt,")",sep="")}
  if(format=="6"){xx <- paste(mmm," ",dd,", ",yy," (",ttt,")",sep="")}
  if(format=="7"){xx <- paste(ttt," (",mmm," ",dd,", ",yy,")",sep="")}
  if(format=="99"){xx <- list(day=ttt, day.no=dd,month=mmm,month.no=mm,year=yy)}
  return(xx)
} # create.date.string


