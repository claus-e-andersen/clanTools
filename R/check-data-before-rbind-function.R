#' @title  Check data before rbind 
#' @description
#' Help identify problems before blindly trying to rbind two dataframes.
#' @usage  
#' check.data.before.rbind(df0,df)
#' if(is.null(df)){df <- df0} {df <- rbind(df,df0)}
#' 
#' df \%>\% mutate(V.raw.sd = get.selected.data.sd(.,V.use.vec)) 
#' @name check.data.before.rbind
#' @author Claus E. Andersen
#' @return nothing (printed statement)
#' @param df.small data frame to be rbinded with a larger dataframe (df)
#''@param df the larger data frame
#' @param action what to do in case of error (="Stop.if.error" is default)
#' @export check.data.before.rbind

check.data.before.rbind <- function(df.small, df, action="Stop.if.error"){
  # Objective: Test before df <- rbind(df,df.small)
  # Created  : November 14, 2019
  # Revised  : November 14, 2019
  # Revised  : April 12, 2020    
  # Name     : Claus E. Andersen
  all.ok <- TRUE
  
  if(!is.null(df) && !is.null(df.small)){
    n.small  <- names(df.small)
    n        <- names(df)
    L.small  <- length(n.small)
    L        <- length(n)
    
    N <- max(L.small,L)
    vec.small <- c(n.small,rep("nothing",N))[1:N]
    vec <- c(n,rep("nothing",N))[1:N]
    df.test <- data.frame(n.small= vec.small, n=vec, stringsAsFactors=FALSE)
    df.test$ok <- df.test$n.small==df.test$n
    N.errors <- sum(!df.test$ok)
    
    if(N.errors>0){
      all.ok <- FALSE    
      print("check.data.before.rbind diagnostics")
      print("*** Head of df.small:")
      print(head(df.small))
      print("*** Head of df:")
      print(head(df))
      print(paste("Number of columns in df.small:",L.small))
      print(paste("Number of columns in df:",L))
      print("Problems are here:")
      print(df.test)
      print(paste("Number or errors =",N.errors))
    } # some errors
  } # not null
  
  if(all.ok){ print("check.data.before.rbind: ALL ok")} else {print("check.data.before.rbind: ERROR. Data in df.small do not match df."); stop("Sorry, but I quit!")}
} # check.data.before.rbind

