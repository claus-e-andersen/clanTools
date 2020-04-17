#' @title  Improved function for computation of row means (row-wise means).
#' @description 
#' The R base function rowMeans does not work if there is only one column!
#' This function mitigates that problem. vec is a vector of column names in df such as, 
#' e.g. c("degC.W1","degC.W2").
#' See also \link{get.selected.data} and \link{get.selected.data.sd}.
#' @usage  rowMeans.ca(df,c("degC.W1","degC.W2"))
#' @name rowMeans.ca
#' @author Claus E. Andersen
#' @return string vector
#' @param df data frame
#' @param vec names of rows to be averaged 
#' @export rowMeans.ca
rowMeans.ca <- function(df,vec){
  # vec is a vector of column names in df such as, e.g. c("degC.W1","degC.W2")
  # Notice that rowMeans does not work if there is only one column!
  done <- FALSE
  if(!done && length(vec)==0){done <- TRUE; res <- rep(NA,nrow(df))}
  if(!done && is.na(vec))    {done <- TRUE; res <- rep(NA,nrow(df))}
  if(!done && length(vec)==1){done <- TRUE; res <- df[,vec]}
  if(!done && length(vec)>1) {done <- TRUE; res <- rowMeans(df[,vec], na.rm=TRUE)} 
  res
}# rowMeans.ca



#' @title  Extract columns from dataframe and compute row means (row-wise means).
#' @description
#' Useful function for extracting data and combining data based on a vector of column names in a dataframe.
#' If name.vec is NULL, then assign the value given in null.default
#' Used in: step42-electrometer-cal-single-file-009-6
#' Se also \link{rowMeans.ca} and \link{get.selected.data.sd}.
#' 
#' Sample use: read.table(fn.full,sep=";",header=TRUE,quote = "\"",) \%>\%
#' 
#'            filter(step.i \%in\% use.these.steps) \%>\%
#'            
#'            mutate(V.base = get.selected.data(.,V.base.use.vec)) \%>\%
#'            
#'            mutate(V.raw = get.selected.data(.,V.use.vec)) \%>\%
#'            
#'            mutate(V.ref = V.raw - V.base)
#'             
#' @usage df \%>\% mutate(V.base = get.selected.data(.,V.base.use.vec)) 
#' @name get.selected.data
#' @author Claus E. Andersen
#' @return string vector
#' @param df data frame
#' @param name.vec names of rows. 
#' @param null.default value to use if there are no values (default to NA).
#' @export get.selected.data
get.selected.data <- function(df,name.vec,null.default=NA){
  # Useful function for extracting data and combining data based on a vector of column names in a dataframe.
  # If name.vec is NULL, then assign the value given in null.default
  # Used in: step42-electrometer-cal-single-file-009-6
  # Sample use: read.table(fn.full,sep=";",header=TRUE,quote = "\"",) %>%
  #             filter(step.i %in% use.these.steps) %>%
  #             mutate(V.base = get.selected.data(.,V.base.use.vec)) %>%
  #             mutate(V.raw = get.selected.data(.,V.use.vec)) %>%
  #             mutate(V.ref = V.raw - V.base) %>%
  print(paste('get.selected.data',' ',name.vec))
  if(is.null(name.vec)){res <- rep(null.default,nrow(df))} else {
    for(col.sel in name.vec){
      if(!col.sel %in% names(df)) stop(paste("Error.",col.sel,"was not found. \nname.vec:", paste(name.vec,collapse=" ")))
    }
    
    
    name.vec.column.no <- match(name.vec,names(df))
    
    if(length(name.vec)==1){
      # If there is only one value, we actually do not need
      # to do row means, and the data therefore even do not 
      # need to be numeric.
      df %>% select(name.vec.column.no) -> res
      # For some reason, the following line is required to make the results
      # work with dplyr.
      res <- res[,1] 
      if(all(is.na(res))){res <- rep(null.default,nrow(df))} 
      ok <- class(res) %in% c("numeric","integer")
      if(!ok){
        print(res)
        print(class(res))
        print("Message from get.selected.data")
        stop("All the selected columns must be of class numeric or integer")
      }
    } else {
      # Numeric data required !
      # Row means will be formed
      df %>%  select(name.vec.column.no) -> df.extracted
      
      # Test is the class of all the selected columns are actually of numeric/integer class:
      ok <- all(sapply(df.extracted,class) %in% c("numeric","integer"))
      if(!ok){
        print(head(df.extracted))
        print(str(df.extracted))
        print(sapply(df.extracted,class))
        print("Message from get.selected.data")
        stop("All the selected columns must be of class numeric or integer")
      }
      
      df.extracted %>% rowMeans(.,na.rm=TRUE) -> res
    }
  }
  res
}# get.selected.data


#' @title  Extract columns from dataframe and compute row standard deviations (row-wise standard deviations).
#' @description
#' Useful function for extracting data and combining data based on a vector of column names in a dataframe.
#' Useful function for extracting data and combining data based on a vector of column names in a dataframe.
#' See \link{get.selected.data} and \link{rowMeans.ca}.
#' Here we compute (row by row) the standard deviation of the selected colums
#' @usage  df \%>\% mutate(V.raw.sd = get.selected.data.sd(.,V.use.vec)) 
#' @name get.selected.data.sd
#' @author Claus E. Andersen
#' @return string vector
#' @param df data frame
#' @param name.vec names of rows. 
#' @param null.default (e.g. 0.0).
#' @export get.selected.data.sd
get.selected.data.sd <- function(df,name.vec,null.default=0.0){
    # Useful function for extracting data and combining data based on a vector of column names in a dataframe.
    # See get.selected.data
    # Here we compute (row by row) the standard deviation of the selected colums
    res <- 0.0
    res2 <- 0.0
    result <- 0.0
    if(is.null(name.vec)){res <- rep(null.default,nrow(df))} else {
      for(col.sel in name.vec){
        if(!col.sel %in% names(df)) stop(paste("Error.",col.sel,"was not found. \nname.vec:", paste(name.vec,collapse=" ")))
      }
      name.vec.column.no <- match(name.vec,names(df))
      
      df %>% 
        select(name.vec.column.no) %>% 
        rowMeans -> res
      
      sq <- function(x){x*x}
      
      df %>% 
        select(name.vec.column.no) %>% 
        sq(.) %>%
        rowMeans -> res2
    }
    
    # We use (see for example, Bevington & Robinson p. 15) that:
    # var = <x^2> - mu^2
    # and we mu ) <x> and adjust for the smaller
    # degree of fredom.
    # Finally, we take sd = sqrt(var)
    
    N <- length(name.vec)
    if(N>1){
      result <- sqrt((res2-res*res)*N/(N-1))
    } else {result <- 0.0}
    result
  } # get.selected.data.sd

