
#' predAssertions
#'
#' Assert things about predictions
#'
#' @param x A vector of predictions
#'
#' @return TRUE or FALSE depending on the result
#'
#' @examples
#' tdat
#' predAssertions(tdat$pred)

predAssertions <- function(x){
   if(!class(x) == "numeric") stop("Prediction vector is not numeric")
   if(!min(x) >= 0) stop(paste("Minimum value of prediction vector is less than zero:",
                              min(x)))
   if(!max(x) <= 1) stop(paste("Maximum value of prediction vector is more than one:",
                               max(x)))
   TRUE
}

#' actAssertions 
#'
#' Assert things about cases 
#'
#' @param x A vector of actual cases  
#'
#' @return TRUE or FALSE depending on the result
#'
#' @examples
#' tdat
#' actAssertions(tdat$actual)

actAssertions <- function(x){
   if(!class(x) == "numeric") stop("Case vector is not numeric")
   if(!length(unique(x)) <= 2 ) stop("Case vector contains too many unique values")
   if(!all(unique(x) %in% c(0,1))) {
      stop("Case vector must only contain the values 0 and 1")
   }

   TRUE
}
