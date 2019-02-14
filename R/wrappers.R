#' A wrapper for trycatch, which allows ignoreing of errors and leave the loop running.
#' This is terrible behavior and should only be used when necessary
#'
#' @param ... whatever function you want to ignore errors from
#' @return output of the function, or NA if something failed
#'
#' @examples
#' data = matrix(rnorm(40), ncol = 8)
#' data[,5] = NA
#'
#' for(i in 2:ncol(data)) {
#'   result = t.test(data[,(i-1)], data[,i])$p.value
#'   print(result)
#' }
#' #This halts execution at the 5'th cloumn
#'
#' for(i in 2:ncol(data)) {
#'   result = t.test(data[,(i-1)], data[,i])$p.value
#'   print(result)
#' }
#' #Works, but returns NA when column 5 is being used
#'
errorcare = function(...) {
tryCatch(...,
         error=function(cond) {
           return(NA)
         })
}


