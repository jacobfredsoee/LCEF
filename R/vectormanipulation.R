#' A function for collapsing 2 or more vectors containing NA values
#' Requires all vectors to have the same length
#'
#' Collapses from first to last
#'
#'
#' @param ... n vectors
#' @return a single vector
#'
#' @examples
#' v1 = c(1,NA,NA)
#' v2 = c(2,3,NA)
#' v3 = c(4,5,6)
#'
#' combineVectors(v1, v2, v3)
#' #returns 1, 3, 6
#'
combineVectors = function(...) {
  input_list = list(...)

  while(length(input_list) > 2) {
    input_list[[(length(input_list)-1)]] = combineVectors(input_list[[(length(input_list)-1)]], input_list[[(length(input_list)-0)]])
    input_list = input_list[1:(length(input_list)-1)]
  }

  p1 = input_list[[(length(input_list)-1)]]
  p2 = input_list[[(length(input_list)-0)]]

  if(length(p1) != length(p2)) stop("unequal lengths")
  for(i in 1:length(p1)) {
    if(is.na(p1)[i]) p1[i] = p2[i]
  }
  return(p1)
}
