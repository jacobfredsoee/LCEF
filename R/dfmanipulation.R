#' A function for sorting a data.frame by colname
#'
#'
#' @param x data.frame
#' @param by colname (string) to sort by
#' @param decreasing passed on, defaults to FALSE
#' @param na.last passed on, defaults to NA
#' @return A sorted data.frame
#' @examples
#' set.seed(42)
#' df = data.frame(start = 1:20,
#'                 ran = sample(1:20, replace = TRUE))
#'
#' sortby(df, "ran")

sortby = function(x, by, decreasing = FALSE, na.last = NA) {
  if(!by %in% colnames(x)) {
    if(!is.na(as.numeric(by))) {
      return(x[order(x[,as.numeric(by)], na.last = na.last, decreasing = decreasing),])
    }
    stop(paste("ERROR: by-column (\"", by, "\") not found in column names", sep = ""))
  }
  return(x[order(x[,which(colnames(x) == by)], na.last = na.last, decreasing = decreasing),])
}
