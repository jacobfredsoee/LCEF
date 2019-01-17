function(x, by, decreasing = FALSE, na.last = NA) {
  if(!by %in% colnames(x)) {
    if(!is.na(as.numeric(by))) {
      return(x[order(x[,as.numeric(by)], na.last = na.last, decreasing = decreasing),])
    }
    stop(paste("ERROR: by-column (\"", by, "\") not found in column names", sep = ""))
  }
  return(x[order(x[,which(colnames(x) == by)], na.last = na.last, decreasing = decreasing),])
}
