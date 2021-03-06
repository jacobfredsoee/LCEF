#' A function for getting patient ID from biobank labels
#'
#'
#' @param samplesName One or more biobank labels. String pattern: XNNXNNNNNX, where X = letter, N = number, e.g. P28B00005E
#' @param asNum Should the results be return as integers, i.e. numeric. Defaults to TRUE
#' @return a vector with patient ids
#'
#' @examples
#' sampleNames = c("P04A08417A", "P04A08417A_postfix", "prefix_P04A08417A_postfix")
#' patientIDFromSample(sampleNames)
#'
#' returns: 8417, 8417, 8417
#'
patientIDFromSample = function(sampleNames, asNum = TRUE) {

  #sampleID string pattern: XNNXNNNNNX, where X = letter, N = number, e.g. P28B00005E
  patientID = sapply(sampleNames, function(sampleName) {
    startFix = regexec("P[0-9]{2}[A-Z]", sampleName)
    startPos = startFix[[1]][1] + as.numeric(attributes(startFix[[1]])[1])
    stopPos = as.numeric(attributes(regexec("P[0-9]{2}[A-Z][0-9]{5}", sampleName)[[1]])[1]) + startFix[[1]][1]

    substr(sampleName, startPos, (stopPos - 1))
  })

  if(asNum) return(as.integer(patientID))
  return(patientID)
}

#' A recursive function getting intersection from mutiple vectors.
#' does not promise to keep any sorting
#'
#' @param ... Two or more vectors
#' @return a vector with overlapping values from all passed vectors
#'
#' @examples
#' v1 = c(1,2,3,4,5)
#' v2 = c(1,2,3,4)
#' v3 = c(2,3,4,5)
#' v4 = c(4,3,2,6,1)
#'
#' intersectR(v1, v2, v3, v4)
#' #returns 4, 3, 2
#'
intersectR = function(...) {
  arguments = list(...)

  if(length(arguments) == 1) { #unlist from recursive calls
    arguments = arguments[[1]]
  }

  if(class(arguments) != "list" | length(arguments) == 1) { #something is not right here
    stop("something is wrong")
  }

  if(length(arguments) > 2) {
    arg1 = arguments[[length(arguments)]]
    arg2 = intersectR(arguments[1:(length(arguments)- 1)])

    return(intersect(arg1, arg2))
  } else {
    res = intersect(arguments[[1]], arguments[[2]])

    return(res)
  }
}

#' A function replacing mutiple values from a vector
#'
#' @param valueData Input
#' @param patterns A vector containing which values should be replaced
#' @param replacements A vector containing replacement values
#' @param other Any vales which is not found in 'patterns' is set to this. Defaults to NA
#' @return a vector with the replaced values
#'
#' @examples
#'
#' origin = c("gleason 3+3", "gleason 3+3=6", "gleason 3+3", "gleason 4+3=7", "gleason 3+4=7", "gleason 3+3", "gleason 7")
#' patternVector = c("gleason 3+3", "gleason 3+3=6", "gleason 4+3=7", "gleason 3+4=7")
#' replacementVector = c("1", "1", "3", "2")
#' valueConvert(origin, patternVector, replacementVector, "unknown")
#'
#' #returns "1", "1", "1", "3", "2", "1", "unknown"
#'
valueConvert = function(valueData, patterns, replacements, other = NA) {

  otherValues = !valueData %in% patterns

  for(i in 1:length(patterns)) {
    valueData[valueData == patterns[i]] = replacements[i]
  }

  valueData[otherValues] = other

  return(valueData)
}

#' A function replacing mutiple values from a vector, but using grep.
#' Thus, patterns can be part of the values, or any other pattern grep understands.
#'
#' @param valueData Input
#' @param patterns A vector containing which values should be replaced
#' @param replacements A vector containing replacement values
#' @param other Any vales which is not found in 'patterns' is set to this. Defaults to NA
#' @return a vector with the replaced values
#'
#' @examples
#'
#' origin = c("gleason 3+3", "gleason 3+3=6", "gleason 3+3", "gleason 4+3=7", "gleason 3+4=7", "gleason 3+3", "gleason 7")
#' patternVector = c("3[+]3", "4[+]3", "3[+]4")
#' replacementVector = c("1", "3", "2")
#' valueConvert(origin, patternVector, replacementVector, "unknown")
#'
#' #returns "1", "1", "1", "3", "2", "1", "unknown"
#'
valueConvertGrep = function(valueData, patterns, replacements, other = NA) {
  otherValues = rep(TRUE, length(valueData))

  for(i in 1:length(patterns)) {
    otherValues[grep(patterns[i], valueData)] = FALSE
    valueData[grep(patterns[i], valueData)] = replacements[i]
  }
  valueData[otherValues] = other

  return(valueData)
}

#' A function for calculating the number of years between two dates
#'
#' @param date1 First date, oldest date
#' @param date2 Second date, newest date
#' @param d Number of digits to round to
#' @return a vector number of years between the two dates
#'
#' @examples
#'
#' datediffYears("1957-05-18", "2018-11-02")
#'
#' #returns "61.5"
#'
datediffYears = function(date1, date2, d = 1) {

  YEAR_LENGTH = 365.24219

  date1 = try(as.Date(date1), silent = FALSE)
  date2 = try(as.Date(date2), silent = FALSE)

  as.numeric(round((date2 - date1) / YEAR_LENGTH, d))
}

#' A function for calculating the birthday from a CPR number, taking the century into account
#'
#' @param cpr CPR number. Any format will work.
#' @return a Date of the birthday
#'
#' @examples
#'
#' birthdayFromCPR("290511-4487")
#' returns "2011-05-29"
#'
#' birthdayFromCPR("290511-3487")
#' returns "1911-05-29"
#'
#'
birthdayFromCPR = function(cpr) {

  #First format the CPR
  if(is.na(cpr) | nchar(cpr) < 9 | (nchar(cpr) > 10 & !grepl("-", cpr))) stop("CPR number not in recognizable format")
  cpr = as.character(cpr)
  if(grepl("-", cpr, fixed = TRUE)) {
    if(nchar(cpr) == 11) finalCPR = gsub("-", "", cpr) #ok
    if(nchar(cpr) == 10) finalCPR = paste("0", gsub("-", "", cpr), sep = "") #needs padding
  } else
  {
    if(nchar(cpr) == 10) finalCPR = cpr #ok
    if(nchar(cpr) == 9) finalCPR = paste("0", cpr, sep = "") #needs padding
  }

  #Find dates
  year = substr(finalCPR, 5, 6)
  month = substr(finalCPR, 3, 4)
  day = substr(finalCPR, 1, 2)

  #Find century
  century = 19
  seven = as.numeric(substr(finalCPR, 7, 7))
  if ((seven == 4 | seven == 9)) {
    if(year <= 36) {
      century = 20
    } else {
      century = 19
    }
  } else if (seven >= 5 & seven <= 9) {
    if (year <= 57) {
      century = 20
    }
    else {
      century = 18
    }
  }

  return(paste0(century, year, "-", month, "-", day))
}
