% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stringmanipulation.R
\name{valueConvert}
\alias{valueConvert}
\title{A function replacing mutiple values from a vector}
\usage{
valueConvert(valueData, patterns, replacements, other = NA)
}
\arguments{
\item{valueData}{Input}

\item{patterns}{A vector containing which values should be replaced}

\item{replacements}{A vector containing replacement values}

\item{other}{Any vales which is not found in 'patterns' is set to this. Defaults to NA}
}
\value{
a vector with the replaced values
}
\description{
A function replacing mutiple values from a vector
}
\examples{

origin = c("gleason 3+3", "gleason 3+3=6", "gleason 3+3", "gleason 4+3=7", "gleason 3+4=7", "gleason 3+3", "gleason 7")
patternVector = c("gleason 3+3", "gleason 3+3=6", "gleason 4+3=7", "gleason 3+4=7")
replacementVector = c("1", "1", "3", "2")
valueConvert(origin, patternVector, replacementVector, "unknown")

#returns "1", "1", "1", "3", "2", "1", "unknown"

}
