% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stringmanipulation.R
\name{datediffYears}
\alias{datediffYears}
\title{A function for calculating the number of years between two dates}
\usage{
datediffYears(date1, date2, d = 1)
}
\arguments{
\item{date1}{First date, oldest date}

\item{date2}{Second date, newest date}

\item{d}{Number of digits to round to}
}
\value{
a vector number of years between the two dates
}
\description{
A function for calculating the number of years between two dates
}
\examples{

datediffYears("1957-05-18", "2018-11-02")

#returns "61.5"

}
