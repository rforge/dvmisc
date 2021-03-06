% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi3.R
\name{bmi3}
\alias{bmi3}
\title{Convert Continuous BMI Values into 3-Level Factor}
\usage{
bmi3(x, labels = TRUE)
}
\arguments{
\item{x}{Numeric vector of BMI values.}

\item{labels}{If \code{TRUE}, factor levels are labeled 
\code{"Normal weight"}, \code{"Overweight"}, and \code{"Obese"}; if 
\code{FALSE}, factor levels are \code{[-Inf, 25)}, \code{[25, 30)}, and 
\code{[30, Inf)}.}
}
\value{
Factor variable with 3 levels.
}
\description{
Converts a continuous BMI variable into a 3-level factor variable: Normal 
weight if \code{[-Inf, 25)}, Overweight if \code{[25, 30)}, and Obese if 
\code{[30, Inf)}.
}
