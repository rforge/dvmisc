% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{diff1_i}
\alias{diff1_i}
\title{1-Unit Lagged Differences for Integer Values}
\usage{
diff1_i(x)
}
\arguments{
\item{x}{Integer vector.}
}
\value{
Integer vector.
}
\description{
Written in C++, this function should always run faster than 
\code{\link[base]{diff}} for calculating differences between adjacent values 
of an integer vector.
}
\examples{
# diff1_i is typically much faster than diff
x <- rpois(1000, lambda = 5)
all.equal(diff(x), diff1_i(x))
benchmark(diff(x), diff1_i(x), replications = 2000)

}
