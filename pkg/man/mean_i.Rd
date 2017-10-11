\name{mean_i}
\alias{mean_i}
\title{
Mean of Integer Values
}
\description{
Written in C++, this function should always run faster than \code{\link{mean}} 
for integer vectors/matrices. Not valid for non-integer objects.
}
\usage{
mean_i(x)
}
\arguments{
  \item{x}{
Integer vector or matrix.
}
}
\details{
NA
}
\value{
Numeric value.
}
\note{
NA
}
\references{
Acknowledgment: This material is based upon work supported by the National
Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
NA
}
\examples{
# For integer objects, mean_i is typically much faster than mean.
x <- rpois(100, lambda = 5)
mean(x) == mean_i(x)
microbenchmark(mean(x),
               mean_i(x),
               times = 1000)
}
