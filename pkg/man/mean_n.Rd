\name{mean_n}
\alias{mean_n}
\title{
Mean of Numeric Values
}
\description{
Defined simply as \code{sum(x) / length(x)}, this function seems to always run
faster than \code{\link{mean}} for numeric, non-integer vectors/matrices. For 
integer objects, \code{\link{mean_i}} should run even faster.
}
\usage{
mean_n(x)
}
\arguments{
  \item{x}{
Numeric vector or matrix.
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
# In general, mean_n is much faster than mean.
x <- rnorm(10000)
mean(x) == mean_n(x)
microbenchmark(mean(x),
               mean_n(x),
               times = 100)

# For very large integer objects, mean may be faster than mean_n. But then 
# mean_i should be even faster.
x <- rpois(100000, lambda = 5)
mean(x) == mean_n(x)
mean(x) == mean_i(x)
microbenchmark(mean(x),
               mean_n(x),
               mean_i(x),
               times = 100)
}
