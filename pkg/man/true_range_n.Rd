\name{true_range_n}
\alias{true_range_n}
\title{
True Range of Numeric Values
}
\description{
Defined as the difference between the maximum and the minimum. Equivalent to 
base R code \code{diff(range(x))}, but much faster. For integer objects, 
\code{\link{true_range_i}} should run even faster.
}
\usage{
true_range_n(x)
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
# In general, true_range_n is much faster than diff(range(x))
x <- rnorm(1000)
all.equal(diff(range(x)), true_range_n(x))
microbenchmark(diff(range(x)),
               true_range_n(x),
               times = 100)
               
# For integer vectors, true_range_i should be even faster.
x <- rpois(1000, lambda = 5)
all.equal(diff(range(x)), true_range_i(x))
microbenchmark(diff(range(x)),
               true_range_n(x),
               true_range_i(x),
               times = 100)
}