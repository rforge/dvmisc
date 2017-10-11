\name{true_range_i}
\alias{true_range_i}
\title{
True Range of Integer Values
}
\description{
Defined as the difference between the maximum and the minimum. Equivalent to 
base R code \code{diff(range(x))}, but much faster.
}
\usage{
true_range_i(x)
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
Integer value.
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
# In general, true_range_i is much faster than diff(range(x))
x <- rpois(1000, lambda = 5)
all.equal(diff(range(x)), true_range_i(x))
microbenchmark(diff(range(x)),
               true_range_i(x),
               times = 100)
}