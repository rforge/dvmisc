\name{range_i}
\alias{range_i}
\title{
"Range" (Actually Minimum and Maximum) of Integer Values
}
\description{
Written in C++, this function should always run faster than \code{\link{range}} 
for integer vectors/matrices. Not valid for non-integer objects.
}
\usage{
range_i(x)
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
Integer vector.
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
# In general, range_i is much faster than range.
x <- rpois(1000, lambda = 5)
all.equal(range(x), range_i(x))
microbenchmark(range(x),
               range_i(x),
               times = 100)
}