\name{range_n}
\alias{range_n}
\title{
"Range" (Actually Minimum and Maximum) of Numeric Values
}
\description{
Written in C++, this function should always run faster than \code{\link{range}} 
for numeric vectors/matrices. For integer objects, \code{\link{range_i}} should 
run even faster. 
}
\usage{
range_n(x)
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
Numeric vector.
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
# In general, range_n is much faster than range.
x <- rnorm(1000)
all.equal(range(x), range_n(x))
microbenchmark(range(x),
               range_n(x),
               times = 100)
               
# For integer vectors, range_i should be even faster.
x <- rpois(1000, lambda = 5)
all.equal(range(x), range_i(x))
microbenchmark(range(x),
               range_n(x),
               range_i(x),
               times = 100)
}