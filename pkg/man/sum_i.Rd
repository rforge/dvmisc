\name{sum_i}
\alias{sum_i}
\title{
Sum of Integer Values
}
\description{
Written in C++, this function runs faster than \code{\link{sum}} for large 
integer vectors/matrices.
}
\usage{
sum_i(x)
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
# For very large integer objects, sum_i is faster than sum
x <- rpois(100000, lambda = 5)
sum(x) == sum_i(x)
microbenchmark(sum(x),
               sum_i(x),
               times = 100)

# For smaller integer objects, sum_i is slower than sum
x <- rpois(1000, lambda = 5)
sum(x) == sum_i(x)
microbenchmark(sum(x),
               sum_i(x),
               times = 100)
}
