\name{min_i}
\alias{min_i}
\title{
Minimum of Integer Values
}
\description{
Written in C++, this function tends to run faster than \code{\link{min}} for 
large integer vectors/matrices. Not valid for non-integer objects.
}
\usage{
min_i(x)
}
\arguments{
  \item{x}{
Integer vector.
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
# For large objects, min_i is faster than min
x <- rpois(100000, lambda = 5)
min(x) == min_i(x)
microbenchmark(min(x),
               min_i(x),
               times = 100)

# For smaller objects, min_i is slower than min
x <- rpois(100, lambda = 5)
min(x) == min_i(x)
microbenchmark(min(x),
               min_i(x),
               times = 100)
}
