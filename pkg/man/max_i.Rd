\name{max_i}
\alias{max_i}
\title{
Maximum of Integer Values
}
\description{
Written in C++, this function tends to run faster than \code{\link{max}} for 
large integer vectors/matrices. Not valid for non-integer objects.
}
\usage{
max_i(x)
}
\arguments{
  \item{x}{
Numeric vector.
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
# For large objects, max_i is faster than max
x <- rpois(100000, lambda = 5)
max(x) == max_i(x)
microbenchmark(max(x),
               max_i(x),
               times = 100)

# For smaller objects, max_i is slower than max
x <- rpois(100, lambda = 5)
max(x) == max_i(x)
microbenchmark(max(x),
               max_i(x),
               times = 100)
}
