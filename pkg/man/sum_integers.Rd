\name{sum_integers}
\alias{sum_integers}
\title{
Sum of Integer Vector
}
\description{
Calculates sum of values in an integer vector \code{x}. Slower than base R
function \code{\link{sum}} for short vectors, but much faster for long vectors.
}
\usage{
sum_integers(x)
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
# For long integer vectors, sum_integers is faster than sum
x <- rpois(100000, lambda = 5)
sum(x) == sum_integers(x)
microbenchmark(sum(x),
               sum_integers(x),
               times = 1000)

# For short integer vectors, sum_integers is slower than sum
x <- rpois(1000, lambda = 5)
sum(x) == sum_integers(x)
microbenchmark(sum(x),
               sum_integers(x),
               times = 1000)
}
