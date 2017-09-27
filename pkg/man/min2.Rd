\name{min2}
\alias{min2}
\title{
Minimum (Alternate Implementation)
}
\description{
C++ version of base R function \code{\link{min}} that runs faster for long input
vectors, assuming \code{x.integer} is correctly specified.
}
\usage{
min2(x, x.integer = FALSE)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{x.integer}{
Logical value indicating whether \code{x} takes on integer values.
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
# For long vectors, min2 is faster than min
x <- rnorm(100000)
min(x) == min2(x)
microbenchmark(min(x),
               min2(x),
               times = 1000)

x <- rpois(100000, lambda = 5)
min(x) == min2(x, TRUE)
microbenchmark(min(x),
               min2(x, TRUE),
               times = 1000)

# For short vectors, min2 is slower than min
x <- rnorm(100)
min(x) == min2(x)
microbenchmark(min(x),
               min2(x),
               times = 1000)

x <- rpois(100, lambda = 5)
min(x) == min2(x, TRUE)
microbenchmark(min(x),
               min2(x, TRUE),
               times = 1000)
}
