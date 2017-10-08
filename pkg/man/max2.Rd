\name{max2}
\alias{max2}
\title{
Maximum (Alternate Implementation)
}
\description{
C++ version of base R function \code{\link{max}} that runs faster for long input
vectors, assuming \code{x.integer} is correctly specified.
}
\usage{
max2(x, x.integer = FALSE)
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
# For long vectors, max2 is faster than max
x <- rnorm(100000)
max(x) == max2(x)
microbenchmark(max(x),
               max2(x),
               times = 100)

x <- rpois(100000, lambda = 5)
max(x) == max2(x, TRUE)
microbenchmark(max(x),
               max2(x, TRUE),
               times = 100)

# For short vectors, max2 is slower than max
x <- rnorm(100)
max(x) == max2(x)
microbenchmark(max(x),
               max2(x),
               times = 100)

x <- rpois(100, lambda = 5)
max(x) == max2(x, TRUE)
microbenchmark(max(x),
               max2(x, TRUE),
               times = 100)
}
