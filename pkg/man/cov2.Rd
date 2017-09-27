\name{cov2}
\alias{cov2}
\title{
Sample Covariance (Alternate Implementation)
}
\description{
Faster version of base R function \code{\link{cov}}.
}
\usage{
cov2(x, y, xy.integer = FALSE)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{y}{
Numeric vector.
}
  \item{xy.integer}{
Logical value indicating whether \code{x} and \code{y} take on integer values.
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
# In general, cov2 is much faster than cov
x <- rnorm(1000)
y <- rnorm(1000)
abs(cov(x, y) - cov2(x, y))
microbenchmark(cov(x, y),
               cov2(x, y),
               times = 1000)

# If x and y are integer vectors, setting xy.integer = TRUE will make it even
# faster
x <- rpois(1000, lambda = 5)
y <- rpois(1000, lambda = 5)
microbenchmark(cov(x, y),
               cov2(x, y),
               cov2(x, y, TRUE),
               times = 1000)
}
