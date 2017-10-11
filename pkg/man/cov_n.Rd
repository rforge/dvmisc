\name{cov_n}
\alias{cov_n}
\title{
Sample Covariance for Numeric Vectors
}
\description{
Written in C++, this function should always run faster than \code{\link{cov}} 
for numeric vectors. For integer vectors, \code{\link{cov_i}} should run even 
faster.
}
\usage{
cov_n(x, y)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{y}{
Numeric vector.
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
# In general, cov_n is much faster than cov
x <- rnorm(1000)
y <- rnorm(1000)
all.equal(cov(x, y), cov_n(x, y))
microbenchmark(cov(x, y),
               cov_n(x, y),
               times = 100)

# For integer vectors, cov_i should be even faster.
x <- rpois(1000, lambda = 5)
y <- rpois(1000, lambda = 5)
all.equal(cov(x, y), cov_i(x, y))
microbenchmark(cov(x, y),
               cov_n(x, y),
               cov_i(x, y),
               times = 100)
}
