\name{cov_i}
\alias{cov_i}
\title{
Sample Covariance for Integer Vectors
}
\description{
Written in C++, this function should always run faster than \code{\link{cov}} 
for integer vectors. Not valid for non-integer vectors.
}
\usage{
cov_i(x, y)
}
\arguments{
  \item{x}{
Integer vector.
}
  \item{y}{
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
# For integer vectors, cov_i is typically much faster than cov.
x <- rpois(1000, lambda = 5)
y <- rpois(1000, lambda = 5)
all.equal(cov(x, y), cov_i(x, y))
microbenchmark(cov(x, y),
               cov_i(x, y),
               times = 100)
}
