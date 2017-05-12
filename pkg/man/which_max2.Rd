\name{which.max2}
\alias{which.max2}
\title{
Version of which.max Compatible with Vector or Matrix Input
}
\description{
Similar to base function which.max, but with two extra features: (1) Can request position of first maximum or position of all maximums; (2) For matrix x, can request either vector position or row/column.
}
\usage{
which.max2(x, only.first = FALSE, arr.ind = TRUE)
}
\arguments{
  \item{x}{
Numeric vector or matrix.
}
  \item{only.first}{
If TRUE, returns position of only the first maximum (like base R which.max); if FALSE, returns position of all maximums.
}
  \item{arr.ind}{
Only used if x is a matrix. If TRUE, returns row/column index of maximum; if FALSE, returns vector position of maximum (like base R which.max).
}
}
\details{
NA
}
\value{
If x is a vector or x is a matrix and arr.ind is TRUE, a numeric value or vector giving the vector position of the maximum(s). If x is a matrix and arr.ind is TRUE, a numeric matrix giving the row and column positions of the maximum(s). 
}
\note{
NA
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
NA
}
\examples{
# Generate 4-by-4 matrix from Poisson(1)
set.seed(123)
(x <- matrix(rpois(n = 16, lambda = 1), ncol = 4))

# Get row/column indices for all maximums
which.max2(x)

# Get vector index for first maximum
which.max2(x, only.first = T, arr.ind = F)
}