\name{which.min2}
\alias{which.min2}
\title{
Version of which.min Compatible with Vector or Matrix Input
}
\description{
Similar to base function which.min, but with two extra features: (1) Can request position of first minimum or position of all minimums; (2) For matrix x, can request either vector position or row/column.
}
\usage{
which.min2(x, only.first = FALSE, arr.ind = TRUE)
}
\arguments{
  \item{x}{
Numeric vector or matrix.
}
  \item{only.first}{
If TRUE, returns position of only the first minimum (like base R which.min); if FALSE, returns position of all minimums.
}
  \item{arr.ind}{
Only used if x is a matrix. If TRUE, returns row/column index of minimum; if FALSE, returns vector position of minimum (like base R which.min).
}
}
\details{
NA
}
\value{
If x is a vector or x is a matrix and arr.ind is TRUE, a numeric value or vector giving the vector position of the minimum(s). If x is a matrix and arr.ind is TRUE, a numeric matrix giving the row and column positions of the minimum(s). 
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
# Generate 4-by-4 matrix from Poisson(2)
set.seed(123)
(x <- matrix(rpois(n = 16, lambda = 2), ncol = 4))

# Get row/column indices for all minimums
which.min2(x)

# Get vector index for first minimum
which.min2(x, only.first = TRUE, arr.ind = FALSE)
}