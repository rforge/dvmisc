\name{var_i}
\alias{var_i}
\title{
Sample Variance for Integer Values
}
\description{
Written in C++, this function should always run faster than \code{\link{var}} 
for integer vectors. Not valid for non-integer input vectors.
}
\usage{
var_i(x)
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
# For integer vectors, var_i is typically much faster than var.
x <- rpois(1000, lambda = 5)
all.equal(var(x), var_i(x))
microbenchmark(var(x),
               var_i(x),
               times = 100)
}