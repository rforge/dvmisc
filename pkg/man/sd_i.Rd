\name{sd_i}
\alias{sd_i}
\title{
Sample Standard Deviation for Integer Values
}
\description{
Written in C++, this function should always run faster than \code{\link{sd}} 
for integer vectors. Not valid for non-integer vectors.
}
\usage{
sd_i(x)
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
all.equal(sd(x), sd_i(x))
microbenchmark(sd(x),
               sd_i(x),
               times = 100)
}
