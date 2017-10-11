\name{diff1_n}
\alias{diff1_n}
\title{
1-Unit Lagged Differences for Numeric Values
}
\description{
Written in C++, this function should always run faster than \code{\link{diff}} 
for calculating differences between adjacent values of a numeric vector. For 
integer vectors, \code{\link{diff1_i}} should run even faster. 
}
\usage{
diff1_n(x)
}
\arguments{
  \item{x}{
Numeric vector.
}
}
\details{
NA
}
\value{
Numeric vector.
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
# In general, diff1_n is much faster than diff.
x <- rnorm(1000)
all.equal(diff(x), diff1_n(x))
microbenchmark(diff(x),
               diff1_n(x),
               times = 100)
               
# For integer vectors, diff1_i should be even faster.
x <- rpois(1000, lambda = 5)
all.equal(diff(x), diff1_i(x))
microbenchmark(diff(x),
               diff1_n(x),
               diff1_i(x),
               times = 100)
}