\name{diff1_i}
\alias{diff1_i}
\title{
1-Unit Lagged Differences for Integer Values
}
\description{
Written in C++, this function should always run faster than \code{\link{diff}} 
for calculating differences between adjacent values of an integer vector.
}
\usage{
diff1_i(x)
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
Integer vector.
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
# diff1_i is typically much faster than diff.
x <- rpois(1000, lambda = 5)
all.equal(diff(x), diff1_i(x))
microbenchmark(diff(x),
               diff1_i(x),
               times = 100)
}