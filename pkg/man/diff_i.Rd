\name{diff_i}
\alias{diff_i}
\title{
Lagged Differences for Integer Values
}
\description{
Written in C++, this function should always run faster than \code{\link{diff}} 
for calculating lagged differences for an integer vector.
}
\usage{
diff_i(x, lag = 1)
}
\arguments{
  \item{x}{
Integer vector.
}
  \item{lag}{
Integer value.
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
# diff_i is typically much faster than diff.
x <- rpois(1000, lambda = 5)
all.equal(diff(x, 2), diff_i(x, 2))
microbenchmark(diff(x, 2),
               diff_i(x, 2),
               times = 100)
}