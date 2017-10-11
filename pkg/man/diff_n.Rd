\name{diff_n}
\alias{diff_n}
\title{
Lagged Differences for Numeric Values
}
\description{
Written in C++, this function should always run faster than \code{\link{diff}} 
for calculating lagged differences for a numeric vector. For integer vectors, 
\code{\link{diff_i}} should run even faster. 
}
\usage{
diff_n(x, lag = 1)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{lag}{
Integer value.
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
# In general, diff_n is much faster than diff.
x <- rnorm(1000)
all.equal(diff(x, 2), diff_n(x, 2))
microbenchmark(diff(x, 2),
               diff_n(x, 2),
               times = 100)
               
# For integer vectors, diff_i should be even faster.
x <- rpois(1000, lambda = 5)
all.equal(diff(x, 2), diff_i(x, 2))
microbenchmark(diff(x, 2),
               diff_n(x, 2),
               diff_i(x, 2),
               times = 100)
}