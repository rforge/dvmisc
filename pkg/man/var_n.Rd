\name{var_n}
\alias{var_n}
\title{
Sample Variance for Numeric Values
}
\description{
Written in C++, this function should always run faster than \code{\link{var}} 
for numeric vectors. For integer vectors, \code{\link{var_i}} should run even 
faster.
}
\usage{
var_n(x)
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
# In general, var_n is much faster than var.
x <- rnorm(1000)
all.equal(var(x), var_n(x))
microbenchmark(var(x),
               var_n(x),
               times = 100)
               
# For integer vectors, var_i should be even faster.
x <- rpois(1000, lambda = 5)
all.equal(var(x), var_i(x))
microbenchmark(var(x),
               var_n(x),
               var_i(x),
               times = 100)
}
