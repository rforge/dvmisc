\name{min_n}
\alias{min_n}
\title{
Minimum of Numeric Values
}
\description{
Written in C++, this function tends to run faster than \code{\link{min}} for 
large numeric vectors/matrices. For integer objects, \code{\link{min_i}} should 
run even faster.
}
\usage{
min_n(x)
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
# For large objects, min_n is faster than min
x <- rnorm(100000)
min(x) == min_n(x)
microbenchmark(min(x),
               min_n(x),
               times = 100)

# For smaller objects, min_n is slower than min
x <- rnorm(100)
min(x) == min_n(x)
microbenchmark(min(x),
               min_n(x),
               times = 100)
}