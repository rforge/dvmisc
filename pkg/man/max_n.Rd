\name{max_n}
\alias{max_n}
\title{
Maximum of Numeric Values
}
\description{
Written in C++, this function tends to run faster than \code{\link{max}} for 
large numeric vectors/matrices. For integer objects, \code{\link{max_i}} should 
run even faster.
}
\usage{
max_n(x)
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
# For large objects, max_n is faster than max
x <- rnorm(100000)
max(x) == max_n(x)
microbenchmark(max(x),
               max_n(x),
               times = 100)

# For smaller objects, max_n is slower than max
x <- rnorm(100)
max(x) == max_n(x)
microbenchmark(max(x),
               max_n(x),
               times = 100)
}
