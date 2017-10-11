\name{sd_n}
\alias{sd_n}
\title{
Sample Standard Deviation for Numeric Values
}
\description{
Written in C++, this function should always run faster than \code{\link{sd}} 
for numeric vectors. For integer vectors, \code{\link{sd_i}} should run even 
faster.
}
\usage{
sd_n(x)
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
# In general, sd_n is much faster than sd.
x <- rnorm(1000)
all.equal(sd(x), sd_n(x))
microbenchmark(sd(x),
               sd_n(x),
               times = 100)

# For integer vectors, sd_i should be even faster.
x <- rpois(1000, lambda = 5)
all.equal(sd(x), sd_i(x))
microbenchmark(sd(x),
               sd_n(x),
               sd_i(x),
               times = 100)
}
