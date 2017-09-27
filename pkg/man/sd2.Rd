\name{sd2}
\alias{sd2}
\title{
Sample Standard Deviation for a Vector (Alternate Implementation)
}
\description{
Faster version of base R function \code{\link{sd}}.
}
\usage{
sd2(x, x.integer = FALSE)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{x.integer}{
Logical value indicating whether \code{x} takes on integer values.
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
# In general, sd2 is much faster than sd.
x <- rnorm(1000)
abs(sd(x) - sd2(x))
microbenchmark(sd(x),
               sd2(x),
               times = 10000)

# If x is an integer vector, setting x.integer = TRUE will make it even faster
x <- rpois(1000, lambda = 5)
microbenchmark(sd(x),
               sd2(x),
               sd2(x, TRUE),
               times = 10000)
}
