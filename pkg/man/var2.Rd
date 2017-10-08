\name{var2}
\alias{var2}
\title{
Sample Variance for a Vector (Alternate Implementation)
}
\description{
Faster version of base R function \code{\link{var}}.
}
\usage{
var2(x, x.integer = FALSE)
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
# In general, var2 is much faster than var
x <- rnorm(1000)
abs(var(x) - var2(x))
microbenchmark(var(x),
               var2(x),
               times = 100)

# If x is an integer vector, setting x.integer = TRUE will make it even faster
x <- rpois(1000, lambda = 5)
microbenchmark(var(x),
               var2(x),
               var2(x, TRUE),
               times = 100)
}
