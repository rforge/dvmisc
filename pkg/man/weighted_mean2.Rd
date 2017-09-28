\name{weighted.mean2}
\alias{weighted.mean2}
\title{
Weighted Arithmetic Mean (Alternate Implementation)
}
\description{
Faster version of base R function \code{\link{weighted.mean}}.
}
\usage{
weighted.mean2(x, w, x.integer = FALSE, w.integer = FALSE)
}
\arguments{
  \item{x}{
Numeric vector of values.
}
  \item{w}{
Numeric vector of weights.
}
  \item{x.integer}{
Logical value indicating whether \code{x} takes on integer values.
}
  \item{w.integer}{
Logical value indicating whether \code{w} takes on integer values.
}
}
\details{
NA
}
\value{
Numeric value.
}
\note{
This function typically executes several times faster than the base R function
\code{\link{weighted.mean}} and weighted average functions in other packages
(e.g. \code{wtd.mean} in \bold{Hmisc} and \code{wt.mean} in \bold{SDMTools}).

When \code{x} or \code{w} are integer vectors, additional speed gains can be had
by specifying \code{x.integer = TRUE} and/or \code{w.integer = TRUE}.
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
# weighted.mean2 is typically much faster than weighted.mean
x <- rnorm(1000)
weights <- runif(1000)
abs(weighted.mean(x, weights) - weighted.mean2(x, weights))
microbenchmark(weighted.mean(x, weights),
               weighted.mean2(x, weights),
               times = 1000)

# If x is an integer vector, setting x.integer = TRUE will make it even faster
x <- rpois(1000, lambda = 5)
weights <- runif(1000)
microbenchmark(weighted.mean(x, weights),
               weighted.mean2(x, weights),
               weighted.mean2(x, weights, x.integer = TRUE),
               times = 1000)
}
