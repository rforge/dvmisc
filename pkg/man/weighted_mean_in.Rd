\name{weighted_mean_in}
\alias{weighted_mean_in}
\title{
Weighted Arithmetic Mean for Integer Values and Numeric Weights
}
\description{
Written in C++, this function should always run faster than 
\code{\link{weighted.mean}}.
}
\usage{
weighted_mean_in(x, w)
}
\arguments{
  \item{x}{
Integer vector of values.
}
  \item{w}{
Numeric vector of weights.
}
}
\details{
NA
}
\value{
Numeric value.
}
\note{
For optimal speed, choose the version of this function that matches the class of 
your \code{x} and \code{w}:

\code{\link{weighted_mean_nn}} for numeric \code{x}, numeric \code{w} \cr
\code{\link{weighted_mean_ni}} for numeric \code{x}, integer \code{w} \cr
\code{\link{weighted_mean_in}} for integer \code{x}, numeric \code{w} \cr
\code{\link{weighted_mean_ii}} for integer \code{x}, integer \code{w} \cr

These functions typically execute several times faster than the base R function
\code{\link{weighted.mean}} and weighted average functions in other packages
(e.g. \code{wtd.mean} in \bold{Hmisc} and \code{wt.mean} in \bold{SDMTools}).
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
# weighted_mean_in is typically much faster than weighted.mean
x <- rpois(1000, lambda = 5)
w <- runif(1000)
all.equal(weighted.mean(x, w), weighted_mean_in(x, w))
microbenchmark(weighted.mean(x, w),
               weighted_mean_in(x, w),
               times = 100)
}
