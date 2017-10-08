\name{mean2}
\alias{mean2}
\title{
Arithmetic Mean (Alternate Implementation)
}
\description{
Faster version of base R function \code{\link{mean}}.
}
\usage{
mean2(x, x.integer = FALSE)
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
If \code{x.integer = FALSE}, the function is simply \code{sum(x) / length(x)};
if \code{x.integer = TRUE}, an internal C++ function is called. For relatively
short \code{x} vectors it may be more efficient to leave
\code{x.integer = FALSE} (see \bold{Examples}).
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
# In general, mean2 is much faster than mean.
x <- rnorm(10000)
mean(x) == mean2(x)
microbenchmark(mean(x),
               mean2(x),
               times = 100)

# For very long integer vectors, mean2 may be slower than mean. But if you set
# x.integer = TRUE, mean2 should become faster.
x <- rpois(100000, lambda = 5)
mean(x) == mean2(x)
mean2(x) == mean2(x, TRUE)
microbenchmark(mean(x),
               mean2(x),
               mean2(x, TRUE),
               times = 100)
}
