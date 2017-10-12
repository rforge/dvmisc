\name{which_min_nm}
\alias{which_min_nm}
\title{
Return (Row, Column) Index of (First) Minimum of a Numeric Matrix
}
\description{
Written in C++, this function tends to run much faster than the equivalent (if 
minimum is unique) base R solution \code{which(x == min(x), arr.ind = TRUE)}.
}
\usage{
which_min_nm(x)
}
\arguments{
  \item{x}{
Numeric matrix.
}
}
\details{
NA
}
\value{
Integer vector.
}
\note{
For optimal speed, choose the version of this function that matches the class of 
your \code{x}:

\code{\link{which_min_nv}} for numeric vector. \cr
\code{\link{which_min_iv}} for integer vector. \cr
\code{\link{which_min_nm}} for numeric matrix. \cr
\code{\link{which_min_im}} for integer matrix.
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
# which_min_nm is typically much faster than which(x == min(x), arr.ind = TRUE)
x <- matrix(rnorm(100), ncol = 10)
all(which(x == min(x), arr.ind = TRUE) == which_min_nm(x))
microbenchmark(which(x == min(x), arr.ind = TRUE),
               which_min_nm(x),
               times = 100)
}
