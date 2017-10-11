\name{which_min_iv}
\alias{which_min_iv}
\title{
Return Index of (First) Minimum of an Integer Vector
}
\description{
Written in C++, this function tends to run faster than \code{\link{which.min}}
for large integer vectors.
}
\usage{
which_min_iv(x)
}
\arguments{
  \item{x}{
Integer vector.
}
}
\details{
NA
}
\value{
Integer value.
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
# For long vectors, which_min_iv is faster than which.min
x <- rpois(10000, lambda = 15)
which.min(x) == which_min_iv(x)
microbenchmark(which.min(x),
               which_min_iv(x),
               times = 100)
               
# For shorter vectors, which_min_iv is slower than which.min
x <- rpois(100, lambda = 15)
which.min(x) == which_min_iv(x)
microbenchmark(which.min(x),
               which_min_iv(x),
               times = 100)
}