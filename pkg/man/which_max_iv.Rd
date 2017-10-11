\name{which_max_iv}
\alias{which_max_iv}
\title{
Return Index of (First) Maximum of an Integer Vector
}
\description{
Written in C++, this function tends to run faster than \code{\link{which.max}}
for large integer vectors.
}
\usage{
which_max_iv(x)
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

\code{\link{which_max_nv}} for numeric vector. \cr
\code{\link{which_max_iv}} for integer vector. \cr
\code{\link{which_max_nm}} for numeric matrix. \cr
\code{\link{which_max_im}} for integer matrix.
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
# For long vectors, which_max_iv is faster than which.max
x <- rpois(10000, lambda = 15)
which.max(x) == which_max_iv(x)
microbenchmark(which.max(x),
               which_max_iv(x),
               times = 100)
               
# For shorter vectors, which_max_iv is slower than which.max
x <- rpois(100, lambda = 15)
which.max(x) == which_max_iv(x)
microbenchmark(which.max(x),
               which_max_iv(x),
               times = 100)
}