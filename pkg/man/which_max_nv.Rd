\name{which_max_nv}
\alias{which_max_nv}
\title{
Return Index of (First) Maximum of a Numeric Vector
}
\description{
Written in C++, this function tends to run faster than \code{\link{which.max}}
for large numeric vectors.
}
\usage{
which_max_nv(x)
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
# For long vectors, which_max_nv is faster than which.max
x <- rnorm(100000)
which.max(x) == which_max_nv(x)
microbenchmark(which.max(x),
               which_max_nv(x),
               times = 100)
               
# For shorter vectors, which_max_nv is slower than which.max
x <- rnorm(100)
which.max(x) == which_max_nv(x)
microbenchmark(which.max(x),
               which_max_nv(x),
               times = 100)
}
