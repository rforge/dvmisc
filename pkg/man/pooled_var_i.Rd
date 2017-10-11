\name{pooled_var_i}
\alias{pooled_var_i}
\title{
Pooled Sample Variance for Integer Vectors
}
\description{
Calculates pooled sample variance used in equal variance two-sample t-test. Runs 
faster than \code{\link{pooled_var_n}} when \code{x} and \code{y} are integer 
vectors, but not valid if \code{x} or \code{y} are non-integer vectors.
}
\usage{
pooled_var_i(x, y)
}
\arguments{
  \item{x}{
Integer vector.
}
  \item{y}{
Integer vector.
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
NA
}
