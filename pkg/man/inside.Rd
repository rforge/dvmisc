\name{inside}
\alias{inside}
\title{
Check Whether a Numeric Value is in Between Two Other Numeric Values
}
\description{
If inclusive is TRUE, code is: x >= ends[1] & x <= ends[2]. If inclusive is FALSE, code is: x > ends[1] & x < ends[2]. 
}
\usage{
inside(x, ends, inclusive = TRUE)
}
\arguments{
  \item{x}{
Numeric value.
}
  \item{ends}{
Numeric vector of length 2 indicating the endpoints for the interval of interest.
}
}
\details{
NA
}
\value{
Logical value indicating whether x falls inside ends[1] and ends[2].
}
\note{
NA
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
NA
}
\examples{
# Check whether 1 is inside [0, 0.5] and [0, 1.5]
inside(1, c(0, 0.5))
inside(1, c(0, 1.5))

# Check whether 3.2 is inside [3.2, 3.6] and (3.2, 3.6)
inside(3.2, c(3.2, 3.6))
inside(3.2, c(3.2, 3.6), inclusive = FALSE)
}