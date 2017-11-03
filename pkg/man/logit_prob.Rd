\name{logit_prob}
\alias{logit_prob}
\title{
Convert Logit to Probability
}
\description{
Defined as: \code{exp_x <- exp(x); out <- exp_x / (1 + exp_x)}. This 2-step
approach is faster than \code{exp(x) / (1 + exp(x))} because the exponentials
only have to be calculated once.
}
\usage{
logit_prob(x)
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
Numeric vector.
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
