\name{sumsim}
\alias{sumsim}
\title{
Summarize Simulation Results
}
\description{
Creates table summarizing results of statistical simulations, providing
common metrics of performance like mean bias, standard deviation, mean standard
error, mean squared error, and confidence interval coverage.
}
\usage{
sumsim(estimates, ses = NULL,
       truth = NULL,
       statistics = c("mean_bias", "sd", "mean_se", "mse",
                      "coverage"),
       alpha = 0.05,
       digits = 3)
}
\arguments{
  \item{estimates}{
Numeric matrix where each column gives the point estimates for a particular
method across multiple trials.
}
  \item{ses}{
Numeric matrix where each column gives the standard errors for a particular
method across multiple trials.
}
  \item{truth}{
Numeric value specifying the true value of the parameter being estimated.
}
  \item{statistics}{
Numeric vector specifying which performance metrics should be calculated.
Possible values are \code{"mean"}, \code{"median"}, \code{"mean_bias"},
\code{"median_bias"}, \code{"sd"}, \code{"iqr"}, \code{"mean_se"} (for mean
standard error), \code{"mse"} (for mean squared error), and \code{"coverage"}
(for confidence interval coverage).
}
  \item{alpha}{
Numeric value specifying alpha for confidence interval. Set to \code{0.05} for
the usual 95\% CI, \code{0.1} for a 90\% CI, and so forth.
}
  \item{digits}{
Numeric value or vector specifying the number of decimal places to include.
}
}
\details{
NA
}
\value{
Character matrix.
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
