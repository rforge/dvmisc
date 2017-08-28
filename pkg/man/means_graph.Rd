\name{means.graph}
\alias{means.graph}
\title{
Graph Means Across a Grouping Variable
}
\description{
Creates plot showing mean of Y variable across levels of a grouping variable,
with customizable error bars.
}
\usage{
means.graph(y, group, error.bars = "t.ci", alpha = 0.05,
            p.legend = TRUE,
            plot.list = NULL,
            lines.list = NULL,
            axis.list = NULL,
            legend.list = NULL,
            ...)
}
\arguments{
  \item{y}{
Numeric vector of values for the continuous variable.
}
  \item{group}{
Vector of values indicating what group each \code{y} observation belongs to.
Function plots group levels across x-axis in same order as \code{table(group)}.
}
  \item{error.bars}{
Character string indicating what the error bars should represent. Possible
values are \code{"sd"} for +/- one standard deviation, \code{"se"} for +/- one
standard error, \code{"t.ci"} for 95\% confidence interval based on t
distribution, \code{"z.ci"} for 95\% confidence interval based on Z
distribution, and \code{"none"} for no error bars.
}
  \item{alpha}{
Numeric value indicating what alpha should be set to for confidence intervals.
Only used if \code{error.bars} is \code{"t.ci"} or \code{"z.ci"}.
}
  \item{p.legend}{
If \code{TRUE}, p-value (from \code{\link{t.test}} function if group has 2
levels, otherwise \code{\link{aov}} function) is printed in a legend.
}
  \item{plot.list}{
Optional list of inputs to pass to \code{\link{plot}} function.
}
  \item{lines.list}{
Optional list of inputs to pass to \code{\link{lines}} function.
}
  \item{axis.list}{
Optional list of inputs to pass to \code{\link{axis}} function.
}
  \item{legend.list}{
Optional list of inputs to pass to \code{\link{legend}} function.
}
  \item{...}{
Additional arguments to pass to \code{\link{t.test}} or \code{\link{aov}}.
}
}
\details{
Function drops observations with missing values for \code{y} and/or
\code{group}.
}
\value{
Plot showing mean of \code{y} across levels of \code{group}.
}
\references{
Acknowledgment: This material is based upon work supported by the National
Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
NA
}
\seealso{
NA
}
\examples{
NA
}
