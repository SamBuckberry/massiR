\name{massi.y.plot}
\alias{massi.y.plot}

\title{
massi.y.plot
}
\description{
The massi.y.plot function plots the data output from massi.y function.
}
\usage{
massi.y.plot(massi.y.out)
}

\arguments{
  \item{massi.y.out}{
This object is the list returned from \code{\link{massi.y}} function.
}
}
\details{
This function produces a bar plot of the coefficient of variation (CV) for each probe in the dataset. This allows the user to identify the most variable probes that are likely to be the most informative in the sex prediction step. The 25\%, 50\% and 75\% quantiles are represented as horizontal lines and represent the threshold values that can be specified for the \code{\link{massi.select}} function.
}
\value{
This function produces a bar plot in a new graphics device. 
}

\author{
Sam Buckberry
}
\note{
See vignette for more details.
}

\seealso{
\code{\link{massi.y}, \link{massi.select}, \link{massi.cluster}, \link{massi.dip}, \link{massi.cluster.plot}}

}
\examples{
\dontrun{
data(massi.test.dataset, MASS.test.probes)

massi.y.out <-
  massi.y(expression.data=massi.test.dataset, y.probes=massi.test.probes)
  
massi.y.plot(massi.y.out)
}
}
