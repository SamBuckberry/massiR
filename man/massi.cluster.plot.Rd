\name{massi.cluster.plot}
\alias{massi.cluster.plot}

\title{
massi.cluster.plot
}
\description{
This function produces three figures in a new graphics device to enable the exploration of the massi.cluster and massi.select results.
}
\usage{
massi.cluster.plot(massi.select.data, massi.cluster.data)
}

\arguments{
  \item{massi.select.data}{
A data.frame containing the subset of y chromosome probe values for each sample. This is returned when running the massi.select function.
}
  \item{massi.cluster.data}{
This is the list returned from the massi.cluster function.
}
}
\details{
The first figure is a heatmap depicting probe values for each sample. The second figure is a bar plot showing the mean probe expression and standard deviation for each sample. The bars are colored with respect to the predicted sex. The third figure is a principal component plot which represents the distances bewteen samples, with each cluster highlighted with elipses.
}
\value{
Returns three plots in a new graphics device.
}
\references{
  Gregory R. Warnes, Ben Bolker, Lodewijk Bonebakker, Robert Gentleman, Wolfgang Huber Andy Liaw, Thomas Lumley, Martin
  Maechler, Arni Magnusson, Steffen Moeller, Marc Schwartz and Bill Venables (2013). gplots: Various R programming tools for
  plotting data. R package version 2.12.1. http://CRAN.R-project.org/package=gplots
  
}
\author{
Sam Buckberry
}

\seealso{
\code{\link{massi.cluster}, \link{massi.select}}
}
\examples{
\dontrun{
# load the test dataset
data(massi.test.dataset, massi.test.probes)

# select the y chromosome probes using massi.select
massi.select.out <- 
massi.select(massi.test.dataset, massi.test.probes)

# cluster samples to predict sex using massi.cluster
massi.cluster.out <- 
massi.cluster(massi.select.out)

# produce plots using massi.cluster.plot
massi.cluster.plot(massi.select.out, massi.cluster.out)
}
}

