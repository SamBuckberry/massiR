\name{massi_cluster_plot}
\alias{massi_cluster_plot}

\title{
massi_cluster_plot
}
\description{
This function produces three figures in a new graphics device to enable the exploration of the massi_cluster and massi_select results.
}
\usage{
massi_cluster_plot(massi_select_data, massi_cluster_data)
}

\arguments{
  \item{massi_select_data}{
A data.frame containing the subset of y chromosome probe values for each sample. This is returned when running the massi_select function.
}
  \item{massi_cluster_data}{
This is the list returned from the massi_cluster function.
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
\code{\link{massi_cluster}, \link{massi_select}}
}
\examples{

# load the test dataset
data(massi.test.dataset, massi.test.probes)

# select the y chromosome probes using massi_select
massi_select_out <- 
massi_select(massi.test.dataset, massi.test.probes)

# cluster samples to predict sex using massi_cluster
massi_cluster_out <- 
massi_cluster(massi_select_out)

# produce plots using massi_cluster_plot
massi_cluster_plot(massi_select_out, massi_cluster_out)

}

