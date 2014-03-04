\name{massi-package}
\alias{massi-package}
\alias{massi}
\docType{package}
\title{
massiR: a microarry Gene Expression Sample Sex Identifier
}
\description{
massi uses y chromosome probe information to cluster samples and predict the sex of each sample in gene expression microarray datasets.
}
\details{
\tabular{ll}{
Package: \tab massi\cr
Type: \tab Package\cr
Version: \tab 0.99.0\cr
Date: \tab 2014-01-27\cr
License: \tab GPL-3\cr
}
The massi analysis requires a typical normalized sample/probe values produced by a microarray experiment. The \code{\link{massi_y}} function will extract the y chromosome probe information and caculate y chromosome probe variance to allow the used to select the most informative probes. Using the \code{\link{massi_select}} function the used can select a probe variation threshold to reduce the number of probes used in the massi.cluster step. The \code{\link{massi_cluster}} function clusters samples into two clusters using the y chromosome probe values. Clustering is performed using the K-medoids method as implimented in the "fpc" package. There are two plotting fucntions, \code{\link{massi_y_plot}} and \code{\link{massi_cluster_plot}}, that allow the user to explore the data at various stages of the analysis. There is also a function, \code{\link{massi_dip}}, that can be used to test if there may be a sample sex-bias in the dataset.

}
\author{
Sam Buckberry

Maintainer: Sam Buckberry <sam.buckberry@adelaide.edu.au>

}
\references{
  Christian Hennig (2013). fpc: Flexible procedures for clustering. R package version 2.1-6.
  http://CRAN.R-project.org/package=fpc
  
  Martin Maechler (2013). diptest: Hartigan's dip test statistic for unimodality - corrected
  code. R package version 0.75-5. http://CRAN.R-project.org/package=diptest
  
  Gregory R. Warnes, Ben Bolker, Lodewijk Bonebakker, Robert Gentleman, Wolfgang Huber Andy Liaw, Thomas Lumley, Martin
  Maechler, Arni Magnusson, Steffen Moeller, Marc Schwartz and Bill Venables (2013). gplots: Various R programming tools for
  plotting data. R package version 2.12.1. http://CRAN.R-project.org/package=gplots
  
}

\seealso{

\code{\link{massi_y}, \link{massi_select}, \link{massi_cluster}, \link{massi_y_plot}, \link{massi_dip}, \link{massi_cluster_plot}}

}
\examples{

# load the test datasets
data(massi.test.dataset, massi.test.probes)

# use the massi.y function to calculate probe variation
massi_y_out <- massi_y(expression_data=massi.test.dataset, y_probes=massi.test.probes)

# plot probe variation to aid in deciding on the most informative subset of y chromosome probes
massi_y_plot(massi_y_out)

# Extract the informative probes for clustering 
massi_select_out <- massi_select(massi.test.dataset, massi.test.probes, threshold=4)

# cluster samples to predict the sex for each sample
massi_cluster_out <- massi_cluster(massi_select_out)

# get the predicted sex for each sample
data.frame(massi_cluster_out[[2]])

}

