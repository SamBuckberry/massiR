\name{massi_cluster}
\alias{massi_cluster}

\title{
massi_cluster
}
\description{
The massi_cluster function predicts the sex of samples using k-medoids clustering.
}
\usage{
massi_cluster(y_data)
}

\arguments{
  \item{y_data}{
the y_data object is the data.frame returned from the \code{\link{massi_select}} function. This is a data.frame with sample names as column names and probe id's as row.names. 
}
}
\details{
This function clusters samples into two clusters using y chromosome probe values. K-medoids clustering is performed using the partitioning around medoids (pam) method implimented in the "fpc" package. The cluster with the highest probe values is determined to be the cluster of male samples and the cluster the lowest values as female samples. 
}
\value{
\item{cluster data}{Contains all of the results from the k-medoids clustering.}
\item{massi.results}{Contains the results for each sample, including sample id, predicted sex, sample z-score and mean probe expression.}

}
\references{
  Christian Hennig (2013). fpc: Flexible procedures for clustering. R package version 2.1-6.
  http://CRAN.R-project.org/package=fpc
}
\author{
Sam Buckberry
}
\seealso{
\code{\link{massi_y}, \link{massi_select}, \link{massi_y_plot}, \link{massi_dip}, \link{massi_cluster_plot}}
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

# get the results in a data.frame format
data.frame(massi_cluster_out[[2]])

}
