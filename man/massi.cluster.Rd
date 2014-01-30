\name{massi.cluster}
\alias{massi.cluster}

\title{
massi.cluster
}
\description{
The massi.cluster function predicts the sex of samples using k-medoids clustering.
}
\usage{
massi.cluster(y.data)
}

\arguments{
  \item{y.data}{
the y.data object is the data.frame returned from the \code{\link{massi.select}} function. This is a data.frame with sample names as column names and probe id's as row.names. 
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
\code{\link{massi.y}, \link{massi.select}, \link{massi.y.plot}, \link{massi.dip}, \link{massi.cluster.plot}}
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

# get the results in a data.frame format
data.frame(massi.cluster.out[[2]])
}
}
