\name{massi_dip}
\alias{massi_dip}

\title{
massi_dip
}
\description{
The massi_dip function applies the dip test to the subset of y chromosome probe values returned from the \code{\link{massi_select}} function. This can be used to indicate if there may be either a male or female bias in the dataset. This function returns a message indicating if the dataset may have a sex bias. The results for massi_dip are not relaible for datasets with 10 or less samples. 
}
\usage{
massi_dip(y_subset_values)
} 

\arguments{
  \item{y_subset_values}{
A data.frame containing the subset of y chromosome probe values for each sample, as returned from the \code{\link{massi_select}} function.
}
}
\details{
This function caclulates z-scores for the y.chromosome probe values returned from the \code{\link{massi_select}} function and then checks if the average z-scores for each sample show a unimodal or multi-modal distribution by applying the dip test. If the proportion of male and female samples in the dataset is relatively balanced, the distribution of average z-scores should be bi-modal. If the distribution looks unimodal, the dataset likely contains a high proportion of one sex. By testing with empirical datasets and randomly generating data subsets with different male/female proportions, guideline values were developed to provide an indication if there is a potential sex bias in the dataset. If the dip statistic is > 0.08 then the dataset is highly likely to have a porportions of male and female samples that will allow the \code{\link{massi_cluster}} function to predict the sex of samples with a high degree of accuracy. The results of this test should only be used as a guide and the results should be interpreted in light of the \code{\link{massi_cluster}} results. For more details see the massi package vignette. 
}
\value{
This function returns a list containing

\item{dip.statistics}{The results from the dip test}
\item{sample.mean.z.score}{The mean of the probe z-scores for each sample used to caclulate the dip statistics}
\item{density}{Density values for the z-scores. Can be informative to plot these results}

}
\references{
  Martin Maechler (2013). diptest: Hartigan's dip test statistic for unimodality - corrected
  code. R package version 0.75-5. http://CRAN.R-project.org/package=diptest
}
\author{
Sam Buckberry
}

\seealso{
\code{\link{massi_y}, \link{massi_select}, \link{massi_cluster}, \link{massi_y_plot}, \link{massi_cluster_plot}}

}
\examples{

# load the test dataset
data(massi.test.dataset, massi.test.probes)

massi_select_out <- massi_select(expression_data=massi.test.dataset, y_probes=massi.test.probes, threshold=4)

# Use the list returned from massi.select to calculate dip statistics and z-scores.
massi_dip_out <- massi_dip(y_subset_values=massi_select_out)

# view a density plot
plot(massi_dip_out[[3]])

# view a histogram of z-scores
hist(x=massi_dip_out[[2]])

}
