\name{massi.dip}
\alias{massi.dip}

\title{
massi.dip
}
\description{
The massi.dip function applies the dip test to the subset of y chromosome probe values returned from the \code{\link{massi.select}} function. This can be used to indicate if there may be either a male or female bias in the dataset. This function returns a message indicating if the dataset may have a sex bias. The results for massi.dip are not relaible for datasets with 10 or less samples. 
}
\usage{
massi.dip(y.subset.values)
} 

\arguments{
  \item{y.subset.values}{
A data.frame containing the subset of y chromosome probe values for each sample, as returned from the \code{\link{massi.select}} function.
}
}
\details{
This function caclulates z-scores for the y.chromosome probe values returned from the \code{\link{massi.select}} function and then checks if the average z-scores for each sample show a unimodal or multi-modal distribution by applying the dip test. If the proportion of male and female samples in the dataset is relatively balanced, the distribution of average z-scores should be bi-modal. If the distribution looks unimodal, the dataset likely contains a high proportion of one sex. By testing with empirical datasets and randomly generating data subsets with different male/female proportions, guideline values were developed to provide an indication if there is a potential sex bias in the dataset. If the dip statistic is > 0.08 then the dataset is highly likely to have a porportions of male and female samples that will allow the \code{\link{massi.cluster}} function to predict the sex of samples with a high degree of accuracy. The results of this test should only be used as a guide and the results should be interpreted in light of the \code{\link{massi.cluster}} results. For more details see the massi package vignette. 
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
\code{\link{massi.y}, \link{massi.select}, \link{massi.cluster}, \link{massi.y.plot}, \link{massi.cluster.plot}}

}
\examples{
\dontrun{
massi.select.out <- massi.select(expression.data=massi.test.dataset, y.probes=massi.test.probes, threshold=4)


# Use the list returned from massi.select to calculate dip statistics and z-scores.
massi.dip.out <- massi.dip(y.subset.values=massi.select.out)

# view a density plot
plot(massi.dip.out[[3]])

# view a histogram of z-scores
hist(x=massi.dip.out[[2]])
}
}
