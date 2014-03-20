\name{massi_y}
\alias{massi_y}

\title{
massi_y
}
\description{
The massi_y function extracts the y chromosome probe values for each sample and calculates the coefficient of variation (CV) for each probe. The returned list contains CV values (\%) for each probe and quantile data. The probe variation data can be visualized using the massi_y_plot function.
}
\usage{
massi_y(expression_data, y_probes)
}

\arguments{
  \item{expression_data}{
The expression.data item contains normalized array expression data for all samples. This can be a data.frame with sample names as columns and probe id's as row names. This argument also allows the specification of an ExpressionSet object.  
}
  \item{y_probes}{
A data.frame of probe id's in one column that match y chromosome genes for the array platform. massiR includes probes for several Illumina and Affymetrix platforms. Details on using these probes are included in the vignette and the \code{\link{y.probes}} manual.
}
}
\details{
The expression.data must be as a data.frame with sample names as column names and probe id's as row.names. ExpressionSet objects can be input and with expression data will be exracted from the ExpressionSet and the returned list would be the same as if data as entered in data.frame format.
}
\value{
The massi_y function returns a list containing probe id's, probe cv and quantiles.

\item{id }{Probe id's}
\item{cv }{Probe cv values}
\item{quantiles }{Quantiles of cv values data}
}

\author{
Sam Buckberry
}

\seealso{
\code{\link{massi_select}, \link{massi_cluster}, \link{massi_y_plot}, \link{massi_dip}, \link{massi_cluster_plot}}

}

\examples{

data(massi.test.dataset, massi.test.probes)
massi_y(massi.test.dataset, massi.test.probes)

}
