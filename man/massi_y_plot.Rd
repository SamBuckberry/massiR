\name{massi_y_plot}
\alias{massi_y_plot}

\title{
massi_y_plot
}
\description{
The massi_y_plot function plots the data output from massi.y function.
}
\usage{
massi_y_plot(massi_y_out)
}

\arguments{
  \item{massi_y_out}{
This object is the list returned from \code{\link{massi_y}} function.
}
}
\details{
This function produces a bar plot of the coefficient of variation (CV) for each probe in the dataset. This allows the user to identify the most variable probes that are likely to be the most informative in the sex prediction step. The 25\%, 50\% and 75\% quantiles are represented as horizontal lines and represent the threshold values that can be specified for the \code{\link{massi_select}} function.
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
\code{\link{massi_y}, \link{massi_select}, \link{massi_cluster}, \link{massi_dip}, \link{massi_cluster_plot}}

}
\examples{

data(massi.test.dataset, massi.test.probes)

massi_y_out <-
  massi_y(expression_data=massi.test.dataset, y_probes=massi.test.probes)
  
massi_y_plot(massi_y_out)

}
