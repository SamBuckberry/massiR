\name{y.probes}
\alias{y.probes}
\docType{data}
\title{
Y chromosome probe list
}
\description{
A list containing probes corresponding to y chromosome genes for Illumina and Affymetrix platforms. Each item in the list is a data.frame of y chromosome probes that can be used in the massi analysis. The names of each item in the list correspond to the ensembl biomart attribute names.
}
\usage{data(y.probes)}
\format{
  The format is:
List of 6
 $ illumina_humanwg_6_v1:'data.frame':	58 obs. of  0 variables
 $ illumina_humanwg_6_v2:'data.frame':	74 obs. of  0 variables
 $ illumina_humanwg_6_v1:'data.frame':	112 obs. of  0 variables
 $ illumina_humanht_12  :'data.frame':	112 obs. of  0 variables
 $ affy_hugene_1_0_st_v1:'data.frame':	138 obs. of  0 variables
 $ affy_hg_u133_plus_2  :'data.frame':	94 obs. of  0 variables
}
\details{
The y chromosome probes for each platform were downloaded from Ensembl biomart using the 'biomaRt' package. For more details on the methods of selecting the probes and how to obtain probes for other platform, see the vignette for the massiR package.
}

\references{
  Mapping identifiers for the integration of genomic datasets with the R/Bioconductor
  package biomaRt. Steffen Durinck, Paul T. Spellman, Ewan Birney and Wolfgang Huber,
  Nature Protocols 4, 1184-1191 (2009).

  BioMart and Bioconductor: a powerful link between biological databases and microarray
  data analysis. Steffen Durinck, Yves Moreau, Arek Kasprzyk, Sean Davis, Bart De Moor,
  Alvis Brazma and Wolfgang Huber, Bioinformatics 21, 3439-3440 (2005).
}
\examples{

# load the probes list
data(y.probes)
# look at the platform names
names(y.probes)
# extract the probes using the platform name
probe.list <- y.probes[["illumina_humanwg_6_v2"]]

}
\keyword{datasets}
