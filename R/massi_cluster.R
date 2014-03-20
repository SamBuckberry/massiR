library("fpc")

massi_cluster <- function(y_data){
  
  # import the data from massi.select
  y_data.subset <- y_data

  y_data.subset.t <- as.data.frame(t(y_data.subset)) #transpose the probe subset values matrix

  y.kmedoids <- pam(x=y_data.subset.t, k=2) ### perform clustering of pam.k clusters

  sample.sex <- data.frame(y.kmedoids$clustering) ## get samples and clusters into data.frame
  
  ## now the samples are classified, the group with the highest mean is assigned as male
  sample.means <- data.frame(rowMeans(y_data.subset.t)) # calculate the mean of probe values for each sample
  sample.sex <- merge(sample.sex, sample.means, by="row.names") # merge the clusters and mean expression by sample ID
  sample.sex$ID <- sample.sex$Row.names
  sample.sex$Row.names <- NULL
  
  ## calculate the mean for each cluster of the NRY probe mean
  cluster.1 <- subset(sample.sex, subset=sample.sex$y.kmedoids.clustering==1) #subset cluster 1
  cluster.1.mean <- mean(cluster.1$rowMeans.y_data.subset.t.) # calculate cluster 1 mean
  
  cluster.2 <- subset(sample.sex, subset=sample.sex$y.kmedoids.clustering==2) # subset cluster 2
  cluster.2.mean <- mean(cluster.2$rowMeans.y_data.subset.t.) # calculate cluster 2 mean
  
  # assign cluster with the highest mean as male, and lowest mean as female
  c1.sex <- ifelse(cluster.1.mean>cluster.2.mean, yes=as.character("male"), no=as.character("female"))
  c2.sex <- ifelse(cluster.1.mean<cluster.2.mean, yes=as.character("male"), no=as.character("female"))  
  
  # create a column for sex and substitute the cluster id for "Male" or "Female"
  sample.sex$kmedoids.sex <- as.character(sample.sex$y.kmedoids.clustering) # create the column for sex
  sample.sex$kmedoids.sex[sample.sex$kmedoids.sex == "1"] <- c1.sex
  sample.sex$kmedoids.sex[sample.sex$kmedoids.sex == "2"] <- c2.sex
  
  sample.sd <- data.frame(sapply(y_data.subset, FUN=sd)) ## add sample sd to output
  
  sample.sd$ID <- row.names(sample.sd)
  
  massi.results <- data.frame(sample.sex$ID) # Add sample ID
  massi.results$ID <- massi.results$sample.sex.ID
  massi.results$mean.probe.value <- sample.sex$rowMeans.y_data.subset.t. # add mean probe values
  massi.results<- merge(massi.results, sample.sd, by="ID") # add sample/probe sd
  massi.results$sample.sd <- massi.results$sapply.y_data.subset..1...FUN...sd.
  massi.results$sapply.y_data.subset..1...FUN...sd. <- NULL
  
  # add sample average z-score
  z.score <- scale(t(y_data.subset))
  z.score.mean <- data.frame(rowMeans(z.score))
  z.score.mean$ID <- row.names(z.score.mean)
  massi.results <- merge(massi.results, z.score.mean, by="ID")
  
  massi.results <- merge(massi.results, sample.sex, by="ID") # add clustering results
  massi.results$sample.sex.ID <- NULL # remove redundant fields
  massi.results$y.kmedoids.clustering <- NULL
  massi.results$rowMeans.y_data.subset.t. <- NULL
  
  colnames(massi.results) <- c("ID", "mean_y_probes_value", "y_probes_sd", "z_score", "sex")
  return(list("cluster.data" = y.kmedoids, "massi.results" = massi.results))
}

