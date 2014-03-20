library("diptest")

massi_dip <- function(y_subset_values) {
  
  # Print arning for datasets with less than 10 samples
  if(as.numeric(ncol(x=y_subset_values)) <10) warning("massi_dip is not reccomended for datasets with <10 samples. Interpret results with caution.")
  
  # Calculate z-scores for each sample
  z.scores <- scale(t(y_subset_values))
  sample.mean.z.score <- rowMeans(z.scores)  

  # calculate dip statistics
  dip <- dip.test(x=sample.mean.z.score)
  dip.statistic <- as.numeric(dip["statistic"])
  
  # calculate density for plotting
  z.score.density <- density(x=sample.mean.z.score)
  
  # create output and print message
  
  if(dip.statistic > 0.08) message("dip test statistic is >0.08. This suggests that the proportion of male and female samples in this dataset is relatively balanced.")
  if(dip.statistic <= 0.08) message("dip test statistic is <=0.08. This suggests that there may be a sample sex bias in this dataset. Results from massi.cluster should be check for a sex-bias and interpreted with caution.")
  
  return(list("dip.statistics" = dip, "sample.mean.z.score" = sample.mean.z.score, "density" = z.score.density))
}



