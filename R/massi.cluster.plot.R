library("gplots")

massi_cluster_plot <- function(massi_select_data, massi_cluster_data) {
  
  #start new graphics device
  dev.new()
  
  # generate a heatmap of y.chromosome.subset values
  ord <- order(rowSums(abs(massi_select_data)),decreasing=T)
  heatmap.2(x=as.matrix(massi_select_data[ord,]), keysize=2, cexRow=0.7,
            key=T, trace="none", dendrogram="row", col=redgreen(75), scale="row")
  
  massi.cluster.results <- data.frame(massi_cluster_data[[2]])
  massi.cluster.results.sort <- massi.cluster.results[order(massi.cluster.results$sex),] # sort data by sex
  probe.means <- massi.cluster.results.sort$mean_y_probes_value # samples probe mean values
  probe.sd <- massi.cluster.results.sort$y_probes_sd # sample probe sd values
  sample.names <- massi.cluster.results.sort$ID # set x-axis names
  plot.top <- ceiling(max(probe.means+probe.sd*1.1)) # set y-axis upper limit
  plot.bottom <- floor(min(probe.means-probe.sd*1.1)) # set y-axis lower limit
  sample.sex <- massi.cluster.results.sort$sex # set the factor for bar color
  # create the plot
  barCenters <- barplot(probe.means, xpd=F, names.arg=massi_cluster_data$ID, cex.names=0.7,
                        ylab="Chr.Y mean probe value +/- SD",
                        xlab="",
                        col=c("red", "green")[as.factor(sample.sex)],
                        las=2, ylim=c(plot.bottom,plot.top))
  segments(barCenters, probe.means-probe.sd, # add the sd bars
           barCenters, probe.means+probe.sd, lwd=0.8)
  legend("topleft", fill=c("red", "green"), title="predicted sex", ## add legend to plot
         legend=c("female", "male"), cex=0.5, )
  
  ## generate PC plot of clusters
  k.medoids.results <- massi_cluster_data[[1]]
  #y.kmedoids <- get(k.medoids.results)
  clusplot(t(massi_select_data), k.medoids.results$clustering, color=TRUE, shade=FALSE, main="",cex.txt=0.5,
           labels=2, lines=0)
  
}

