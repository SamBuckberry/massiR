
massi.y.plot <- function (massi.y.out) {
  
  dev.new()
  ## plot CV for each probe with quantiles
  
  barplot(height=massi.y.out[[2]], names.arg=massi.y.out[[1]], xpd=T,
          cex.names=0.5, las=2, ylab="Probe CV (%)")
  
  # Get the quantile values from the massi.y output
  quantiles <- massi.y.out[[3]]
  
  # add lines for the 0%, 25%, 50%, and 75% quartiles
  abline(h=quantiles[1:4], col=c("black", "red", "blue", "green"), lwd=2)
  legend("topleft",cex=0.7, title="Threshold (Quantile)",
         col=c("black", "red", "blue", "green"),
         fill= c("black", "red", "blue", "green"),
         legend=c("1 (0%)", "2 (25%)",
                  "3 (50%)", "4 (75%)"))
  
}

