library("Biobase")

massi_select <- function(expression_data, y_probes, threshold=3) {
  
  #Check that the input data is in the correct data.frame or ExpressionSet class
  class.expression_data <- class(expression_data)
  class.expression_data <- class.expression_data[1]
  is.ExpressionSet <- ifelse(class.expression_data == "ExpressionSet", yes=TRUE, no=FALSE)
  #if(is.ExpressionSet == TRUE) {print("OK: Input expression data as ExpressionSet")}
  is.data.frame <- ifelse(class.expression_data == "data.frame", yes=TRUE, no=FALSE)
  #if(is.data.frame == TRUE) {print("OK: Input expression data as data.frame")}
  
  # test if input data is in accpeted formats, and stop if not.
  if((is.ExpressionSet == FALSE) & (is.data.frame == FALSE)) {
    stop("Input data must be as data.frame or ExpressionSet class")
  }
  
  # Check class of y_probes  
  class.y_probes <- class(y_probes)
  if(class.y_probes != "data.frame") stop("Input y_probes data must be in data.frame class")
  
  # If input is in ExpressionSet class, convert to data.frame
  if(class.expression_data == "ExpressionSet") {
    expression_data <- data.frame(exprs(expression_data))
    expression_data$ID <- rownames(expression_data) # set probe as ID
  }
  
  if(class.expression_data == "data.frame") {
    expression_data$ID <- rownames(expression_data) # set probe as ID
  }
  
  ## Check if threshold is integer between 1:4
  if (!isTRUE(threshold == floor(threshold))) stop("Threshold must be an integer 1,2,3 or 4")
  if (!isTRUE(threshold >= 1)) stop("Threshold must be an integer 1,2,3 or 4")
  if (!isTRUE(threshold <= 4)) stop("Threshold must be an integer 1,2,3 or 4")
  
  # set probe as ID for y_probes
  y_probes$ID <- row.names(y_probes) 
  
  # extract matched probes from expression matrix using ID
  y.values <- as.data.frame(merge(expression_data, y_probes, by="ID")) 
  
  # count number of probes with match in dataset
  # This value is to be used for future features not yet implemented
  n.matched.probes <- as.numeric(nrow(y.values))
  
  # define function to calculate CV
  cal.cv <- function(x) ( 100*sd(x)/mean(x) )
  
  # calculate CV for each probe
  y.values$CV <- apply(y.values[, -which(names(y.values) == "ID")], MARGIN=1, FUN=cal.cv)
  
  # calculate quantiles for probe CV
  quantiles <- quantile(y.values$CV)
  
  cv.threshold <- quantiles[threshold] # set threshold (4=75%, 3=50% ,2=25%, 1=all)

  cv.cutoff <- function(x) ((x)>=cv.threshold) # define function to select probes above threshold
  y.values$above.threshold<- cv.cutoff(x=y.values$CV) # identify probes above threshold
  #y.values <- y.values
  y.subset.values <- as.data.frame(subset(y.values[, -which(names(y.values) == "CV")],
                                          subset=y.values$above.threshold == TRUE)) #extract probes above threshold
  
  # change data frame into format for cluster analysis
  y.subset.values$above.threshold <- NULL
  row.names(y.subset.values) <- NULL
  row.names(y.subset.values) <- y.subset.values$ID
  y.subset.values$ID <- NULL
  y_subset_values <- y.subset.values
  
  # Return Y values and Y subset values as a list
  return(y_subset_values)

}
