library("Biobase")

massi_y <- function(expression.data, y.probes){
  
  #Check that the input data is in the correct data.frame or ExpressionSet class
  class.expression.data <- class(expression.data)
  class.expression.data <- class.expression.data[1]
  is.ExpressionSet <- ifelse(class.expression.data == "ExpressionSet", yes=TRUE, no=FALSE)
  #if(is.ExpressionSet == TRUE) {print("OK: Input expression data as ExpressionSet")}
  is.data.frame <- ifelse(class.expression.data == "data.frame", yes=TRUE, no=FALSE)
  #if(is.data.frame == TRUE) {print("OK: Input expression data as data.frame")}
  
  # test if input data is in accpeted formats, and stop if not.
  if((is.ExpressionSet == FALSE) & (is.data.frame == FALSE)) {
    stop("Input data must be as data.frame or ExpressionSet class")
  }
  
  # Check class of y.probes  
  class.y.probes <- class(y.probes)
  if(class.y.probes != "data.frame") stop("Input y.probes data must be in data.frame class")
  
  # If input is in ExpressionSet class, convert to data.frame
  if(class.expression.data == "ExpressionSet") {
    expression.data <- data.frame(exprs(expression.data))
    expression.data$ID <- rownames(expression.data) # set probe as ID
  }
  
  if(class.expression.data == "data.frame") {
    expression.data$ID <- rownames(expression.data) # set probe as ID
  }
  
  # set probe as ID for y.probes
  y.probes$ID <- row.names(y.probes) 
  
  # extract matched probes from expression matrix using ID
  y.values <- as.data.frame(merge(expression.data, y.probes, by="ID")) 
  
  # count number of probes with match in dataset
  n.matched.probes <- as.numeric(nrow(y.values))
  
  # define function to calculate CV
  cal.cv <- function(x) ( 100*sd(x)/mean(x) )
  
  # calculate CV for each probe
  y.values$CV <- apply(y.values[, -which(names(y.values) == "ID")], MARGIN=1, FUN=cal.cv)
 
  # calculate quantiles for probe CV
  quantiles <- quantile(y.values$CV)
  
  # Create list for function output
  probe.CV.values <- list("id" = y.values$ID, "cv" = y.values$CV, "quantiles" = quantiles)
  
  # Return list with probe CV and quantiles
  return(probe.CV.values)
}
