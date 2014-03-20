library("Biobase")

massi_y <- function(expression_data, y_probes){
  
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
  
  # set probe as ID for y_probes
  y_probes$ID <- row.names(y_probes) 
  
  # extract matched probes from expression matrix using ID
  y.values <- as.data.frame(merge(expression_data, y_probes, by="ID")) 
  
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
