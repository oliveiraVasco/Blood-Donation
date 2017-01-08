require(neuralnet)

NeuralNetworkFitting <- function(output, input, hidden.layers)
{
  # Neural network fitting
  #
  # Args: 
  #   ouput: Data frame with results of each observation
  #   input: Data frame with features in each column
  #   hidden.layers: one dimensional object with the number of hidden layers
  #
  # Return:
  #   net: Fitted network
  #
  
  data <- cbind(output, input)
  formula <- as.formula(paste(colnames(output), " ~ ", 
                           paste(colnames(input), collapse = " + ")))
  
  net <- neuralnet(formula, data, hidden = hidden.layers)
  return (net)
}
