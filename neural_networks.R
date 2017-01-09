require(neuralnet)

NeuralNetworkFitting <- function(data, hidden.layers)
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
  
  formula <- as.formula(paste(colnames(data)[1], " ~ ", 
                           paste(colnames(data)[-1], collapse = " + ")))
  
  net <- neuralnet(formula, data, hidden = hidden.layers)
  return (net)
}

SigmoidFunction <- function(data)
{
  # Computes sigmoid function
  #
  # Args:
  #   data: Object or value to be computed
  #
  # Returns
  #   data: Sigmoid of object/value 
  #
  
  data <- 1 / (1 + exp(-data))
  return (data)
}

NeuralPrediction <- function(weights.list, data)
{
  # Given the weights and the input, computes the neural network prediction
  #
  # Args:
  #   weights.list: List of weights
  #   data: Input data
  #
  # Return:
  #   prediction: Neural network prediction
  #
  
  layers <- length(weights.list)
  prediction <- as.matrix(data)
  for( i in 1:layers)
  {
    prediction <- cbind(rep(1, nrow(prediction)), prediction)
    prediction <- SigmoidFunction( prediction %*% weights.list[[i]] )
  }
  return (prediction)
}

LogLoss <- function(y, y.predicted)
{
  # Computes the logarithm loss function
  #
  # Args:
  #   y: Real values
  #   y.predicted: Estimated values
  #
  # Returns:
  #   loss: Log loss obtained
  #
  
  loss <- - sum( y * log(y.predicted) + ( 1 - y ) * log( 1 - y.predicted)) / (length(y))
  return (loss)
}

NeuralNetworkAnalysis <- function(data.train, data.cv, limit.per.hidden, limit.hidden)
{
  # Function collects the Log Loss of neural networks fitted 
  #
  # Args: 
  #   data.train: Two dimensional obect with y on the first column (used for training/fitting)
  #   data.cv: Two dimensional obect with y on the first column (used for cross validation)
  #   limit.per.hidden: Limit of nodes inside each hidden layer
  #   limit.hidden: Limit of hidden layers to test 
  #
  # Returns:
  #   collected.losses: Two dimensional object with log loss in sample and out of sample.
  #
  
  collected.losses <- data.frame()
  insert <- 1
  hidden.structure <- c(0)
  for ( i in 1:limit.hidden)
  {
    hidden.structure[i] <- 0
    for(j in 1:limit.per.hidden)
    {
      print( paste("Limit hidden:", i, "; Limit per hidden:", j))
      hidden.structure[i] <- hidden.structure[i] + 1
      print(hidden.structure)
      network <- NeuralNetworkFitting(data.train, hidden.structure)
      if (!is.null(network$weights))
      {
        collected.losses[insert, 1] <- i
        collected.losses[insert, 2] <- j
        collected.losses[insert, 3] <- LogLoss(data.train[ ,1],
                                               NeuralPrediction(network$weights[[1]],
                                                                data.train[ ,-1]))
        #if (!is.na(data.cv))
        collected.losses[insert, 4] <- LogLoss(data.cv[ ,1],
                                               NeuralPrediction(network$weights[[1]],
                                                                data.cv[ ,-1]))
        insert <- insert + 1
      }
      else
        print("No convergence")

    }
  }
  return (collected.losses)
}