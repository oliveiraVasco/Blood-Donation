require(neuralnet)
source("random_sample.R")

require(doParallel)

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
      collected.losses[insert, 1] <- i
      collected.losses[insert, 2] <- j
      if (!is.null(network$weights))
      {
        collected.losses[insert, 3] <- LogLoss(data.train[ ,1],
                                               NeuralPrediction(network$weights[[1]],
                                                                data.train[ ,-1]))
        #if (!is.na(data.cv))
        collected.losses[insert, 4] <- LogLoss(data.cv[ ,1],
                                               NeuralPrediction(network$weights[[1]],
                                                                data.cv[ ,-1]))
      }
      else
      {
        print("No convergence")
        collected.losses[insert, 3] <- 0
        collected.losses[insert, 4] <- 0
      }
      insert <- insert + 1
    }
  }
  return (collected.losses)
}

RandomSampleNeuralNetworkAnalysis <- function(data, limit.per.hidden, limit.hidden, number.samples, train.percentage, number.cores)
{
  # Generates random samples and performs NeuralNetworkAnalysis
  #
  # Args:
  #   data: Two dimensional object to be segmented in train and cross validation
  #   limit.per.hidden: Limit of nodes inside each hidden layer
  #   limit.hidden: Limit of hidden layers to test 
  #   number.samples: Number of samples to generate
  #   train.percentage: Percentage of data for train (from 0 to 1)
  #
  # Returns:
  #   output.list: List of dataframes with each sample result
  #
  
  registerDoParallel(cores = number.cores)
  options(cores = number.cores) 
  
  output.list <- list()
  #for ( i in 1:number.samples)
  output.list <- foreach (i = 1:number.samples, .combine = c) %dopar%
  {
    source("random_sample.R")
    source("neural_networks.R")
    random.indexes <- GenerateSample(nrow(data), train.percentage)
    data.train <- SegmentTrainingSample(data, random.indexes)
    data.cv <- SegmentCrossValidation(data, random.indexes)
    output.list[[i]] <- NeuralNetworkAnalysis(data.train, data.cv, limit.per.hidden, limit.hidden)
  }
  return (output.list)
}

ListAgregation <- function(error.list)
{
  # Aggregates the result of RandomSampleNeuralNetworkAnalysis computing the means of each sample
  #
  # Args:
  #   error.list: List of combined dataframes obtained from RandomSampleNeuralNetworkAnalysis
  #
  # Returns:
  #   aggregated: Information about nodes on the first two columns. Logloss of train and cross-validation respectively
  #
  
  i <- 4
  aggregated <- matrix(data = 0, nrow = length(error.list[[1]]), ncol = 4)
  aggregated[ ,1] <- error.list[[1]]
  aggregated[ ,2] <- error.list[[2]]
  len.list <- length(error.list)
  while(i <= len.list)
  {
    aggregated[ ,3] <- aggregated[ ,3] + error.list[[i-1]]
    aggregated[ ,4] <- aggregated[ ,4] + error.list[[i]]
    i <- i + 4
  }
  aggregated[ ,3] <- aggregated[ ,3] / (len.list/4)
  aggregated[ ,4] <- aggregated[ ,4] / (len.list/4)
  return (aggregated)
}
