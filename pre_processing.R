
Normalization <- function(features)
{
  # Normalizes each column of features
  #
  # Args: 
  #   features: Two dimensional object to normalize
  #
  # Returns:
  #   features: Normalized two dimensional object
  #
  
  for ( i in 1:ncol(features))
  {
    features[ ,i] <- (features[ ,i] - min(features[ ,i])) / (max(features[ ,i]) - min(features[ ,i]))
  }
  return (features)
}