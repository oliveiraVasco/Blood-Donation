
Normalization <- function(features)
{
  for ( i in 1:ncol(features))
  {
    features[ ,i] <- (features[ ,i] - min(features[ ,i])) / (max(features[ ,i]) - min(features[ ,i]))
  }
  return (features)
}