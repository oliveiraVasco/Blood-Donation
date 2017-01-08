source("pre_processing.R")
source("neural_networks.R")

# Collecting data -----------------------

data.train <- read.csv("data/train.csv")
data.test <- read.csv("data/test.csv")

# Data cleaning -------------------------

donation <- as.data.frame(data.train[ ,ncol(data.train)])
colnames(donation) <- "Donation"
data.train <- data.train[ ,-ncol(data.train)]
data.train <- data.train[ ,-1]
data.test <- data.test[ ,-1]


# Data pre Processing -------------------

normalized.data <- Normalization(rbind(data.train, data.test))

data.train <- normalized.data[1:nrow(data.train), ]
data.test <- normalized.data[(nrow(data.train) + 1):nrow(normalized.data), ]

rm(normalized.data)

# Network Fitting -----------------------

network <- NeuralNetworkFitting(donation, data.train, c(1))
