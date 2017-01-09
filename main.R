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


data.train <- cbind(donation, data.train)

rm(donation, normalized.data)

# Simple segmentation -------------------
#train.dimension.index <- nrow(data.train) - round(nrow(data.train) * 0.7)
#data.cv <- data.train[1:train.dimension.index, ]
#data.train <- data.train[(train.dimension.index + 1):nrow(data.train), ]

# Network Fitting -----------------------

#network <- NeuralNetworkFitting(data.train, c(1))

#errors <- NeuralNetworkAnalysis(data.train, data.cv, 6, 3)

errors.list <- RandomSampleNeuralNetworkAnalysis(data.train, 2, 2, 2, 0.7, 2)

plot(errors[ ,3], ylim = range(c(errors[ ,3], errors[ ,4])))
par(new = TRUE)
plot(errors[ ,4], col = "blue", ylim = range(c(errors[ ,3], errors[ ,4])))
