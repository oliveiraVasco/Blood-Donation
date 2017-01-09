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


# Network random sample analysis -----------------------

limit.per.hidden = 4
limit.hidden = 5
number.samples = 5
train.percentage = 0.7
number.cores = 4

errors.list <- RandomSampleNeuralNetworkAnalysis(data.train, limit.per.hidden, 
                                                 limit.hidden, number.samples, 
                                                 train.percentage, number.cores)

# Result aggregation ----------------------

aggregated <- ListAgregation(errors.list)

# Result plotting -------------------------

plot(aggregated[ ,3], ylim = range(c(aggregated[ ,3], aggregated[ ,4])),
     xlab = "Total_Nodes", ylab = "Log_Loss")
par(new = TRUE)
plot(aggregated[ ,4], col = "blue", ylim = range(c(aggregated[ ,3], aggregated[ ,4])),
     xlab = "Total_Nodes", ylab = "Log_Loss")



