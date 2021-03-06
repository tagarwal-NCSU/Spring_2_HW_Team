###################################
#                                 #
#         Fraud Analytics:        #
#        Anomaly Detection        #
#                                 #
#          Dr Aric LaBarr         #
#                                 #
###################################

# # Needed Libraries for Analysis #
# install.packages("benford.analysis")
# install.packages("robustbase")
# install.packages("car")
# install.packages("stats")
# install.packages("FNN")
# install.packages("dbscan")
# install.packages("randomForest")
# install.packages("devtools")
# install.packages("isotree")

library(benford.analysis)
library(robustbase)
library(car)
library(stats)
library(FNN)
library(dbscan)
library(randomForest)
library(devtools)
library(isotree)

# Load Needed Data #
# setwd(".")
ins <- read.csv(file = "aggregated_policy_nolabel.csv", header = TRUE)

# Benford's Law #
rewarddigit1 <- as.numeric(substring(ins$Reward_Amount, first = 1, last = 1))
ben <- benford(rewarddigit1, number.of.digits = 1)

plot(ben, multiple = FALSE)
chisq(ben)

# Z-scores #
hist(ins$Coverage_Income_Ratio_Claim)
x <- ins$Coverage_Income_Ratio_Claim

ins$Z_Coverage_Income_Ratio_Claim <- abs((x - mean(x))/sd(x))
hist(ins$Z_Coverage_Income_Ratio_Claim)

length(which(ins$Z_Coverage_Income_Ratio_Claim > 3))

# Robust Z-scores #
ins$RZ_Coverage_Income_Ratio_Claim <- abs((x - median(x))/mad(x))
hist(ins$RZ_Coverage_Income_Ratio_Claim)

length(which(ins$RZ_Coverage_Income_Ratio_Claim > 3))

# Boxplots - 1.5*IQR Rule #
par(mfrow = c(1,2))

bp <- boxplot(ins$Coverage_Income_Ratio_Claim, 
              col = "lightblue", 
              main = "Standard Boxplot", 
              ylab = "Ratio of Income to Coverage at Claim", 
              ylim = c(-5, 45))

adjbp <- adjbox(ins$Coverage_Income_Ratio_Claim, 
                col = "lightblue", 
                main = "Adjusted Boxplot", 
                ylab = "Ratio of Income to Coverage at Claim", 
                ylim = c(-5, 45))

length(bp$out)
length(adjbp$out)

par(mfrow = c(1,1))

# Mahalanobis Distances #
plot(x = ins$Time_Between_CL_R, y = ins$Cov_Limit_Claim/1000, 
     xlim = c(-10, 30), 
     ylim = c(-10, 3000),
     main = "Coverage Limit vs. Speed of Payment",
     xlab = "Time Between Claim and Payment",
     ylab = "Coverage Limit at Claim (Thousands $)")

df <- data.frame(Time = ins$Time_Between_CL_R, 
                 CovLimit = ins$Cov_Limit_Claim/1000)

rad <- sqrt(qchisq(0.9999975, ncol(df)))
ellipse(center = colMeans(df, na.rm = TRUE), 
        shape = cov(df), radius = rad, col = "lightblue")

# MCD Adjustment to MD #
mcdresult <- covMcd(df)
robustcenter <- mcdresult$center
robustcov <- mcdresult$cov

rad <- sqrt(qchisq(0.9999975, ncol(df)))
ellipse(center = robustcenter, shape = robustcov, 
        radius = rad, col = "red")

# k-Nearest Neighbors #
df <- data.frame(Time = ins$Time_Between_CL_R, 
                 CovLimit = ins$Cov_Limit_Claim/1000)

ins_knn <- get.knn(data = df, k = 5)

df$knn_score <- rowMeans(ins_knn$nn.dist)

plot(CovLimit ~ Time, data = df, cex = sqrt(knn_score)+0.01, pch = 20,
     xlim = c(-10, 30), 
     ylim = c(-10, 3000),
     main = "Coverage Limit vs. Speed of Payment \nSize Adjusted for kNN Score",
     xlab = "Time Between Claim and Payment",
     ylab = "Coverage Limit at Claim (Thousands $)")

# Local Outlier Factor #
df <- data.frame(Time = ins$Time_Between_CL_R, 
                 CovLimit = ins$Cov_Limit_Claim/1000)

ins_lof <- lof(scale(df), k = 5)

df$lof_score <- ins_lof

plot(CovLimit ~ Time, data = df, cex = lof_score, pch = 20,
     xlim = c(-10, 30), 
     ylim = c(-10, 3000),
     main = "Coverage Limit vs. Speed of Payment 
             \nSize Adjusted for LOF Score",
     xlab = "Time Between Claim and Payment",
     ylab = "Coverage Limit at Claim (Thousands $)")

# Isolation Forests #
df <- data.frame(Time = ins$Time_Between_CL_R, 
                 CovLimit = ins$Cov_Limit_Claim/1000)

ins_for_100 <- isolation.forest(df = df, ntrees = 100, random_seed = 12345)
ins_score_100 <- predict(ins_for_100, df)

ins_for_500 <- isolation.forest(df = df, ntrees = 500, random_seed = 12345)
ins_score_500 <- predict(ins_for_500, df)

hist(ins_score_500)
df$iso_score <- ins_score_500

plot(CovLimit ~ Time, data = df, cex = iso_score, pch = 20,
     xlim = c(-10, 30), 
     ylim = c(-10, 3000),
     main = "Coverage Limit vs. Speed of Payment \nSize Adjusted for ISO Score",
     xlab = "Time Between Claim and Payment",
     ylab = "Coverage Limit at Claim (Thousands $)")

# Classifier-Adjusted Density Estimation #
df <- data.frame(Time = ins$Time_Between_CL_R, 
                 CovLimit = ins$Cov_Limit_Claim/1000)

trans_uni <- function(x, len = length(x)) {
  if ( is.integer(x) ) {
    sample(min(x):max(x), len, replace = TRUE)
  } else if ( is.numeric(x) ) {
    runif(len, min(x), max(x))
  } else if ( is.factor(x) ) {
    factor(sample(levels(x), len, replace = TRUE))
  } else {
    sample(unique(x), len, replace = TRUE)
  }
}

cade <- function(df, n_tree) {
  
  actual <- df
  
  rand <- as.data.frame(lapply(actual, trans_uni))
  
  actual$y <- 0
  rand$y <- 1

  data <- rbind(actual, rand)
  
  tree <- randomForest(as.factor(y) ~ ., data = data, ntree = n_tree)
  
  # The classifier probabilities
  df$prob <- predict(tree, newdata = df, type = 'prob')[, 2]
  df$odds <- df$prob / (1 - df$prob)
  
  df
}

ins_cade <- cade(df = df, n_tree = 500)

df$prob <- ins_cade$prob
df$odds <- ins_cade$odds

plot(CovLimit ~ Time, data = df, cex = odds, pch = 20,
     xlim = c(-10, 30), 
     ylim = c(-10, 3000),
     main = "Coverage Limit vs. Speed of Payment \nSize Adjusted for CADE Odds",
     xlab = "Time Between Claim and Payment",
     ylab = "Coverage Limit at Claim (Thousands $)")

hist(ins_cade$odds[which(ins_cade$prob > 0.01)], breaks = 50,
     main = "Histogram of CADE Odds")

df <- data.frame(ins$Type,
                 ins$Reward_Reason,
                 ins$Reward_Amount,
                 ins$Number_Changes,
                 ins$Cov_Limit_Claim,
                 ins$Income_Claim,
                 ins$Initial_to_Claim_Difference,
                 ins$CH_Count_6Mon,
                 ins$Coverage_CH_After_Claim,
                 ins$Susp_Adj_Tech,
                 ins$Adj_Tech_Avg_Reward,
                 ins$Med_Deletion_Flag,
                 ins$Med_Conditions,
                 ins$Time_Between_CL_R, 
                 ins$Changes_Med_History,
                 ins$Adj_Tech_Avg_Reward,
                 ins$Susp_Adj_Tech,
                 ins$Coverage_Income_Ratio_Claim,
                 ins$Adj_Tech_Confidence, 
                 ins$Tech_Adj_Confidence)

ins_cade <- cade(df = df, n_tree = 500)

hist(ins_cade$prob[which(ins_cade$prob > 0.01)], breaks = 50)

which.max(ins_cade$prob)
ins[6,]
