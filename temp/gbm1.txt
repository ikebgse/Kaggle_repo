# set working directory
setwd("C:/Users/MF/Documents/BGSE/TERM 2/Kaggle/data")

# fetch required libraries
library(gbm)
library(randomForest)

# read data
data <- read.csv("news_popularity_training.csv", header = TRUE)

# set seed
set.seed(1111)

# sample all columns from the first 21000 rows without replacement -> training sample [21000 x 62]obs
train <- data[sample(nrow(data), 21000), ]

# take all the rows in data with a different id than the the ones in the training sample, with all columns-> testing sample [9000 x 62]obs
test <- data[!(data$id %in% train$id), ]

#subsample the training set with 9k obs
train1<-train[sample(nrow(train), 9000), ]


##################################################
##################################################
##################################################

# 1st generalized boosted model (GBM)

# growing 1000 trees: random forest predicting popularity through certain variables.
fit8<-randomForest(as.factor(train1$popularity) ~ train1$timedelta 
                   + train1$n_tokens_content + train1$n_non_stop_words 
                   + train1$kw_min_avg +train1$self_reference_avg_sharess 
                   + train1$is_weekend +train1$self_reference_max_shares 
                   + train1$self_reference_min_shares
                   + train1$data_channel_is_entertainment 
                   + train1$LDA_02 + train1$kw_min_avg
                   + train1$kw_max_avg,
                   data=train1,
                   importance=TRUE,
                   ntree=500)

# 1st model
# applying a t-distribution on the 21k training sample, 500 trees, depth of 3
model<-gbm(formula = fit8,
    distribution = "tdist",
    data = train,
    var.monotone = NULL,
    n.trees = 500,
    interaction.depth = 3,
    n.minobsinnode = 80,
    shrinkage = 0.01,
    bag.fraction = 0.5,
    train.fraction = 1.0,
    cv.folds=0,
    keep.data = TRUE,
    verbose = TRUE,
    class.stratify.cv=NULL,
    n.cores = NULL)

#.#picking the top 11 variables?
summary(model)
gbm.perf(model)

# using the new model to test predictions : generating 9k popularity prediction on the testing data. maybe "response" uses the last col as the predicted var
testpredictions<-predict(object = model,
                         newdata = test,
                         n.trees = gbm.perf(model,plot.it = FALSE),
                         type = "response")

# rounding predictions and setting them in a dataframe
testpredictions<-round(testpredictions)
test.pred<-as.data.frame(testpredictions)

# fetching the actual popularity variable in the testing sample
actual <- as.data.frame(test)[,62]

# accuracy = 0.4528889
percent.correct <- mean(test.pred[,1] == actual)
percent.correct


##################################################
##################################################
##################################################