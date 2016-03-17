#'kaggle competition
#'
#'@param training data set for training
#'@param test data set for test
#'
#'@import assertthat
#'@import caret
#'@import gbm
#'@import randomForest
#'@import functional
#'
#'@return a dataframe with predictied class on test data set
#'
#'


BigO <- function(training, test.real, outputCSV=TRUE){
  if (!require("assertthat")) install.packages("assertthat")
  if (!require("caret")) install.packages("caret")
  if (!require("gbm")) install.packages("gbm")
  if (!require("randomForest")) install.packages("randomForest")
  if (!require("functional")) install.packages("functional")


  not_empty(X)
  not_empty(y)


  ##PreProp
  #read data and remove non-predictors
  training <- read.csv("news_popularity_training.csv")[,-c(1,2,3)]
  test.real <- read.csv("testReal.csv")[,-c(1,2,3)]
  training <- training[-22686,]


  training$n_non_stop_words <- NULL
  test.real$n_non_stop_words <- NULL


  #transform response to factor
  training$popularity <- as.factor(training$popularity)


  #remove repetative variable
  rep.dummy <-findLinearCombos(training)$remove
  training[,rep.dummy] <-data.frame(NULL)
  test.real[,rep.dummy] <- data.frame(NULL)


  #make num_keywords a categorical variable
  training$num_keywords <- as.factor(training$num_keywords)
  test.real$num_keywords <- as.factor(test.real$num_keywords)


  #dummy variable name


  dummy_data_channel <- grep("data_channel", names(training))
  dummy_data_channel.name <- names(training)[dummy_data_channel]


  data_channel <- as.matrix(training[dummy_data_channel.name]) %*% 1:length(dummy_data_channel)
  data_channel <- factor(as.vector(data_channel))


  training[dummy_data_channel.name] <- data.frame(NULL)
  training <- data.frame(data_channel = data_channel, training)


  data_channel <- as.matrix(test.real[dummy_data_channel.name]) %*% 1:length(dummy_data_channel)
  data_channel <- factor(as.vector(data_channel))


  test.real[dummy_data_channel.name] <- data.frame(NULL)
  test.real <- data.frame(data_channel = data_channel, test.real)


  dummy_weekday <- grep("weekday", names(training))
  dummy_weekday.name <- names(training)[dummy_weekday]


  weekday <- as.matrix(training[dummy_weekday.name]) %*% 1:length(dummy_weekday)
  weekday <- factor(as.vector(weekday))


  training[dummy_weekday.name] <- data.frame(NULL)
  training <- data.frame(weekday = weekday, training)


  weekday <- as.matrix(test.real[dummy_weekday.name]) %*% 1:length(dummy_weekday)
  weekday <- factor(as.vector(weekday))


  test.real[dummy_weekday.name] <- data.frame(NULL)
  test.real <- data.frame(weekday = weekday, test.real)


  #preprocess the non-dummy variable
  category.name <- c("weekday", "data_channel", "num_keywords")
  training.pp <- preProcess(training[, !names(training) %in% c(category.name,"popularity")],
                            method = c("center", "scale"))


  training <- predict(training.pp, newdata = training)
  test.real <- predict(training.pp, newdata = test.real)


  set.seed(111)


  #RF1
  Grid.rf1 <-  expand.grid(mtry = 7)
  fitControl.rf1<- trainControl(method = "none")
  rf1 <- train(popularity ~ ., data = training,
               ntree=750,
               method = "rf",
               trControl = fitControl.rf1,
               verbose = FALSE,
               tuneGrid = Grid.rf1
               )


  #RF2
  Grid.rf2 <-  expand.grid(mtry = 6)
  fitControl.rf2<- trainControl(method = "none")
  rf2 <- train(popularity ~ ., data = training,
               ntree=1000,
               method = "rf",
               trControl = fitControl.rf2,
               verbose = FALSE,
               tuneGrid = Grid.rf2
               )


  #RF3
  Grid.rf3 <-  expand.grid(mtry = 8)
  fitControl.rf3<- trainControl(method = "none")
  rf3 <- train(popularity ~ ., data = training,
               ntree=1250,
               method = "rf",
               trControl = fitControl.rf3,
               verbose = FALSE,
               tuneGrid = Grid.rf3
  )



  set.seed(111)


  #gbm1
  Grid.gbm1 <-  expand.grid(interaction.depth = 13,
                          n.trees = c(800),
                          shrinkage = 0.008,
                          n.minobsinnode = 10)
  fitControl.gbm1 <- trainControl(method = "none")
  gbm1 <- train(popularity ~ ., data = training,
                  method = "gbm",
                  trControl = fitControl.gbm1,
                  verbose = FALSE,
                  tuneGrid = Grid.gbm1)


  #gbm2
  Grid.gbm2 <-  expand.grid(interaction.depth = 10,
                            n.trees = c(2000),
                            shrinkage = 0.003,
                            n.minobsinnode = 15)
  fitControl.gbm2 <- trainControl(method = "none")
  gbm2 <- train(popularity ~ ., data = training,
                method = "gbm",
                trControl = fitControl.gbm2,
                verbose = FALSE,
                tuneGrid = Grid.gbm2)


  #gbm3
  Grid.gbm3 <-  expand.grid(interaction.depth = 8,
                            n.trees = c(1000),
                            shrinkage = 0.01,
                            n.minobsinnode = 15)
  fitControl.gbm3 <- trainControl(method = "none")
  gbm3 <- train(popularity ~ ., data = training,
                method = "gbm",
                trControl = fitControl.gbm3,
                verbose = FALSE,
                tuneGrid = Grid.gbm3)


  #prediction
  pred.rf1 <- predict(rf1, test.real)
  pred.rf2 <- predict(rf2, test.real)
  pred.rf3 <- predict(rf3, test.real)


  pred.gbm1 <- predict(gbm1, test.real)
  pred.gbm2 <- predict(gbm2, test.real)
  pred.gbm3 <- predict(gbm3, test.real)


  #Take majority rule
  predMatrix <- cbind(pred.rf1, pred.rf2, pred.rf3, pred.gbm1, pred.gbm2, pred.gbm3)


  pred.avg<-sapply(1:nrow(predMatrix), function(idx) {
    # get the number of time each entry in matrix occurs
    t <- table(t(predMatrix[idx, ]))
    # get the maximum count
    t.max <- max(t)
    # sample 1 value that equate to maximum count
    t <- as.numeric(names(t[t == t.max]))
    t <- t[sample(length(t),1)]
  })


  #output
  result <- data.frame(id = 30001:39644, popularity = pred.avg)


  if(outputCSV){
    write.csv(result, "BigO.csv", row.names = FALSE)
  }


  return(result)


}
