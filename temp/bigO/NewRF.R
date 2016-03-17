library(caret)
library(randomForest)

#read data and remove non-predictors
training <- read.csv("news_popularity_training.csv")[,-c(1,2,3)]
test.real <- read.csv("testReal.csv")[,-c(1,2,3)]
training <- training[-22686,]

training$n_non_stop_words <- NULL
test.real$n_non_stop_words <- NULL

#transform response to factor
training$popularity <- as.factor(training$popularity)



#remove repetative dummy variable
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
dataset <- rbind(training, test.real)

















library(doMC)
registerDoMC(cores = 4)


set.seed(111)
rfGrid <-  expand.grid(mtry = 5:15)


fitControl.rf<- trainControl(method = "CV", number = 1,
                                  index = list(Fold1 = 1:29999),
                                  indexOut = list(Fold2 = 30000:39643))


rfFit <- train(popularity ~ ., data = dataset,
                    ntree=1000,
                    method = "rf",
                    trControl = fitControl.rf,
                    verbose = FALSE,
                    tuneGrid = rfGrid
                    )
