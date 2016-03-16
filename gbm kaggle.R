install.packages("gbm")
library(gbm)
# read data
data <- read.csv("news_popularity_training.csv", header = TRUE)

# set seed and split training data into train and test
set.seed(1111)
train <- data[sample(nrow(data), 21000), ]
tost <- data[!(data$id %in% train$id), ]
train1<-train[sample(nrow(train), 9000),]

fit8<-randomForest(as.factor(train1$popularity)~ train1$timedelta 
                   + train1$n_tokens_content + train1$n_non_stop_words 
                   + train1$kw_min_avg +train1$self_reference_avg_sharess 
                   + train1$is_weekend +train1$self_reference_max_shares 
                   + train1$self_reference_min_shares
                   + train1$data_channel_is_entertainment 
                   + train1$LDA_02 + train1$kw_min_avg
                   + train1$kw_max_avg, data=train1, importance=TRUE, ntree=500)
#model 1 gbm
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

summary(model)
gbm.perf(model)
#test predictions
testpredictions<-predict(object = model, newdata = test
                         , n.trees = gbm.perf(model,plot.it = FALSE)
                         ,type = "response")
testpredictions<-round(testpredictions)
test.pred<-as.data.frame(testpredictions)
actual <- as.data.frame(test)[,62]
percent.correct <- mean(test.pred[,1] == actual)
#model2 gbm
fit7<-randomForest(as.factor(train1$popularity)~ train1$self_reference_min_shares +train1$timedelta
                                   + train1$LDA_01+ train1$LDA_02
                                  + train1$LDA_03 + train1$LDA_04
                                   + train1$self_reference_min_shares + train1$self_reference_avg_sharess
                                   + train1$is_weekend + train1$rate_positive_words 
                                   + train1$title_sentiment_polarity, data=train1, importance=TRUE, ntree=1000)

model1<-gbm(formula = fit7,
           distribution = "gaussian",
           data = train1,
           var.monotone = NULL,
           n.trees = 500,
           interaction.depth = 3,
           n.minobsinnode = 30,
           shrinkage = 0.01,
           bag.fraction = 0.5,
           train.fraction = 1.0,
           cv.folds=2,
           keep.data = TRUE,
           verbose = TRUE,
           class.stratify.cv=NULL,
           n.cores = NULL)
summary(model1)
gbm.perf(model1)

for(i in 1:length(model1$var.names)){
  plot(model, i.var = 5
       , n.trees=gbm.perf(model1,plot.it = FALSE)
       ,type = "response")
}

testpredictions<-predict(object = model1, newdata = test
                         , n.trees = gbm.perf(model1,plot.it = FALSE)
                         ,type = "response")
testpredictions<-round(testpredictions)
test.pred<-as.data.frame(testpredictions)
actual <- as.data.frame(test)[,62]
percent_correct <- mean(test.pred[,1] == actual)


#try to subset observations with popularity=4
trainp4<-train[train$popularity %in% c(3,4,5),]

verypop<-as.data.frame(rbind(trainp4,data[sample(nrow(train),nrow(trainp4)),]))
# regress on randomforest model and chech which variables predict better our observations
obs4<-randomForest(as.factor(verypop$popularity) ~verypop$timedelta + verypop$n_tokens_title 
                   +verypop$n_tokens_content + verypop$n_unique_tokens
                   +verypop$n_non_stop_unique_tokens, data = verypop, importance=TRUE, ntree=500)



# number of data distribution
ob1<-data[data$popularity %in% 1,] #9478 obs,0.3159333
ob2<-data[data$popularity %in% 2,] #13764 obs 0.4588
ob3<-data[data$popularity %in% 3,] #5712 obs 0.1904
ob4<-data[data$popularity %in% 4,] #999 obs  0.0333
ob5<-data[data$popularity %in% 5,] #47 obs   0.001566667
obfe1<-train[train$popularity %in% 1,]

# find the best predictors for popularity 1
# create subset with more popularity 1 observations
pop1<-as.data.frame(rbind(ob1,data[sample(nrow(data),nrow(ob1)),]))


fitpop1<-randomForest(as.factor(pop1$popularity)~ self_reference_min_shares +timedelta
                   + LDA_01+ LDA_02
                   + LDA_03 +LDA_04
                   + self_reference_min_shares + self_reference_avg_sharess
                   + is_weekend + rate_positive_words 
                   + title_sentiment_polarity, data=pop1, importance=TRUE, ntree=1000)
# 11 variables
# error rate: 31.78%, 0,0411 in pop1
#weekend,timedelta,LDA02 most significant
varImpPlot(fitpop1)

############ 2nd trial###############
#11 other variables
# error rate: 33.58%, 0.0280 in pop1
#average_token_length,n_tokens_content,n_unique_tokens,n_non_stop_words most significant
fitpop2 <- randomForest(as.factor(popularity) ~ n_tokens_content+  num_self_hrefs
                        +n_non_stop_words +num_hrefs + num_imgs + num_videos 
                    + n_tokens_title + num_keywords + timedelta + n_unique_tokens 
                    + average_token_length , data=pop1, importance=TRUE, ntree=1000)
varImpPlot(fitpop2)

############ 3rd trial###############
# 11 variables
# error rate: 33.65%%, 0.0186 in pop1
#kw_max_min + kw_avg_min + kw_min_max, kw_avg_max kw_max_avg kw_min_avg
fitpop3<-randomForest(as.factor(popularity)~kw_min_avg 
                      + data_channel_is_entertainment + kw_avg_max
                      + data_channel_is_socmed + kw_max_avg  
                      + data_channel_is_world + kw_min_avg + kw_max_min + kw_avg_min + kw_min_max
                      + kw_max_max  , data=pop1, importance=TRUE, ntree=1000)

#kw_avg_max + kw_min_avg + kw_max_avg + kw_min_avg
varImpPlot(fitpop3)

############ 4th trial###############
# 11 variables
# error rate: 33.99%, 0.0263 in pop1
#self_reference_min_shares,self_reference_max_shares,self_reference_avg_sharess,isweekend
fitpop4<-randomForest(as.factor(popularity)~self_reference_min_shares
                      + self_reference_max_shares + self_reference_avg_sharess
                      + weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday
                      + weekday_is_thursday + weekday_is_friday 
                      + weekday_is_saturday + weekday_is_sunday 
                      + is_weekend, data=pop1, importance=TRUE, ntree=1000)
varImpPlot(fitpop4)

############ 5th trial###############
# 11 variables
# error rate: 34.25%, 0.0372 in pop1
#             33.73%  0.0341
# global_rate_negative_words,LDA 00,02,04
fitpop5<-randomForest(as.factor(popularity)~ 
                   + LDA_00 + LDA_01+ LDA_02
                   + LDA_03 + LDA_04 + global_subjectivity 
                   + global_sentiment_polarity + global_rate_positive_words 
                   + global_rate_negative_words + self_reference_max_shares 
                   + self_reference_min_shares, data=pop1, importance=TRUE, ntree=1000)
varImpPlot(fitpop5)

############ FINAL trial###############
#32.35% 0.0347
#31.83% 0.036
#kwminmax is dissapointing
fitfinal<-randomForest(as.factor(popularity)~  LDA_00 + LDA_02 + LDA_04
        + global_rate_negative_words + self_reference_min_shares 
        + kw_max_avg + kw_min_avg +kw_max_min 
        + kw_avg_min + kw_min_max + kw_avg_max + average_token_length 
        + n_tokens_content + n_unique_tokens + n_non_stop_words + is_weekend, data=pop1, importance=TRUE, ntree=1000)
varImpPlot(fitfinal)

#test in random subsets
test<-randomForest(as.factor(popularity)~  LDA_00 + LDA_02 + LDA_04
             + global_rate_negative_words + self_reference_min_shares 
             + kw_max_avg + kw_min_avg +kw_max_min 
             + kw_avg_min + kw_min_max + kw_avg_max + average_token_length 
             + n_tokens_content + n_unique_tokens + n_non_stop_words + is_weekend, data=train1, importance=TRUE, ntree=410)
varImpPlot(test)


Prediction <- predict(test,tost )
pred<-as.data.frame(Prediction)
actual <- as.data.frame(tost)[,62]
percent_correct <- mean(pred[,1] == actual)












testReal<-read.csv("testReal.csv",header = TRUE)


m<-as.data.frame(cbind(y=data[,62],data[,3:61]))
#9,13,28,35,36,37
gbm1<-gbm(formula =  y~n_non_stop_unique_tokens 
          + n_tokens_content +  num_self_hrefs
          + n_non_stop_words +num_hrefs + num_imgs + num_videos 
          + n_tokens_title  + timedelta + n_unique_tokens 
          + LDA_00 + LDA_01+ LDA_02 +num_hrefs  
          + LDA_03 + LDA_04 + global_subjectivity 
          + global_sentiment_polarity + global_rate_positive_words 
          + global_rate_negative_words + self_reference_max_shares
          + self_reference_min_shares + self_reference_avg_sharess
          + data_channel_is_entertainment
          + data_channel_is_socmed       
          + data_channel_is_tech         
          + data_channel_is_world
          + is_weekend +average_token_length  
          + weekday_is_monday  + weekday_is_wednesday
           + weekday_is_friday  + weekday_is_sunday +  
            +data_channel_is_entertainment + kw_avg_max +kw_avg_avg 
          +kw_max_avg +kw_max_min  + kw_min_avg + kw_max_min + kw_avg_min + kw_min_max + rate_positive_words + rate_negative_words+ avg_positive_polarity  
          + min_positive_polarity   + min_negative_polarity + max_negative_polarity  
          + title_subjectivity +abs_title_sentiment_polarity  +title_sentiment_polarity,
            distribution = "gaussian",
          weights = as.data.frame(c(rep(1,8),2,1,1,1,3,rep(1,15),2,rep(1,7),3,3,rep(1,12))),
            data = m,
            var.monotone = NULL,
            n.trees = 350,
            interaction.depth = 10,
            n.minobsinnode = 70,
            shrinkage = 0.05,
            bag.fraction = 0.5,
            train.fraction = 0.5,
            cv.folds= 0,
            keep.data = TRUE,
            verbose = TRUE,
            class.stratify.cv=NULL,
            n.cores = NULL)
summary(gbm1)
# check performance using an out-of-bag estimator
best.iter <- gbm.perf(gbm1,method="OOB")
print(best.iter)
# check performance using a 50% heldout test set
best.iter <- gbm.perf(gbm1,method="test")
print(best.iter)

# check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbm1,method="cv")
print(best.iter)
# plot the performance # plot variable influence
summary(gbm1,n.trees=1) # based on the first tree
summary(gbm1,n.trees=best.iter) # based on the estimated best number of trees

# compactly print the first and last trees for curiosity
print(pretty.gbm.tree(gbm1,1))
print(pretty.gbm.tree(gbm1,gbm1$n.trees))



#predict the error
Pradiction <- predict(gbm1, testReal,n.trees=best.iter)
pred<-as.data.frame(Pradiction)
pred<-round(pred,2)
pred<-round(pred)
actual <- as.data.frame(testReal)[,62]
percent.correct <- mean(pred[,1] == actual)





ob1<-testReal[testReal$popularity %in% 1,] #2946 obs,30,56%
ob2<-testReal[testReal$popularity %in% 2,] #4530 obs 46,97%
ob3<-testReal[testReal$popularity %in% 3,] #1831 obs 18,99%
ob4<-testReal[testReal$popularity %in% 4,] #326 obs  33,8%
ob5<-testReal[testReal$popularity %in% 5,] #11 obs   0,1%




###PCA ANALYSIS

pca<-prcomp(data[,3:ncol(data)-1],
center = TRUE,
scale. = TRUE) 

head(unclass(pca$rotation))
#VARIANCE RETAINED BY EACH PRINCIPAL COMPONENT
eig<-(pca$sdev)**2
#VARIANCES IN PERCENTAGE
variance<-eig*100/sum(eig)
#CUMULATIVE VARIANCES
cumvar<-cumsum(variance)
eig.data<-data.frame(eig=eig,variance=variance,cumvariance=cumvar)
head(eig.data)
summary(pca)
#cumulative variance explained
plot(cumsum(pca$sdev^2/sum(pca$sdev^2)))
pc.use<-3# explains 80% of variance
trunc<-pca$x[,1:pc.use]%*%t(pca$rotation[,1:pc.use])
#and add the center(and rescale back to data)
if(pca$scale!=FALSE){
  trunc<-scale(trunc,center=FALSE,scale = 1/pca$scale)
}
if(pca$center!=FALSE){
  trunc<-scale(trunc,center = -1*pca$center,
               scale=FALSE)
}
  
}
dim(trunc)
#get the eigenvalues
install.packages("factoextra")
library("factoextra")
eig.val<-get_eigenvalue(pca)



