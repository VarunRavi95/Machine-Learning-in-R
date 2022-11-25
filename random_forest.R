# training data for random forest
library(randomForest)

x1 <- runif(100)


x2 <- runif(100)

trdata <- cbind(x1, x2)
y <- as.numeric(x1 < x2)
trlabels <- as.factor(y)

trdata <- cbind(trdata, trlabels)

rf_model1 <- randomForest(as.factor(trlabels)~., 
                          data = trdata, 
                          ntree = 1, 
                          nodesize = 25,
                          keep.forest = TRUE)

rf_model2 <- randomForest(as.factor(trlabels)~.,
                          data = trdata,
                          ntree = 10,
                          nodesize = 25,
                          keep.forest = TRUE)

rf_model3 <- randomForest(as.factor(trlabels)~.,
                          data = trdata,
                          ntree = 100,
                          nodesize = 25, 
                          keep.forest = TRUE)

set.seed(1234)

x1 <- runif(1000)
x2 <- runif(1000)
tedata <- cbind(x1, x2)

y <- as.numeric(x1<x2)
telabels <- as.factor(y)
tedata <- cbind(tedata, telabels)
tedata[,3] <- as.factor(tedata[,3])
# plot(x1, x2, col = (y+1))

pred_1 <- predict(object = rf_model1, newdata = tedata, type = 'class')
pred_2 <- predict(object = rf_model2, newdata = tedata, type = 'class')
pred_3 <- predict(object = rf_model3, newdata = tedata, type = 'class')

missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

missclass_1 <- missclass(tedata[,3], pred_1)
missclass_2 <- missclass(tedata[,3], pred_2)
missclass_3 <- missclass(tedata[,3], pred_3)

# tree_nr <- c(1,10,100)
# model_list <- list()
# for (i in 1:length(tree_nr)) {
#   rf_model <- randomForest(as.factor(trlabels)~.,
#                             data = trdata,
#                             ntree = tree_nr[i],
#                             nodesize = 25,
#                             keep.forest = TRUE)
#   model_list[i] <- append(model_list,rf_model1[i])
# }

