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
plot(rf_model1)
rf_model2 <- randomForest(as.factor(trlabels)~.,
                          data = trdata,
                          ntree = 10,
                          nodesize = 25,
                          keep.forest = TRUE)
plot(rf_model2)
rf_model3 <- randomForest(as.factor(trlabels)~.,
                          data = trdata,
                          ntree = 100,
                          nodesize = 25, 
                          keep.forest = TRUE)

plot(rf_model3)
model_list <- list(A = rf_model1,
                   B = rf_model2,
                   C = rf_model3)


set.seed(1234)

x1 <- runif(1000)
x2 <- runif(1000)
tedata <- cbind(x1, x2)

y <- as.numeric(x1<x2)
telabels <- as.factor(y)
tedata <- cbind(tedata, telabels)
tedata[,3] <- as.factor(tedata[,3])
# plot(x1, x2, col = (y+1))


# pred_1 <- predict(object = rf_model1, newdata = tedata, type = 'class')
# pred_2 <- predict(object = rf_model2, newdata = tedata, type = 'class')
# pred_3 <- predict(object = rf_model3, newdata = tedata, type = 'class')

missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

# missclass_1 <- missclass(tedata[,3], pred_1)
# missclass_2 <- missclass(tedata[,3], pred_2)
# missclass_3 <- missclass(tedata[,3], pred_3)


misclass_df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(misclass_df) <- c('Nr_Tree', 'Missclass_Rate')



for (i in 1:length(model_list)) {
  pred_vals <- c()
  misclass_rates <- c()
  pred_vals <- predict(object = model_list[[i]], newdata = tedata, type = 'class')
  misclass_rates <- missclass(tedata[,3], pred_vals)
  misclass_df <- rbind(misclass_df, data.frame('Nr_Tree' = model_list[[i]]$ntree,
                                               'Missclass_Rate' = misclass_rates))
}


list_df <- list()

for (i in 1:1000) {
  x1 <- runif(100)
  x2 <- runif(100)
  trdata <- as.data.frame(matrix(ncol = 3, nrow = 100))
  colnames(trdata) <- c('x1', 'x2', 'trlabels')
  trdata[,1] <- x1
  trdata[,2] <- x2
  y <- as.numeric(x1 < x2)
  trlabels <- as.factor(y)
  trdata[,3] <- trlabels
  list_df[[i]] <- (trdata)
}




tree_no <- c(1,10,100)
misclass_rates <- c()
missrates_1 <- c()
missrates_10 <- c()
missrates_100 <- c()
for (i in 1:length(tree_no)) {
  for (j in 1:length(list_df)) {
    rf_model <- randomForest(as.factor(trlabels)~., 
                             data = as.data.frame.list(list_df[[j]]),
                             ntree = tree_no[i],
                             nodesize = 25,
                             keep.forest = TRUE)
    pred_vals <- c()
    
    pred_vals <- predict(object = rf_model, newdata = tedata, type = 'class')
    misclass_rates <- missclass(tedata[,3], pred_vals)
    
    if (tree_no[i] == 1) {
      missrates_1 <- rbind(missrates_1, misclass_rates)
    }else if (tree_no[i] == 10) {
      missrates_10 <- rbind(missrates_10, misclass_rates)
    }
    else{
      missrates_100 <- rbind(missrates_100, misclass_rates)
    }
  }
}

new_misclass_df <- data.frame(Tree1_Misclass = missrates_1,
                              Tree10_Misclass = missrates_10,
                              Tree100_Misclass = missrates_100)

row.names(new_misclass_df) <- NULL

mean_tree1 <- mean(new_misclass_df[,1])
mean_tree2 <- mean(new_misclass_df[,2])
mean_tree3 <- mean(new_misclass_df[,3])

variance_tree1 <- var(new_misclass_df[,1])
variance_tree2 <- var(new_misclass_df[,2])
variance_tree3 <- var(new_misclass_df[,3])

# 2nd case

list_df_2 <- list()

for (i in 1:1000) {
  x1 <- runif(100)
  x2 <- runif(100)
  trdata_2 <- as.data.frame(matrix(ncol = 3, nrow = 100))
  colnames(trdata_2) <- c('x1', 'x2', 'trlabels')
  trdata_2[,1] <- x1
  trdata_2[,2] <- x2
  y <- as.numeric(x1 < 0.5)
  trlabels <- as.factor(y)
  trdata_2[,3] <- trlabels
  list_df_2[[i]] <- (trdata_2)
}

set.seed(9000)

x1 <- runif(1000)
x2 <- runif(1000)
tedata_2 <- cbind(x1, x2)

y <- as.numeric(x1<0.5)
telabels <- as.factor(y)
tedata_2 <- cbind(tedata_2, telabels)
tedata_2[,3] <- as.factor(tedata_2[,3])

misclass_rates_2 <- c()
missrates_1_case2 <- c()
missrates_10_case2 <- c()
missrates_100_case2 <- c()
for (i in 1:length(tree_no)) {
  for (j in 1:length(list_df_2)) {
    rf_model_2 <- randomForest(as.factor(trlabels)~., 
                             data = as.data.frame.list(list_df_2[[j]]),
                             ntree = tree_no[i],
                             nodesize = 25,
                             keep.forest = TRUE)
    pred_vals <- c()
    
    pred_vals <- predict(object = rf_model_2, newdata = tedata_2, type = 'class')
    misclass_rates_2 <- missclass(tedata_2[,3], pred_vals)
    
    if (tree_no[i] == 1) {
      missrates_1_case2 <- rbind(missrates_1_case2, misclass_rates_2)
    }else if (tree_no[i] == 10) {
      missrates_10_case2 <- rbind(missrates_10_case2, misclass_rates_2)
    }
    else{
      missrates_100_case2 <- rbind(missrates_100_case2, misclass_rates_2)
    }
  }
}

new_misclass_df_case2 <- data.frame(Tree1_Misclass = missrates_1_case2,
                              Tree10_Misclass = missrates_10_case2,
                              Tree100_Misclass = missrates_100_case2)

row.names(new_misclass_df_case2) <- NULL

mean_tree1_case2 <- mean(new_misclass_df_case2[,1])
mean_tree2_case2 <- mean(new_misclass_df_case2[,2])
mean_tree3_case2 <- mean(new_misclass_df_case2[,3])

variance_tree1_case2 <- var(new_misclass_df_case2[,1])
variance_tree2_case2 <- var(new_misclass_df_case2[,2])
variance_tree3_case2 <- var(new_misclass_df_case2[,3])

# 3rd case

list_df_3 <- list()

for (i in 1:1000) {
  x1 <- runif(100)
  x2 <- runif(100)
  trdata_3 <- as.data.frame(matrix(ncol = 3, nrow = 100))
  colnames(trdata_3) <- c('x1', 'x2', 'trlabels')
  trdata_3[,1] <- x1
  trdata_3[,2] <- x2
  y <- as.numeric((x1<0.5 & x2<0.5)
                  | (x1>0.5 & x2>0.5))
  trlabels <- as.factor(y)
  trdata_3[,3] <- trlabels
  list_df_3[[i]] <- (trdata_3)
}

set.seed(4567)

x1 <- runif(1000)
x2 <- runif(1000)
tedata_3 <- cbind(x1, x2)

y <- as.numeric((x1<0.5 & x2<0.5)
                 | (x1>0.5 & x2>0.5))
telabels <- as.factor(y)
tedata_3 <- cbind(tedata_3, telabels)
tedata_3[,3] <- as.factor(tedata_3[,3])

misclass_rates_3 <- c()
missrates_1_case3 <- c()
missrates_10_case3 <- c()
missrates_100_case3 <- c()
for (i in 1:length(tree_no)) {
  for (j in 1:length(list_df_3)) {
    rf_model_3 <- randomForest(as.factor(trlabels)~., 
                               data = as.data.frame.list(list_df_3[[j]]),
                               ntree = tree_no[i],
                               nodesize = 12,
                               keep.forest = TRUE)
    pred_vals <- c()
    
    pred_vals <- predict(object = rf_model_3, newdata = tedata_3, type = 'class')
    misclass_rates_3 <- missclass(tedata_3[,3], pred_vals)
    
    if (tree_no[i] == 1) {
      missrates_1_case3 <- rbind(missrates_1_case3, misclass_rates_3)
    }else if (tree_no[i] == 10) {
      missrates_10_case3 <- rbind(missrates_10_case3, misclass_rates_3)
    }
    else{
      missrates_100_case3 <- rbind(missrates_100_case3, misclass_rates_3)
    }
  }
}
plot(rf_model_3)
new_misclass_df_case3 <- data.frame(Tree1_Misclass = missrates_1_case3,
                                    Tree10_Misclass = missrates_10_case3,
                                    Tree100_Misclass = missrates_100_case3)

row.names(new_misclass_df_case3) <- NULL
plot(rf_model_3)
mean_tree1_case3 <- mean(new_misclass_df_case3[,1])
mean_tree2_case3 <- mean(new_misclass_df_case3[,2])
mean_tree3_case3 <- mean(new_misclass_df_case3[,3])

variance_tree1_case3 <- var(new_misclass_df_case3[,1])
variance_tree2_case3 <- var(new_misclass_df_case3[,2])
variance_tree3_case3 <- var(new_misclass_df_case3[,3])

mean_var_df <- data.frame(matrix(ncol = 6, nrow = 3))
colnames(mean_var_df) <- c('Mean_Case1',
                           'Mean_Case2',
                           'Mean_Case3',
                           'Var_Case1',
                           'Var_Case2',
                           'Var_Case3')
rownames(mean_var_df) <- c('1Tree',
                           '10Tree',
                           '100Tree')
mean_var_df[1,] <- c(mean_tree1, 
                     mean_tree1_case2, 
                     mean_tree1_case3, 
                     variance_tree1,
                     variance_tree1_case2,
                     variance_tree1_case3)
mean_var_df[2,] <- c(mean_tree2, 
                     mean_tree2_case2, 
                     mean_tree2_case3, 
                     variance_tree2,
                     variance_tree2_case2,
                     variance_tree2_case3)
mean_var_df[3,] <- c(mean_tree3, 
                       mean_tree3_case2, 
                       mean_tree3_case3, 
                       variance_tree3,
                       variance_tree3_case2,
                       variance_tree3_case3)
