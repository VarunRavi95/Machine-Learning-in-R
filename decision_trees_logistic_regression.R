library(dplyr)
library(tree)
library(rpart)
library(rpart.plot)
bank_dataset <- read.csv2('bank-full.csv', header = TRUE)

# factor_job <- factor(bank_dataset$job, levels = c('admin',
#                                     'blue-collar',
#                                     'entrepreneur',
#                                     'housemaid',
#                                     'management',
#                                     'retired',
#                                     'self-employed',
#                                     'services',
#                                     'student',
#                                     'technician',
#                                     'unemployed',
#                                     'unknown'))
# unique(bank_dataset$day)
# bank_dataset[sapply(bank_dataset, is.character)] <- lapply(bank_dataset[sapply(bank_dataset, is.character)],as.factor)

# removing duration column
bank_dataset_new <- bank_dataset[,-12]

bank_dataset_new <- bank_dataset_new%>%
  mutate_if(is.character, as.factor)
# is.factor(bank_dataset$job)



# Hold-out - partitioning into train valid and test

n <- dim(bank_dataset_new)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.4))

bank_train <- bank_dataset_new[id,]

id1 <- setdiff(1:n, id)
set.seed(12345)
id2 <- sample(id1, floor(n*0.3))

bank_valid <- bank_dataset_new[id2,]

id3 <- setdiff(id1, id2)

bank_test <- bank_dataset_new[id3,]

# 2a.

default_fit <- tree(y~., data = bank_train)

# default_fit
# plot(default_fit)
# text(default_fit)
# summary(default_fit)

# 2b 

tree_fit_2 <- tree(y~., 
                   data = bank_train, 
                   control = tree.control(nobs = nrow(bank_train),
                                          minsize = 7000), 
                   split = c('deviance', 'gini'))
# tree_fit_2
# plot(tree_fit_2)
# text(tree_fit_2)
# summary(tree_fit_2)

# 2c

tree_fit_3 <- tree(y~., 
                   data = bank_train, 
                   control = tree.control(nobs = nrow(bank_train),
                                          mindev = 0.0005), 
                   split = c('deviance', 'gini'))
# tree_fit_3
# plot(tree_fit_3)
# text(tree_fit_3)
# summary(tree_fit_3)

# Predicting for model 1
# prob_tree_train <- predict(object = default_fit,
#                            newdata = bank_train)
# prob_tree_valid <- predict(object = default_fit,
#                        newdata = bank_valid)
# 
# bestI_train <- apply(prob_tree_train, MARGIN = 1, which.max)
# bestI_valid <- apply(prob_tree_valid, MARGIN = 1, which.max)
# 
# pred_tree_train <- levels(bank_train$y)[bestI_train]
# pred_tree_valid <- levels(bank_valid$y)[bestI_valid]

# table(bank_train$y, pred_tree_train)
# table(bank_valid$y, pred_tree_valid)
# 
# missclass=function(X,X1){
#   n=length(X)
#   return(1-sum(diag(table(X,X1)))/n)
# }
# 
# training_misclass_error <- missclass(bank_train$y, pred_tree_train)
# validation_misclass_error <- missclass(bank_valid$y, pred_tree_valid)
# 
# # Predicting for model 2
# 
# prob_tree_train_2 <- predict(object = tree_fit_2,
#                            newdata = bank_train)
# prob_tree_valid_2 <- predict(object = tree_fit_2,
#                            newdata = bank_valid)
# 
# bestI_train_2 <- apply(prob_tree_train_2, MARGIN = 1, which.max)
# bestI_valid_2 <- apply(prob_tree_valid_2, MARGIN = 1, which.max)
# 
# pred_tree_train_2 <- levels(bank_train$y)[bestI_train_2]
# pred_tree_valid_2 <- levels(bank_valid$y)[bestI_valid_2]
# 
# table(bank_train$y, pred_tree_train_2)
# table(bank_valid$y, pred_tree_valid_2)

missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

# training_misclass_error_2 <- missclass(bank_train$y, pred_tree_train_2)
# validation_misclass_error_2 <- missclass(bank_valid$y, pred_tree_valid_2)

# Predicting for model 3

# prob_tree_train_3 <- predict(object = tree_fit_3,
#                              newdata = bank_train)
# prob_tree_valid_3 <- predict(object = tree_fit_3,
#                              newdata = bank_valid)
# 
# bestI_train_3 <- apply(prob_tree_train_3, MARGIN = 1, which.max)
# bestI_valid_3 <- apply(prob_tree_valid_3, MARGIN = 1, which.max)
# 
# pred_tree_train_3 <- levels(bank_train$y)[bestI_train_3]
# pred_tree_valid_3 <- levels(bank_valid$y)[bestI_valid_3]
# 
# table(bank_train$y, pred_tree_train_3)
# table(bank_valid$y, pred_tree_valid_3)
# 
# 
# training_misclass_error_3 <- missclass(bank_train$y, pred_tree_train_3)
# validation_misclass_error_3 <- missclass(bank_valid$y, pred_tree_valid_3)

misclass_df <- data.frame(matrix(ncol = 3, nrow = 0))
names(misclass_df) <- c('Model', 'DataSet', 'MisclassRate')

calc_misclass_error <- function(model, data_set){
  
  prob_tree_calc <- predict(object = model,
                       newdata = data_set)
  bestI_calc <- apply(prob_tree_calc, MARGIN = 1, which.max)
  pred_tree_calc <- levels(data_set$y)[bestI_calc]
  data_set_misclass_err <- missclass(data_set$y, pred_tree_calc)
  
  misclass_df <<- rbind(misclass_df, data.frame('Model' = as.character(substitute(model)),
                                               'DataSet' = as.character(substitute(data_set)),
                                               'MisclassRate' = data_set_misclass_err))
  return(misclass_df)
}

calc_misclass_error(model = default_fit, data_set = bank_train)
calc_misclass_error(model = default_fit, data_set = bank_valid)
calc_misclass_error(model = tree_fit_2, data_set = bank_train)
calc_misclass_error(model = tree_fit_2, data_set = bank_valid)
calc_misclass_error(model = tree_fit_3, data_set = bank_train)
calc_misclass_error(model = tree_fit_3, data_set = bank_valid)

# 3

opt_res <- cv.tree(object = tree_fit_3)
plot(opt_res$size, opt_res$dev, type = 'b', col = 'red')

trainScore <- rep(0,52)
validScore <- rep(0,52)
prune_df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(prune_df) <- c('Leaves', 'Training Deviance', 'Validation Deviance')
for (i in 2:52) {
  opt_tree_pruned <- prune.tree(tree_fit_3, best = i)
  pred_valid <- predict(opt_tree_pruned, 
                        newdata = bank_valid, 
                        type = 'tree')
  trainScore[i] <- deviance(opt_tree_pruned)
  validScore[i] <- deviance(pred_valid)
  prune_df <- rbind(prune_df, data.frame('Leaves' = i,
                                         'Training Deviance' = trainScore[i],
                                         'Validation Deviance' = validScore[i]))
}

plot(2:52, prune_df[,2], type = 'b', col = 'red', ylim = c(8000, 12000))
points(2:52, prune_df[,3], type = 'b', col = 'blue')

optimal_leaves <- prune_df[which.min(prune_df$Validation.Deviance),1]

opt_tree_new <- prune.tree(tree = tree_fit_3,
                           best = optimal_leaves)
plot(opt_tree_new)
text(opt_tree_new)
opt_pred_valid <- predict(opt_tree_new, 
                          newdata = bank_valid, 
                          type = 'class')
calc_misclass_error(model = opt_tree_new,
                    data_set = bank_valid)

opt_pred_test <- predict(object = opt_tree_new,
                         newdata = bank_test,
                         type = 'class')

calc_misclass_error(model = opt_tree_new,
                    data_set = bank_test)
confuson_matrix_test <- table(bank_test$y, opt_pred_test)

TN <- confuson_matrix_test[1,1]
TP <- confuson_matrix_test[2,2]
FP <- confuson_matrix_test[1,2]
FN <- confuson_matrix_test[2,1]
N <- TN + FP
P <- TP + FN

accuracy_test <- (TP+TN)/(P+N)

recall_test <- TP/(TP+FN)
precision_test <- TP/(TP+FP)
f1_score <- (2*precision_test*recall_test)/(recall_test+precision_test)

