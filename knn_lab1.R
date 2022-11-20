library(kknn)
library(tidyr)
digits_dataset <- read.csv('optdigits.csv', header = FALSE)

n <- dim(digits_dataset)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.5))
train_digits_dataset <- digits_dataset[id,]

id1 <- dplyr::setdiff(1:n, id)
set.seed(12345)
id2 <- sample(id1, floor(n*0.25))
valid_digits_dataset <- digits_dataset[id2,]

id3 <- setdiff(id1, id2)
set.seed(12345)
test_digits_dataset <- digits_dataset[id3,]

knn_model <- kknn(formula = as.factor(V65)~.,
                  train = train_digits_dataset, 
                  test = test_digits_dataset,
                  k = 30, 
                  kernel = 'rectangular')
knn_model_2 <- kknn(formula = as.factor(train_digits_dataset$V65)~.,
                    train = train_digits_dataset, 
                    test = train_digits_dataset,
                    k = 30, 
                    kernel = 'rectangular')

pred_value <- knn_model$fitted.values
pred_value_2 <- knn_model_2$fitted.values


table(test_digits_dataset$V65, pred_value)
table(train_digits_dataset$V65, pred_value_2)

missclass_knn <- function(X,X1){
  n <- length(X)
  return(1-sum(diag(table(X,X1)))/n)
}
# missclass_knn()
missclass_knn(test_digits_dataset$V65, pred_value)
missclass_knn(train_digits_dataset$V65, pred_value_2)

# train_digits_dataset$pred_values <- pred_value_2
prob_values_2 <- knn_model_2$prob

train_prob <- train_digits_dataset
train_prob$pred <- pred_value_2
View(train_prob)
train_prob$prob <- prob_values_2[,9]
train_prob <- dplyr::filter(train_prob, train_prob$V65 == 8)
train_prob <- train_prob[order(train_prob$prob),]



new_df_ez_1 <- train_prob[nrow(train_prob)-2,]
as.list.data.frame(new_df_ez_1)
new_df_ez_long <- matrix(as.numeric(new_df_ez_1[,c(1:64)]), ncol = 8, byrow = TRUE)

heatmap(x = new_df_ez_long, Rowv = NA, Colv = NA)

new_df_ez_2 <- train_prob[nrow(train_prob)-1,]
as.list.data.frame(new_df_ez_2)
new_df_ez_long_2 <- matrix(as.numeric(new_df_ez_2[,c(1:64)]), ncol = 8, byrow = TRUE)
heatmap(x = new_df_ez_long_2, Rowv = NA, Colv = NA)

new_df_hard_1 <- train_prob[1,]
as.list.data.frame(new_df_hard_1)
new_df_hard_long_1 <- matrix(as.numeric(new_df_hard_1[,c(1:64)]), ncol = 8, byrow = TRUE)
heatmap(x = new_df_hard_long_1, Rowv = NA, Colv = NA)

new_df_hard_2 <- train_prob[2,]
as.list.data.frame(new_df_hard_2)
new_df_hard_long_2 <- matrix(as.numeric(new_df_hard_2[,c(1:64)]), ncol = 8, byrow = TRUE)
heatmap(x = new_df_hard_long_2, Rowv = NA, Colv = NA)

new_df_hard_3 <- train_prob[3,]
as.list.data.frame(new_df_hard_3)
new_df_hard_long_3 <- matrix(as.numeric(new_df_hard_3[,c(1:64)]), ncol = 8, byrow = TRUE)
heatmap(x = new_df_hard_long_3, Rowv = NA, Colv = NA)

k <- 1:30
valid_misclass_error <- c()
training_misclass_error <- c()
# cross_entropy <- c()
for (i  in k) {
  knn_model_3 <- kknn(formula = as.factor(train_digits_dataset$V65)~.,
                      train = train_digits_dataset, 
                      test = valid_digits_dataset, 
                      k = i,
                      kernel = 'rectangular')
  knn_model_4 <- kknn(formula = as.factor(train_digits_dataset$V65)~.,
                      train = train_digits_dataset, 
                      test = train_digits_dataset, 
                      k = i,
                      kernel = 'rectangular')
  pred_value_3 <- knn_model_3$fitted.values
  valid_misclass_error <- c(valid_misclass_error,missclass_knn(valid_digits_dataset$V65, pred_value_3))
  pred_value_4 <- knn_model_4$fitted.values
  training_misclass_error <- c(training_misclass_error, missclass_knn(train_digits_dataset$V65, pred_value_4))
  
}
valid_misclass_error
training_misclass_error
plot(k, valid_misclass_error, type = 'l', ylim = c(0,0.055))
lines(k, training_misclass_error, type = 'l', col = 'red')

opt_k <- 3

knn_model_5 <- kknn(formula = as.factor(train_digits_dataset$V65)~.,
                    train = train_digits_dataset, 
                    test = test_digits_dataset, 
                    k = opt_k,
                    kernel = 'rectangular')
pred_value_test_opt <- knn_model_5$fitted.values
missclass_knn(test_digits_dataset$V65, pred_value_test_opt)
# prob_values_3 <- knn_model_5$prob

entropy<-function(p){
  for(i in 1:length(p)){
    return(-(sum(log(p[i]))))
  }
}

copy_df <- valid_digits_dataset
k<-1:30

cross_entropy_error<-c()
for(i in 1:length(k)){
  m2<-kknn(as.factor(train_digits_dataset$V65)~., train_digits_dataset,valid_digits_dataset, k=i,kernel="rectangular")
  pred_valid<-m2$fitted.values
  p<-LDATS::softmax(as.numeric(pred_valid))
  p_c_v <- c()
  # cross_entropy_error<-c(cross_entropy_error,entropy(p))
  for (j in as.numeric(colnames(m2$prob))) {
    copy_df$soft <- p
    p_c <- dplyr::filter(copy_df, V65 == j)
    p_c_v <- c(p_c_v,sum(p_c$soft))
  }
  # v_1<--mean(valid$X0.26log(as.numeric(m2$fitted.values))+(1-valid$X0.26)log(1-as.numeric(m2$fitted.values)))
  cross_entropy_error<-c(cross_entropy_error,entropy(p_c_v))
}
cross_entropy_error
plot(k,cross_entropy_error,type='l')

p
# 8 = 0.212
# 9 = 0.54
# 0 = 0.00038
# 1 = 0.0067
# 2 = 0.0075
# 3 = 0.0084
# 4 = 0.036
# 5 = 0.071
# 6 = 0.029
# 7 = 0.082

p_c <- c(0.212, 0.54, 0.00038, 0.0067, 0.0075, 0.0084, 0.036, 0.071, 0.029, 0.082)
sum(p_c)
entropy(p_c)
probvalz <- 
y <- model.matrix(~0+., data = valid_d)
View(knn_model_5$prob)
entr <- 0
sum <- 0
for (i in 1:ncol(knn_model_3$prob)) {
  
  for (j in 1:nrow(knn_model_3$prob)) {
    sum <- sum + (y[j,]*log((knn_model_3$prob[j,i])+(10^-15)))
  }
}
sum

sum(1,2)

sum(log(knn_model_3$prob[,1]+(10^-15)))+
  sum(log(knn_model_3$prob[,2]+(10^-15)))+
  sum(log(knn_model_3$prob[,3]+(10^-15)))+
  sum(log(knn_model_3$prob[,4]+(10^-15)))+
  sum(log(knn_model_3$prob[,5]+(10^-15)))+
  sum(log(knn_model_3$prob[,6]+(10^-15)))+
  sum(log(knn_model_3$prob[,7]+(10^-15)))+
  sum(log(knn_model_3$prob[,8]+(10^-15)))+
  sum(log(knn_model_3$prob[,9]+(10^-15)))+
  sum(log(knn_model_3$prob[,10]+(10^-15)))

sum(log(c(1,2,3)))
log(3)

y_1 <- model.matrix(~0+as.factor(valid_digits_dataset$V65), data = valid_digits_dataset)

k<-1:30
entropy<-c()
for(i in 1:length(k)){
  m_e<-kknn(as.factor(train_digits_dataset$V65)~., train_digits_dataset,valid_digits_dataset, k=i,kernel="rectangular")
  prob<-m_e$prob
  log_prb <- log(prob+10^-15)
  entropy <-c(entropy,-sum(y_1 * log_prb)) 
}
entropy
plot(k,entropy,type='l')

