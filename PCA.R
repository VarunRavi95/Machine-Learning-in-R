library(caret)
comm_dataset <- read.csv(file = 'communities.csv', header = TRUE)

scaler <- preProcess(comm_dataset[,c(1:100)])

comm_datasetS <- predict(object = scaler, comm_dataset)

eigen_pca <- eigen(cov(comm_datasetS[,-101]))

cumul_sum_eig <- cumsum(eigen_pca$values)

pc_req <- sum(cumul_sum_eig < 95)

prop_var_2 <- eigen_pca$values[1]+eigen_pca$values[2]

pca_proc <- princomp(x = comm_datasetS[,-101])
plot(pca_proc$loadings[,1], type ='l')

head(sort(abs(pca_proc$loadings[,1]), decreasing = TRUE),5)

pc_12_df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(pc_12_df) <- c('PC1', 'PC2', 'VCPop')

pc_12_df <- rbind(pc_12_df, data.frame('PC1' = pca_proc$scores[,1],
                                       'PC2' = pca_proc$scores[,2],
                                       'VCPop' = comm_dataset[,ncol(comm_dataset)]))

pc_plot <- ggplot(data = pc_12_df)+
  geom_point(aes(x = PC1, y = PC2,  colour = pc_12_df$VCPop))


set.seed(12345)
n <- nrow(comm_dataset)
id <- sample(1:n, floor(n*0.5))
comm_dataset_train <- comm_dataset[id,]
comm_dataset_test <- comm_dataset[-id,]

scaler2 <- preProcess(comm_dataset_train)
comm_dataset_trainS <- predict(object = scaler2, comm_dataset_train)
comm_dataset_testS <- predict(object = scaler2, comm_dataset_test)

lm_fit <- lm(formula = ViolentCrimesPerPop~., data = comm_dataset_trainS)

vals_pred_test <- predict(object = lm_fit, newdata = comm_dataset_testS)

mse_test <- mean((comm_dataset_testS$ViolentCrimesPerPop - vals_pred_test)^2)
mse_train <- mean((comm_dataset_trainS$ViolentCrimesPerPop - lm_fit$fitted.values)^2)

trainErr <- list()
testErr <- list()
k <- 0

cost_func <- function(params){
  param_matrix <- matrix(params[1:100], ncol = 1, nrow = 100)
  newTrain <- model.matrix(ViolentCrimesPerPop~.-1, comm_dataset_trainS)
  newTest <- model.matrix(ViolentCrimesPerPop~.-1, comm_dataset_testS)
  new_mse_train <- mean((newTrain %*% param_matrix - comm_dataset_trainS$ViolentCrimesPerPop)^2)
  new_mse_test <- mean((newTest %*% param_matrix - comm_dataset_testS$ViolentCrimesPerPop)^2)
  .GlobalEnv$k= .GlobalEnv$k+1
  .GlobalEnv$trainErr[[k]]=new_mse_train
  .GlobalEnv$testErr[[k]]=new_mse_test
  return(new_mse_train)
}

res <- optim(c(rep(0,100)), fn = cost_func, method = 'BFGS')


testErr[[which.min(testErr)]]

plot(unlist(trainErr[500:length(trainErr)]), ylim = c(0,1), type = 'l', col = 'red')
lines(unlist(testErr[500:length(testErr)]), type = 'l', col = 'blue')
abline(v = 2182-500)
