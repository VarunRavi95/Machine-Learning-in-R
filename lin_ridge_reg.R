parkinson_df <- read.csv('parkinsons.csv')
parkinson_df <- parkinson_df[,-c(1,2,3,4,6)]
n <- dim(parkinson_df)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.6))
train_park_df <- parkinson_df[id,]
# train_park_df <- train_park_df[,-c(train_park_df$subject.,
#                                    train_park_df$age,
#                                    train_park_df$sex,
#                                    train_park_df$test_time,
#                                    train_park_df$total_UPDRS)]
test_park_df <- parkinson_df[-id,]

scaler <- caret::preProcess(train_park_df)
train_park_s_df <- predict(scaler, train_park_df)
test_park_s_df <- predict(scaler, test_park_df)

lm_fit <- lm(formula = train_park_s_df$motor_UPDRS~-1+.,
             data = train_park_s_df)
summary(lm_fit)

lm_fit_test <- predict(lm_fit, test_park_s_df)

train_mse <- mean(lm_fit$residuals^2)

mse_test <- mean((lm_fit_test - test_park_s_df$motor_UPDRS)^2)
coef_df <- data.frame(lm_fit$coefficients)
x <- c(rep(0,17))


x_i <- matrix(as.matrix(train_park_s_df[,-1]), ncol = 16, nrow = nrow(train_park_s_df))
theta <- matrix(x[1:16], ncol = 1, nrow = 16)
x_i %*% theta

likelihood <- function(x){
  theta <- matrix(x[1:16], ncol = 1, nrow = 16)
  sigma <- x[17]
  x_i <- matrix(as.matrix(train_park_s_df[,-1]), ncol = 16, nrow = nrow(train_park_s_df))
  return(((-nrow(train_park_s_df)/2)*log(2*pi*(sigma^2)))-((1/(2*(sigma^2))) * sum((x_i %*% theta - as.matrix(train_park_s_df[,1]))^2)))
}
likelihood(c(rep(1,17)))
y <- optim(c(rep(1,17)), fn = likelihood, method = 'BFGS')
y$par[-17]

ridge <- function(theta,...){
  # y <- optim(c(rep(1,17)), fn = likelihood, method = 'BFGS')
  ridge_val <- -likelihood(theta) + (... * (sum(theta^2)))
  return(ridge_val)
}

ridgeopt <- function(lambda){
  return(optim(c(rep(1,17)),lambda = lambda, fn = ridge, method = 'BFGS'))
}
ridgeopt(5)

df <- function(lambda){
  opt_val_fun <- ridgeopt(lambda)
  sigma_2 <- opt_val_fun$par[17]^2
  theta_fun <- opt_val_fun$par[-17]
  y_hat <- x_i %*% theta_fun
  y <- train_park_s_df[,1]
  df_val <- 0
  for (i in 1:nrow(y_hat)) {
    hat_mat <- x %% solve( t(x)%%x + idiag(ncol(x)) ) %% t(x)
  }
  return(df_val*(1/sigma_2))
}
df(10000)
# for lambda = 1 train
opt_val <- ridgeopt(1)
par_lam_1 <- opt_val$par[-17]

x_tr <- matrix(as.matrix(train_park_s_df[,-1]), ncol = 16, nrow = nrow(train_park_s_df))
x_ts <- matrix(as.matrix(test_park_s_df[,-1]), ncol = 16, nrow = nrow(test_park_s_df))
pred_train <- x_tr %*% par_lam_1
pred_test_1 <- x_ts %*% par_lam_1

mse_train_ridge <- mean((pred_train - train_park_s_df$motor_UPDRS)^2)
mse_test_ridge <- mean((pred_test_1 - test_park_s_df$motor_UPDRS)^2)

# for lambda = 100 train
opt_val_100 <- ridgeopt(100)
par_lam_100 <- opt_val_100$par[-17]
pred_train_100 <- x_tr %*% par_lam_100
pred_test_100 <- x_ts %*% par_lam_100
mse_train_ridge_100 <- mean((pred_train_100 - train_park_s_df$motor_UPDRS)^2)
mse_test_ridge_100 <- mean((pred_test_100 - test_park_s_df$motor_UPDRS)^2)
# for lambda = 1000 train

opt_val_1000 <- ridgeopt(1000)
par_lam_1000 <- opt_val_1000$par[-17]
pred_train_1000 <- x_tr %*% par_lam_1000
pred_test_1000 <- x_ts %*% par_lam_1000
mse_train_ridge_1000 <- mean((pred_train_1000 - train_park_s_df$motor_UPDRS)^2)
mse_test_ridge_1000 <- mean((pred_test_1000 - test_park_s_df$motor_UPDRS)^2)

DF <- function(lambdas){
  #Slide 22 - has the hat matrix for Ridge Regression
  #To compute the df based on training data
  x <- as.matrix(train_park_s_df[-1])
  y <- as.matrix(train_park_s_df[,1])
  dim(x)
  degfree_df <- data.frame(matrix(ncol=2, nrow = 0))
  colnames(degfree_df) <- c('lambda', 'df')
  for (i in lambdas) {
    hat_mat <- x %*% solve( t(x)%*%x + i*diag(ncol(x)) ) %*% t(x)
    degfree_df <- rbind(degfree_df,
                        data.frame(lambda = i, df = sum(diag(hat_mat)))
    )
  }
  return(degfree_df)
} 

DF(1:10)
