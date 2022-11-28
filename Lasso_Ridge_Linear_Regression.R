library(dplyr)
library(glmnet)
tecator_data <- read.csv(file = 'tecator.csv')

n <- dim(tecator_data)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.5))
tecator_tr <- tecator_data[id,]
tecator_ts <- tecator_data[-id,]

tecator_tr_new <- tecator_tr %>%
  select(-c(Sample,Protein, Moisture))

tecator_ts_new <- tecator_ts %>%
  select(-c(Sample,Protein, Moisture))

lm_fit <- lm(formula = Fat~., 
             data = tecator_tr_new)

mse_train <- mean((tecator_tr_new$Fat - lm_fit$fitted.values)^2)

pred_test <- predict.lm(object = lm_fit, newdata = tecator_ts_new)

mse_test <- mean((tecator_ts_new$Fat - pred_test)^2)

covariates <- tecator_tr_new[,1:100]
response <- tecator_tr_new[,101]
lasso_fit <- glmnet(x = as.matrix(covariates),
                    y = response, 
                    alpha = 1)
coef_df <- as.data.frame(matrix(coef(lasso_fit)))

plot(lasso_fit, xvar = 'lambda')
abline(v = -0.25)

log(0.853)

ridge_fit <- glmnet(x = as.matrix(covariates),
                    y = response,
                    alpha = 0,
                    family = 'gaussian')
plot(ridge_fit, xvar = 'lambda')

cv_lasso_fit <- cv.glmnet(x = as.matrix(covariates),
                       y = response, 
                       alpha = 1)
plot(cv_lasso_fit)
cv_lasso_fit$lambda.min
coef(cv_lasso_fit, s="lambda.min")
