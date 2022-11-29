library(dplyr)
library(glmnet)
library(ggplot2)
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

plot(lasso_fit, xvar = 'lambda', label = TRUE)
abline(v = log(0.7082))

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
opt_lamdba <- cv_lasso_fit$lambda.min
coef(cv_lasso_fit, s="lambda.min")

lasso_fit_opt <- glmnet(x = as.matrix(covariates),
                        y = response, 
                        alpha = 1,
                        lambda = opt_lamdba)

covariates_x <- tecator_ts_new[,1:100]
pred_test <- predict(object = lasso_fit_opt, 
                     newx = as.matrix(covariates_x))
colnames(pred_test) <- NULL
mse_test_opt <- mean((tecator_ts_new$Fat - pred_test)^2)

y_pred_df <- as.data.frame(matrix(ncol = 2, nrow = 0))
colnames(y_pred_df) <- c('Original_Vals', 'Pred_Vals')

y_pred_df <- rbind(y_pred_df, data.frame('Original_Vals' = tecator_ts_new$Fat,
                                         'Pred_Vals' = pred_test))
p_scatter <- ggplot(y_pred_df)+
  geom_point(aes(x = Original_Vals, y = Pred_Vals))

plot(x = y_pred_df$Original_Vals, y = y_pred_df$Pred_Vals, type = 'p', col = c('red','blue'))
