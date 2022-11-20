library(ggplot2)
library(caret)
library(dplyr)
library(nnet)
set.seed(12345)
# 1
# reading the data from csv file

diabetes_dataset <- read.csv('pima-indians-diabetes.csv', header = FALSE)
names(diabetes_dataset) <- c('Number of times pregnant',
                             'Plasma glucose concentration a 2 hours in an oral 
                             glucose tolerance test',
                             'Diastolic blood pressure (mm Hg)',
                             'Triceps skinfold thickness (mm)',
                             '2-Hour serum insulin (mu U/ml)',
                             'Body mass index (weight in kg/(height in m)^2)',
                             'Diabetes pedigree function',
                             'Age (years)',
                             'Diabetes (0=no or 1=yes)')

# plot(x = diabetes_dataset[,2],
#      y = diabetes_dataset[,8],
#      type = 'p',
#      xlab = 'Plasma glucose concentration a 2 hours in an oral 
#      glucose tolerance test',
#      ylab = 'Age (years)')
p <- ggplot(data = diabetes_dataset, 
            aes(x = diabetes_dataset[,2],
                y = diabetes_dataset[,8]))+
  geom_point(aes(colour = diabetes_dataset[,9]))
p
# 2
n <- ncol(diabetes_dataset)
scale_data <- preProcess(diabetes_dataset[,1:(n-1)])
dataS <- predict(scale_data, diabetes_dataset)
log_model <- glm(formula = dataS[,9]~dataS[,2]+
                   dataS[,8], 
                 family = binomial,data = dataS)
diab_prob <- predict(log_model, type = 'response')
r <- 0.5
diab_pred <- ifelse(diab_prob > r, 1, 0)

table(dataS[,9], diab_pred)
summary(log_model)
# the probabilistic model P(diabetic = 0) = 
# 1/1+exp(-0.77962+(1.13963*Plasma glucose concentration)+(0.29140*Age))

missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

missclass(dataS[,9], diab_pred)
upd_dataset <- cbind(dataS, diab_pred)

p2 <- ggplot(data = upd_dataset, 
            aes(x = upd_dataset[,2],
                y = upd_dataset[,8]))+
  geom_point(aes(colour = upd_dataset[,10]))
p2

# The decision boundary equation for the trained model is:

slope <- coef(log_model)[2]/(-coef(log_model)[3])
intercept <- coef(log_model)[1]/(-coef(log_model)[3]) 

p3 <- ggplot(data = upd_dataset, 
             aes(x = upd_dataset[,2],
                 y = upd_dataset[,8]))+
  geom_point(aes(colour = upd_dataset[,10]))+
  geom_abline(slope = slope, intercept = intercept)
p3

# When r = 0.2
r <- 0.2
diab_pred_2 <- ifelse(diab_prob > r, 1, 0)

# table(dataS[,9], diab_pred_2)
# missclass2=function(X,X1){
#   n=length(X)
#   return(1-sum(diag(table(X,X1)))/n)
# }
# missclass2(dataS[,9], diab_pred_2)

upd_dataset_2 <- cbind(dataS, diab_pred_2)
p4 <- ggplot(data = upd_dataset_2, 
             aes(x = upd_dataset_2[,2],
                 y = upd_dataset_2[,8]))+
  geom_point(aes(colour = upd_dataset_2[,10]))
p4

# When r = 0.8
r <- 0.8
diab_pred_3 <- ifelse(diab_prob > r, 1, 0)

# table(dataS[,9], diab_pred_3)
# missclass3=function(X,X1){
#   n=length(X)
#   return(1-sum(diag(table(X,X1)))/n)
# }
# missclass3(dataS[,9], diab_pred_3)

upd_dataset_3 <- cbind(dataS, diab_pred_3)
p5 <- ggplot(data = upd_dataset_3, 
             aes(x = upd_dataset_3[,2],
                 y = upd_dataset_3[,8]))+
  geom_point(aes(colour = upd_dataset_3[,10]))
p5

# Basis function expansion

z1 <- diabetes_dataset[,2]^4
z2 <- (diabetes_dataset[,2]^3)*(diabetes_dataset[,8])
z3 <- (diabetes_dataset[,2]^2)*(diabetes_dataset[,8]^2)
z4 <- (diabetes_dataset[,2])*(diabetes_dataset[,8]^3)
z5 <- (diabetes_dataset[,8])^4

new_df <- diabetes_dataset[,c(2,8,9)]
new_df$z1 <- z1
new_df$z2 <- z2
new_df$z3 <- z3
new_df$z4 <- z4
new_df$z5 <- z5
updated_diab_dataset <- cbind(diabetes_dataset, z1, z2, z3, z4, z5)

updated_scale <- preProcess(new_df[,-3])
dataS_2 <- predict(updated_scale, new_df)

log_model_2 <- glm(formula = dataS_2[,3]~., 
                        data = dataS_2, family = binomial)
summary(log_model_2)
diab_prob_2 <- predict(log_model_2, type = 'response')
r_1 <- 0.5
diab_pred_4 <- ifelse(diab_prob_2 > r_1, 1, 0)

upd_dataset_4 <- cbind(dataS_2, diab_pred_4)
missclass4=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}
table(dataS_2[,3], diab_pred_4)
missclass4(dataS_2[,3], diab_pred_4)

# Decision Boundary for multiple features

slope_2 <- (coef(log_model_2)[2]/(-coef(log_model_2)[3]))
intercept_2 <- coef(log_model_2)[1]/(-coef(log_model_2)[3]) 

p6 <- ggplot(data = upd_dataset_4, 
             aes(x = upd_dataset_4[,1],
                 y = upd_dataset_4[,2]))+
  geom_point(aes(colour = upd_dataset_4[,9]))+
  geom_abline(slope = slope_2, intercept = intercept_2)
p6
coef(log_model_2)

# Question 1.
# Which feature do we base the probabilistic model on? What determines it?

# Question 2. 
# How do we plot a decision boundary when there are more than one feature?
# What should be the x and y axis of the scatterplot?

