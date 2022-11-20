library(MASS)
library(dplyr)
library(tree)
library(broom)
library(glmnet)
# Excercise 1
crabs1 <- crabs
as.data.frame(crabs1)
log_model <- glm(sex~RW+CL+CW,
                 family = binomial, 
                 data = crabs1)

crab_prob <- predict(log_model, type = 'response')
r <- 0.5
crab_pred <- ifelse(crab_prob > r, 'M', 'F')
table(crabs1$sex, crab_pred)
# approach2
crabs2 <- crabs1 %>%
  select(sex, RW:CW)
levels(crabs1$sex)
log_model2 <- glm(sex~., family = binomial, data = crabs2)
ProbM <- predict(log_model2, type = 'response')
ProbF <- 1 - ProbM
pred <- ifelse(ProbF/ProbM > 3/2, 'F', 'M')
table(crabs2$sex, pred)

# Excercise 2

crabs3 <- crabs1 %>%
  select(sex, FL:BD)

dec_tree <- tree(formula = as.factor(sex)~., data = crabs3)
plot(dec_tree)
summary(dec_tree)
# dec_tree$y
fit <- predict(dec_tree, newdata = crabs3, type = 'class')
table(crabs3$sex, fit)
dec_tree_2 <- tree(formula = as.factor(sex)~., 
                   data = crabs3,
                   control = tree.control(nrow(crabs3),
                                          minsize = 40))
summary(dec_tree_2)
plot(dec_tree_2)

probs <- (predict(dec_tree, crabs3) + predict(dec_tree_2, crabs3))/2
best <- apply(probs, MARGIN = 1, FUN = which.max)
pred <- levels(crabs3$sex)[best]
table(crabs3$sex, pred)

# Excercise 4

fullT <- tree(as.factor(sex)~.,data = crabs3)
probs <- predict(fullT, crabs3)
table(crabs3$sex, probs[,2]/probs[,1] > 0.7)

decision <- ifelse(probs[,2]/probs[,1] > 0.7, 'M', 'F')
tab <- table(crabs3$sex, decision)
TP <- tab[1,1]
TN <- tab[2,2]
FP <- tab[2,1]
FN <- tab[1,2]

TPR <- TP/(TP+FN)
FPR <- FP/(FP+TN)

precision <- TP/(TP+FP)
summary(fullT)
print(fullT)

result <- cv.tree(object = fullT)
result_new <- cv.tree(object = fullT)
leaves_opt_1 <- result_new$size[which.min(result_new$dev)]

opt_tree <- prune.tree(tree = fullT, best = leaves_opt_1)
plot(opt_tree)
text(opt_tree)

crab_pca <- crabs1 %>% 
  select(FL:BD)
res_pca <- prcomp(x = crab_pca)
screeplot(res_pca)
res_pca$x

plot(res_pca$x[,1], res_pca$x[,2], col = c('red', 'green'))

res_pca_2 <- princomp(x = crab_pca)
res_pca_2$loadings[,1]

rownames(res_pca_2$loadings)[which.max(res_pca_2$loadings[,1])]
res_pca_2$scores

compressed_dat <- res_pca_2$scores[,1:5]%*%t(res_pca_2$loadings[,1:5])+
  matrix(colMeans(crab_pca), nrow = nrow(crab_pca), ncol = ncol(crab_pca), byrow = T)
View(crabs1)
crab_lasso <- crabs1 %>%
  select(CW, c(FL,RW,CL,BD))
View(crab_lasso)

lass_mod <- glmnet(x = crab_lasso[,c(2:5)],
                   y = crab_lasso[,1], 
                   alpha = 1, 
                   lambda =1,
                   family = 'gaussian')
coef(lass_mod)
# CW = 5.61 + 0.959*CL
crab_lasso_1 <- crabs1 %>%
  select(FL:BD)
x <- as.matrix(crab_lasso_1 %>% select(-CW))
y <- as.matrix(crab_lasso_1 %>% select(CW))
lass_mod_cv <- cv.glmnet(x = x,
                         y = y,
                         alpha = 1,
                         family = 'gaussian')
lass_mod_cv$lambda.min

test_err <- list()
train_err <- list()
k = 0

mse_train <- function(w){
  MSEtrain <- ((w - 1)^2) + ((w-2)^2)
  MSEtest <- ((w-4)^2) + ((w+1)^2)
  # .GlobalEnv$k <- .GlobalEnv$k + 1
  # .GlobalEnv$train_err[[k]] <- MSEtrain
  # .GlobalEnv$test_err[[k]] <- MSEtest
  return(MSEtrain)
}
optim(c(0), fn = mse_train, method = 'BFGS')

plot(as.numeric(train_err), type = 'l', ylim = c(0,60), col = 'blue')
points(as.numeric(test_err), type = 'l', col = 'red')
