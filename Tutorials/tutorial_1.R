# Data reading/writing
# Excercise 1
birth <- read.csv(file = 'birthstatistics.csv')

# Excercise 2
blog <- read.csv(file = 'blogData_test.csv', header = FALSE)

# Excercise 3

tecator <- readxl::read_xls('tecator.xls')
class(tecator)
# Excercise 4

tecator_csv <- write.csv(tecator, file = 'tecator_1.csv', row.names = FALSE)

# Basic data manipulation
# Excercise 1
tecator1 <- as.data.frame(tecator)

# Excercise 2 
row.names(tecator1) <- tecator1[,'Sample']+10

# Excercise 3
colnames(tecator1)[1] <- 'ID'

# Excercise 4

tecator_2 <- tecator1[tecator1$Channel1 > 3 & tecator1$Channel2 > 3,5:8]

# Excercise 5
tecator1 <- tecator1[,-1]

# Excercise 6

index <- stringr::str_which(colnames(tecator1), 'Channel')
channel_df <- tecator1[,index]

means <- colMeans(channel_df)
tecator1[,index] <- tecator1[,index]/matrix(means, ncol = length(means), nrow = length(means), byrow = TRUE)

# Excercise 7

sum_matrix <- matrix(apply(tecator1[1:5,],MARGIN = 1, function(x){return(sum(x^2))}))

# Excercise 8

X <- as.matrix(tecator1[,1:100])
y <- as.matrix(tecator1[,101])
solve(t(X) %*% X, t(X) %*% y)

# Excercise 9

tecator1$ChannelIX <- as.factor(ifelse(tecator1$Channel1 > 1,'high', 'low'))

# Excercise 10
Intercepts <- numeric(100)
for (i in 1:length(Intercepts)) {
  fit <- lm(formula = paste('Fat~Channel',i,sep = ''), data = tecator1)
  Intercepts[i] <- coef(fit)[1]
}
Intercepts

# Excercise 11

x <- c(1,3)
y <- 5*x+11

plot(x,y, type = 'l')

# Data manipulation: dplyr and tidyr

# Excercise 1
birth1 <- tibble::as_tibble(birth)

# Excercise 2
library(dplyr)
birth2 <- birth1 %>% select(X2002:X2020)

# Excercise 3

birth1 <- birth1 %>%
  mutate(.data = birth1, Status = 
           ifelse(birth1$foreign.Swedish.background == 'born in Sweden with two parents born in Sweden', 'Yes', 'No'))

# Excercise 4

birth1 %>% count(sex, region)

# Excercise 5

birth3 <- birth1 %>% 
  select(-sex, -foreign.Swedish.background) %>%
  group_by(region, Status) %>%
  summarise_all(sum) %>%
  ungroup()
# Excercise 6

birth4 <- birth3 %>%
  group_by(region) %>%
  mutate(Percentage = X2002/sum(X2002)*100) %>%
  filter(Status == 'Yes') %>%
  select(region, Percentage) %>%
  ungroup() %>%
  arrange(Percentage)

# Excercise 7
library(tidyr)
birth5 <- birth1 %>%
  group_by(region, sex, foreign.Swedish.background, Status) %>%
  pivot_longer(X2002:X2020, names_to = 'Year', values_to = 'Born') %>%
  mutate(Year=as.numeric(stringr::str_remove(Year, "X")))

# Excercise 8

birth6 <- birth5 %>% 
  pivot_wider(names_from = 'Year', values_from = 'Born', names_prefix = 'Y_')

# Excercise 9

blog5 <- tibble(blog) %>%
  select_if(function(x) !all(x==0))

# Probability, statistics and machine learning

# Excercise 1
set.seed(123)
y <- birth[sample(nrow(birth), 5, replace = FALSE),]

# Excercise 2

rnorm(n = 5, mean = 5, sd = .1)

# Excercise 3

rnorm(n = 168, mean = birth$X2002, sd = .1)

# Excercise 4

dnorm(c(-1,0,1))

# Excercise 5

loglik <- function(w, w0){
  Probs=-(birth$X2003-w*birth$X2002-w0)
  return(-sum(Probs))
}
loglik(1,1)

# Excercise 6

birth$X2020 = w*birth$X2002 + w1*birth$X2003 + w2*birth$X2004 + w0

fit9 <- lm(birth$X2020~birth$X2002 + birth$X2003 + birth$X2004, data = birth)
summary(fit9)
preds <- predict(fit9)
mse <- mean((birth$X2020 - preds)^2)

# Excercise 7

install.packages('fastDummies')
dummy <- fastDummies::dummy_columns(birth, select_columns = c('region', 'sex'))

# Excercise 8

index_train <- birth[1:50, -(1:3)]
index_test <- birth[51:100, -(1:3)]

scaler <- caret::preProcess(index_train)
trainS <- predict(scaler, index_train)
testS <- predict(scaler, index_test)

# Excercise 9


log_reg <- glm(formula = as.factor(birth$sex)~birth$X2002+birth$X2003+birth$X2004, family = 'binomial')
prob <- predict(log_reg, type = 'response')

pred_1 <- ifelse(prob > 0.5, 'girls', 'boys')
table(birth$sex, pred_1)
summary(log_reg)

# Excercise 10

library(kknn)
train=birth%>%select(X2002:X2010, sex)
knnfit <- kknn(formula = as.factor(birth$sex)~., train = train, test = train, k = 10, kernel = 'rectangular')
pred <- knnfit$fitted.values
table(train$sex, pred)

# Excercise 11

df <- data.frame(x1 = 1, x2 = 2, x3 = 3)

mylikelihood <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  return((x1-df$x1)^2+(x2-df$x2)^2+(x3-df$x3)^2)
}
res <- optim(c(0,0,0), fn =mylikelihood, method="BFGS")
res$par
