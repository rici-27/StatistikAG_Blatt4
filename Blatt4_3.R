library(glmnet)
library(ISLR2)
library(tidyverse)


grid <- (0:100)/10

random_decomposition <- function(lambda){
  sample_size <- 200
  train_indices <- sample(400, size = sample_size)
  train_data <- Credit[train_indices, ]
  test_data <- Credit[-train_indices, ]
  
  y <- Credit$Balance[train_indices]
  
  design <- model.matrix(~ Income + Limit + Rating + Cards + Age 
          + Education + Own + Student + Married + Region -1, train_data)
  X <- design
  fit_ridge <- glmnet(X, y, alpha=1, lambda = lambda, intercept = TRUE)
  
  test <- model.matrix(~ Income + Limit + Rating + Cards + Age 
        + Education + Own + Student + Married + Region -1, test_data)
  vorhersagen <- predict(fit_ridge, test, s="lambda.min")
  
  L2_Fehler <- 0
  for (l in (1:200)){
    L2_Fehler <- L2_Fehler + (vorhersagen[l] - test_data$Balance[l])^2
  }
  L2_Fehler <- sqrt( (1/200) * L2_Fehler)
  return(L2_Fehler)
}

L2_Fehler_nach_lambda <- array(0, dim = 101)
for (i in (1:101)){
  print(i)
  L2_Fehler <- 0
  for (k in (1:1000)){
    L2_Fehler <- L2_Fehler + random_decomposition(grid[i])
  }
  L2_Fehler_nach_lambda[i] <- sqrt( (1/1000) * L2_Fehler)
}


temp <- 0
for (i in (1:1000)){
  temp <- temp + random_decomposition(0)
}
L2_Fehler_LS <- sqrt( (1/1000) * temp)


ggplot() + 
  geom_point(aes(x=grid, y = L2_Fehler_nach_lambda), size = 2) +
  geom_point(aes(x=0, y=L2_Fehler_LS), color = "red", size = 2)

## simply fitting the model with just an intercept?
