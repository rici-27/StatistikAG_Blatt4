library(glmnet)
library(ISLR2)
library(tidyverse)


## Aufgabe a)

#str(Credit)
View(Credit)
# plots erstellen :-)

## Aufgabe b)

# Design Matrix erstellen
income <- Credit$Income
limit <- Credit$Limit
rating <- Credit$Rating

income <- income/sd(income)
limit <- limit/sd(limit)
rating <- rating/sd(rating)

# hier auch mean abziehen?

X = cbind(income, limit, rating)

# Ziel Variable
y <- Credit$Balance

grid <- rev(10^seq(4, -2, length=10000))

ridge <- glmnet(X, y, alpha = 0, intercept = TRUE, lambda = grid)
View(ridge)
koeffizienten_rre <- coef(ridge)

rre_intercept <- koeffizienten_rre[1,]
rre_income <- koeffizienten_rre[2,]
rre_limit <- koeffizienten_rre[3,]
rre_rating <- koeffizienten_rre[4,]

ggplot() + 
  geom_point(aes(x=grid, y=rev(rre_intercept), color = "Intercept"), show.legend = TRUE) +
  geom_point(aes(x=grid, y=rev(rre_income), color = "Income"), show.legend = TRUE) +
  geom_point(aes(x=grid, y=rev(rre_limit), color = "Limit"), show.legend = TRUE) +
  geom_point(aes(x=grid, y=rev(rre_rating), color = "Rating"), show.legend = TRUE) +
  labs(
    x = "lambda-Werte",
    y = "Parameter",
    title = "Ridge Regression"
  ) + 
  scale_color_manual(
    values = c("Intercept" = "blue", "Income" = "red", "Limit" = "green", "Rating" = "yellow"),
    name = "Parameters"
  )  +
  scale_x_continuous(trans='log10') +
  theme_classic()

lasso <- glmnet(X, y, alpha = 1, intercept = TRUE, lambda = grid)
koeffizienten_lasso <- coef(lasso)

lasso_intercept <- koeffizienten_lasso[1,]
lasso_income <- koeffizienten_lasso[2,]
lasso_limit <- koeffizienten_lasso[3,]
lasso_rating <- koeffizienten_lasso[4,]

# vorsicht glmnet dreht lambdas um!

ggplot() + 
  geom_point(aes(x=grid, y=rev(lasso_intercept), color = "Intercept"), show.legend = TRUE) +
  geom_point(aes(x=grid, y=rev(lasso_income), color = "Income"), show.legend = TRUE) +
  geom_point(aes(x=grid, y=rev(lasso_limit), color = "Limit"), show.legend = TRUE) +
  geom_point(aes(x=grid, y=rev(lasso_rating), color = "Rating"), show.legend = TRUE) +
  labs(
    x = "lambda-Werte",
    y = "Parameter",
    title = "Lasso Regression"
  ) + 
  scale_color_manual(
    values = c("Intercept" = "blue", "Income" = "red", "Limit" = "green", "Rating" = "yellow"),
    name = "Parameters"
  ) + 
  scale_x_continuous(trans='log10') +
  theme_classic()


## L2-Norm Ratio of Ridge and Least Squares Estimator

leastsquares <- glmnet(X, y, alpha = 1, intercept = TRUE, lambda = 0)
koeffizienten_ls <- coef(leastsquares)
koeffizienten_ls
# vorsicht mit lm kommen andere werte raus
# glmnet für einzelne lambda schlecht?
ls_norm <- sqrt(sum(koeffizienten_ls^2))

ridge_L2_norms <- apply(koeffizienten_rre, 2, function(x) sqrt(sum(x^2)))
ridge_L2_norms <- as.array(ridge_L2_norms)
View(ridge)

ratio_ls <- ridge_L2_norms / ls_norm
ggplot() + 
  geom_point(aes(x=grid[1:10000], y=rev(ratio_ls[1:10000])), color = "blue") +
  labs(
    x = "Parameter",
    y = "Ratio",
    title = "Ratio of Ridge and Least Squares Estimates"
  ) + 
  theme_classic() +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits = c(0.2, 1.25))

## ohne intercept

ridge_L2_norms_woi <- apply(koeffizienten_rre[2:4,], 2, function(x) sqrt(sum(x^2)))
ridge_L2_norms_woi <- as.array(ridge_L2_norms_woi)

ls_norm_woi <- sqrt(sum(koeffizienten_ls[2:4]^2))

ratio_ls_woi <- ridge_L2_norms_woi / ls_norm_woi
ggplot() + 
  geom_point(aes(x=grid[1:10000], y=rev(ratio_ls_woi[1:10000])), color = "blue") +
  labs(
    x = "Parameter",
    y = "Ratio",
    title = "Ratio of Ridge and Least Squares Estimates Without Intercept"
  ) + 
  theme_classic() +
  #scale_y_continuous(limits = c(0.2, 1.25)) +
  scale_x_continuous(trans='log10') 

