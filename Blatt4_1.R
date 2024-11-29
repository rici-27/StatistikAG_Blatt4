library(glmnet)
library(ISLR2)
library(tidyverse)


## Aufgabe a)

str(Credit)
View(Credit)


## Aufgabe b)

# Design Matrix erstellen
income <- Credit$Income
limit <- Credit$Limit
rating <- Credit$Rating

income <- (income - mean(income))/sd(income)
limit <- (limit - mean(limit))/sd(limit)
rating <- (rating - mean(rating))/sd(rating)

X = cbind(income, limit, rating)

# Ziel Variable
y <- Credit$Balance


grid <- rev(10^seq(4, -2, length=10000))
grid

ridge <- glmnet(X, y, alpha = 0, intercept = TRUE, lambda = grid)
koeffizienten_rre <- coef(ridge)
koeffizienten_rre[,3]

# kommt hier überhaupt alles an ergebnissen raus?
# oder wird abgeschnitten?
# mal datentypen anschauen in R!!


intercept_para_rre <- koeffizienten_rre[1,]
income_para_rre <- koeffizienten_rre[2,]
limit_para_rre <- koeffizienten_rre[3,]
rating_para_rre <- koeffizienten_rre[4,]

ggplot() + 
  geom_point(aes(x=grid, y=intercept_para, color = "Intercept"), show.legend = TRUE) +
  geom_point(aes(x=grid, y=income_para, color = "Income"), show.legend = TRUE) +
  geom_point(aes(x=grid, y=limit_para, color = "Limit"), show.legend = TRUE) +
  geom_point(aes(x=grid, y=rating_para, color = "Rating"), show.legend = TRUE) +
  labs(
    x = "lambda-Werte",
    y = "Parameter",
    title = "Ridge Regression"
  ) + 
  scale_color_manual(
    values = c("Intercept" = "blue", "Income" = "red", "Limit" = "green", "Rating" = "yellow"),
    name = "Parameters"
  ) + 
  theme_classic()

lasso <- glmnet(X, y, alpha = 1, intercept = TRUE, lambda = grid)
koeffizienten_lasso <- coef(lasso)

intercept_para_lasso <- koeffizienten_lasso[1,]
income_para_lasso <- koeffizienten_lasso[2,]
limit_para_lasso <- koeffizienten_lasso[3,]
rating_para_lasso <- koeffizienten_lasso[4,]

ggplot() + 
  geom_point(aes(x=grid, y=intercept_para, color = "Intercept"), show.legend = TRUE) +
  geom_point(aes(x=grid, y=income_para, color = "Income"), show.legend = TRUE) +
  geom_point(aes(x=grid, y=limit_para, color = "Limit"), show.legend = TRUE) +
  geom_point(aes(x=grid, y=rating_para, color = "Rating"), show.legend = TRUE) +
  labs(
    x = "lambda-Werte",
    y = "Parameter",
    title = "Lasso Regression"
  ) + 
  scale_color_manual(
    values = c("Intercept" = "blue", "Income" = "red", "Limit" = "green", "Rating" = "yellow"),
    name = "Parameters"
  ) + 
  theme_classic()

ridge_L2_norms <- apply(koeffizienten_rre, 2, function(x) sqrt(sum(x^2)))
lasso_L2_norms <- apply(koeffizienten_lasso, 2, function(x) sqrt(sum(x^2)))

ridge_L2_norms <- as.vector(as.array(ridge_L2_norms))
lasso_L2_norms <- as.vector(as.array(lasso_L2_norms))

ratio = ridge_L2_norms/lasso_L2_norms

ggplot() + 
  geom_point(aes(x=grid, y=ratio), color = "blue") +
  labs(
    x = "Ratio",
    y = "Parameter",
    title = "Ratio of Ridge and Lasso Estimates"
  ) + 
  theme_classic()



