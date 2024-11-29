library(glmnet)
library(ISLR2)
library(tidyverse)


## Aufgabe c)

# Design Matrix erstellen
income <- Credit$Income
limit <- Credit$Limit
rating <- Credit$Rating

X1 = cbind(income, limit, rating)
X2 = cbind(1000 * income, limit, rating)


# Ziel Variable
y <- Credit$Balance

ridge1 <- glmnet(X1, y, alpha=0, lambda=100, Intercept=TRUE)
lasso1 <- glmnet(X1, y, alpha=1, lambda=100, Intercept=TRUE)

ridge2 <- glmnet(X2, y, alpha=0, lambda=100, Intercept=TRUE)
lasso2 <- glmnet(X2, y, alpha=1, lambda=100, Intercept=TRUE)

ergebnis <- cbind(coef(ridge1), coef(lasso1), coef(ridge2), coef(lasso2))
View(ergebnis)
as.matrix(ergebnis)

# ridge wurde beeinflusst aber lasso nicht, bei rre nur intercept und limit

?model.matrix
