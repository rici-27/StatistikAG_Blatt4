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

# hier nochmal least squares anschauen
# beta wird einfach an der income stelle rescaled

ridge1 <- glmnet(X1, y, alpha=0, lambda=100, Intercept=TRUE)
lasso1 <- glmnet(X1, y, alpha=1, lambda=100, Intercept=TRUE)

ridge2 <- glmnet(X2, y, alpha=0, lambda=100, Intercept=TRUE)
lasso2 <- glmnet(X2, y, alpha=1, lambda=100, Intercept=TRUE)

ergebnis <- cbind(coef(ridge1), coef(lasso1), coef(ridge2), coef(lasso2))
colnames(ergebnis) <- c("Ridge", "Lasso", "Ridge skal", "Lasso skal")
View(as.matrix(ergebnis))

# wieso ändert sich bei rre nichts? beta_1 flexibler im anderen term 


## Aufgabe d)

design <- model.matrix(~ Income + Limit + Rating + Cards + Age + Education + Own + Student + Married + Region -1, Credit)
#View(design)
X <- design

fit_ridge <- cv.glmnet(X, y, alpha=0, intercept = TRUE)
fit_lasso <- cv.glmnet(X, y, alpha=1, intercept = TRUE)
fit_ls <- glmnet(X, y, alpha=0, lambda=0, intercept =TRUE)

daten = model.matrix(~ Income + Limit + Rating + Cards + Age + Education + Own + Student + Married + Region -1, Credit[400,])
#View(daten)
daten <- as.array(daten)
# 400ten rausschmeißen ?
daten <- as.matrix(daten)

predict(fit_ridge, daten, s="lambda.min")
predict(fit_lasso, daten, s="lambda.min")
predict(fit_ls, daten, s="lambda.min")

# wieso performed ridge nicht ?

?predict
?model.matrix
