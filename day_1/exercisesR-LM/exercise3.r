# Regression coefficients: solution of normal equations for simple linear regression

limepH <- data.frame(rate=c(0,1,2,4,8),
                     pH=c(5.71,5.75,5.84,6.16,6.47))


regr.object <- lm(pH ~ rate, data=limepH)
coef(regr.object)  # least squares estimates
summary(regr.object)
anova(regr.object)

# Plot data with fitted regression line
plot(pH ~ rate, data=limepH, main="pH vs lime amount", cex=1.5, col="red", pch=16)
abline(regr.object)


# Easy vector and matrix manipulation in R:
X <- model.matrix(regr.object)  # X-matrix
y <- limepH$pH

(XtX <- t(X) %*% X)
(Xty <- t(X) %*% y)
(XtXi <- solve(t(X) %*% X))
(b <- XtXi%*%Xty)


F <- predict(regr.object)
plot(pH ~ rate, data=limepH, main="pH vs lime amount", cex=1, col="red", pch=16)
abline(regr.object, lwd=1.5, col="red")
for (i in 1:5) with (limepH, arrows(rate[i], pH[i],rate[i], F[i], length=0.08, lwd=2, code=3, col="red"))
title(paste("\n \n sum of squares of residuals=",round(sum(residuals(regr.object)^2),5)), cex.main=0.9)

options(digits=4)
confint(regr.object)
