# 10.) 
library("ISLR")
weekly = Weekly
# a.)
summary(weekly)
str(weekly)
plot(weekly)
# Volume appears to have surged in more recent years. However, no other clear patterns seem to exist.

# b.)
fit1 = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = weekly, family = binomial)
summary(fit1)
# Only Lag2 emerges as statistically significant

# c.)
fit1_probs = predict(fit1)
fit1_pred = rep("Down", length(fit1_probs))
fit1_pred[fit1_probs > 0.5] = "Up"
table(fit1_pred, weekly$Direction)
# The logistic regression produced too many "Down" predictions, harming the accuracy.

# d.)
fit2_data = weekly[weekly$Year < 2009,]
fit2 = glm(Direction ~ Lag2, data = fit2_data, family = binomial)
fit2_test = weekly[weekly$Year > 2008,]
fit2_probs = predict(fit2, fit2_test, type = "response")
fit2_pred = rep("Down", length(fit2_probs))
fit2_pred[fit2_probs > 0.5] = "Up"
fit2_truth = weekly[weekly$Year > 2008,9]
fit2_confusion = table(fit2_pred, fit2_truth)
(fit2_confusion[[1]] + fit2_confusion [[4]]) / sum(fit2_confusion)
fit2_confusion

# e.)
install.packages("MASS")
library("MASS")
fit3 = lda(Direction ~ Lag2, data = fit2_data)
plot(fit3)
