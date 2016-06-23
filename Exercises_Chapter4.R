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
fit3_pred = predict(fit3, fit2_test, type = "response")[[1]]
fit3_truth = weekly[weekly$Year > 2008,9]
fit3_confusion = table(fit3_pred, fit3_truth)
(fit3_confusion[[1]] + fit3_confusion [[4]]) / sum(fit3_confusion)
fit3_confusion

# f.)
install.packages("MASS")
library("MASS")
fit4 = qda(Direction ~ Lag2, data = fit2_data)
fit4_pred = predict(fit4, fit2_test, type = "response")[[1]]
fit4_truth = weekly[weekly$Year > 2008,9]
fit4_confusion = table(fit4_pred, fit4_truth)
fit4_confusion
mean(fit4_pred == fit4_truth)

# g.)
install.packages("class")
library("class")
knn_train = as.matrix(fit2_data[,3])
knn_test = as.matrix(fit2_test[,3])
knn_direction = fit2_data$Direction
set.seed(1)
fit5 = knn(knn_train, knn_test, knn_direction)
table(fit5, fit4_truth)
mean(fit5 == fit4_truth)

# h.) Logistic regression and LDA both produce accuracy rates of 62.5%.

#11.)
# a.) 
Auto = Auto
mpg_data = data.frame(Auto, mpg01 = 1)
mpg_data$mpg01[mpg_data$mpg < median(mpg_data$mpg)] = 0

# b.) Horsepower, weight, and displacement have negative relationships with mpg01, while no other variables
# appear to have a reliable relationship with it.
plot(mpg_data)

# c.)
training_rows = sample(length(mpg_data$mpg01)*.667)
mpg_training = mpg_data[training_rows,]
mpg_test = mpg_data[-training_rows,]

# d.)
fit_LDA = lda(mpg01 ~ horsepower + weight + displacement, data = mpg_test)
pred_LDA = predict(fit_LDA, mpg_test, type = "response")[[1]]
truth_LDA = mpg_test$mpg01
confusion_LDA = table(pred_LDA, truth_LDA)
1-((confusion_LDA[[1]] + confusion_LDA [[4]]) / sum(confusion_LDA))
# Error rate = 9.92%

# e.)
fit_QDA = qda(mpg01 ~ horsepower + weight + displacement, data = mpg_test)
pred_QDA = predict(fit_QDA, mpg_test, type = "response")[[1]]
truth_QDA = mpg_test$mpg01
confusion_QDA = table(pred_QDA, truth_QDA)
1-((confusion_QDA[[1]] + confusion_QDA [[4]]) / sum(confusion_QDA))
# Error rate = 10.69%

# f.)
fit_logistic = glm(mpg01 ~ horsepower + weight + displacement, data = mpg_test, family = binomial)
pred_logistic = predict(fit_logistic, mpg_test, type = "response")[[1]]
truth_logistic = mpg_test$mpg01
confusion_logistic = table(pred_QDA, truth_logistic)
1-((confusion_logistic[[1]] + confusion_logistic [[4]]) / sum(confusion_logistic))
# Error rate = 10.69%

# g.)
set.seed(1)
knn_training = mpg_training[,3:5]
knn_test = mpg_test[,3:5]
knn_direction = mpg_training$mpg01
fit_knn = knn(knn_training, knn_test, knn_direction, k = 5)
confusion_knn = table(fit_knn, truth_logistic)
1-((confusion_knn[[1]] + confusion_knn [[4]]) / sum(confusion_knn))
# Error rate = 19.08% with k = 5