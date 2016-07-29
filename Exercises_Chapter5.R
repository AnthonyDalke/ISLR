# 5.)
library("ISLR")
Default = Default
# a.)
set.seed(1)
fit_log = glm(default ~ income + balance, data = Default, family = "binomial")
# b.)
training = sample(10000, 5000)
fit_log_train = glm(default ~ income + balance, data = Default, subset = training, family = "binomial")
fit_log_pred = predict(fit_log_train, Default[-training,])
fit_log_pred[which(fit_log_pred > 0.5)] = "Yes"
fit_log_pred[which(fit_log_pred <= 0.5)] = "No"
fit_log_table = table(fit_log_pred, Default$default[-training])
(fit_log_table[[2]] + fit_log_table[[3]]) / sum(fit_log_table)
# Error of 2.84%
# c.)
# Repeated above code with new samples three times; trial 1: 2.88% error, trial 2: 2.72%, trial 3: 2.50%
# Results appear similar, which speaks to stability of model
# d.)
Default$student = gsub("No", 0, Default$student)
Default$student = gsub("Yes", 1, Default$student)
Default$student = as.numeric(Default$student)
fit_log2_train = glm(default ~ income + balance + student, data = Default, subset = training, 
                     family = "binomial")
fit_log2_pred = predict(fit_log2_train, Default[-training,])
fit_log2_pred[which(fit_log2_pred > 0.5)] = "Yes"
fit_log2_pred[which(fit_log2_pred <= 0.5)] = "No"
fit_log2_table = table(fit_log2_pred, Default$default[-training])
(fit_log2_table[[2]] + fit_log2_table[[3]]) / sum(fit_log2_table)
# No, the student dummy variable did not significantly reduce the test error rate.

# 6.)
set.seed(1)
# a.) summary(fit_log)
# income std. error = 0.000004985; balance std. error = 0.0002274
# b.) 
boot.fn = function(data, training_rows){
  boot.model = glm(default ~ income + balance, data = data, subset = training_rows, family = "binomial")
  boot.model$coefficients
}
# c.)
install.packages("boot")
library("boot")
boot(Default, boot.fn, 50)
# d.) The std. errors came very close: 0.000004653632 and 0.0002043368, respectively.