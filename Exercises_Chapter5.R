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
# Repeated above code with new samples three times; trial 1: 2.88% error, trial 2: 2.72%, trial 3: 2.50%
# Results appear similar, which speaks to stability of model