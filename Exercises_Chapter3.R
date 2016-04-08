# 1.) All the predictor variables except NEWSPAPER have statistically significant effects on the model.
# In other words, holding the other variables constant, each variable helps explain the relationship between
# the predictor and dependent variables.

# 2.) The k-NN classifier examines the labels, or classifications, of each neighbor
# within "k" distance of each test data set.  It then takes a "majority vote" and assigns
# the "winning" label.  In contrast, k-NN regression averages the numerical values of
# the neighbors within "k" distance of each test data set.

# 3.) a.) The interaction variable between GPA and gender makes the difference here: for a high enough female
# GPA, the negative coefficient pushes the predicted salary below that of a male, so "iii".
# b.)
50 + (20 * 4) + (0.07 * 110) + 35 + (0.01 * 110 * 4) - (10 * 4)
# c.) False - the deciding piece of information concerns the variable's p-value, not simply its coefficient.

# 4.) a.) Because the data set has a linear relationship, the linear model should have a lower RSS
# b.) Since higher-dimension models have a tendency to overfit on training data, the linear model should
# have a lower RSS
# c.) In this case, the cubic model should have a lower RSS, because its higher dimensions should enable
# it to capture the non-linear relationship in the data
# d.) Again, since higher-dimension models tend to overfit, the linear model should have a lower RSS

# 5.) a-prime reflects the product of fitted x values and the accompanying point's derivative (slope)

# 6.) Since linear regression aims to minimize the distance between fitted values and x-bar and y-bar,
# it will strive to come as close as possible to (x-bar, y-bar).

# 7.) Both the linear regression and correlational coefficient formulas depend on the distance between
# fitted values and the averages of x and y.

# 8.)
# a.) 
auto = Auto
auto_reg = lm(mpg ~ horsepower, data = auto)
summary(auto_reg)
# i.) A negative relationship between horsepower and mpg exists.
# ii.) Horsepower has a very large t value (and, consequently, a small p value), so a large relationship
# iii.)  Again, the relationship appears negative.
# iv.) 
coef(auto_reg)[1] + (coef(auto_reg)[2] * 98)
confint(auto_reg)
# b.)
plot(auto$horsepower, auto$mpg)
abline(auto_reg)
# c.)
plot(auto_reg)
# Residuals appear to form a non-normal distribution (grow as predictor variable grows)

# 9.)
# a.)
plot(auto)
# b.)
cor(subset(auto, select = -name))
# c.)
auto_mult_reg = lm(mpg ~ .-name, data = auto)
summary(auto_mult_reg)
# i.) Yes, the overall model has a very large F-statistic and small p-value, along with a large adjusted R^2
# ii.) Displacement, Weight, Year, and Origin all have small enough p-values
# iii.) As a car's year becomes more recent, its mpg improves
# d.)
plot(auto_mult_reg)
# Outliers do tend to grow along with fitted values, but no "usually large" outliers appear.  If anything,
# observations 326, 327, and 323 come closest to this criteria.
# e.)
auto_int_reg = lm(mpg ~ horsepower:weight + cylinders*displacement + weight:acceleration, data = auto)
summary(auto_int_reg)
# Interactions between horsepower and weight and cylinders and displacement appear statistically significant.
# f.)
auto_tran_reg = lm(mpg ~ sqrt(weight) + (acceleration^2) + log(horsepower), data = auto)
summary(auto_tran_reg)
summary(auto_mult_reg)
# Taking the sqrt of weight makes it less statistically significant as a predictor.  Squaring acceleration
# actually makes it statistically significant. And taking the log of horsepower makes it stat. sig.