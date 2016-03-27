### Chapter 2 Exercises ###

#1.a.) Better - With the small p, the risk of overfitting is relatively small,
        #and the large sample size will allow a flexible model to more accurately
        #approximate the training data.
#1.b.) Worse - The small n and large p risk overfitting with a flexible model
#1.c.) Better - A more flexible model will capture the non-linear nature of this data
#1.d.) Worse - A flexible model might overfit the high-variance data

#2.a.) n = 500, p = profit, number of employees, industry; regression; inference
#2.b.) n = 20, p = price, marketing budget, competition price, 10 other variables; 
      #classification; prediction
#2.c.) n = 52, p = % US market change, % British market change, 
      #% German market change; regression; prediction

#3.a.) Visual exercise
#3.b.) Bayes (irreducible) error remains 1 for all flexibility levels, because its
      #irreducible nature prevents it from changing; training errors fall as
      #flexibility increases since the more flexible models become, the more they
      #"meld" to fit training data; in contrast, test errors initially decrease but
      #then increase because less flexible (linear) models fail to accurately fit 
      #new data, yet highly flexibile models tend to over-fit on training data,
      #rendering them incapable of accurately fitting new test data; bias decreases
      #with flexibility because more flexible models more closely fit with training
      #data; in contrast, variance increases with flexibility for the same reason

#4.Skipping due to general nature

#5.) More flexible models can better capture the uniqueness of non-linear
    #data, reducing training error, but they run the risk of overfitting

#6.) Parametric approaches yield more flexible models by considering more variables
    #(or parameters).  This can come in handy when building multiple regression
    #models or classification models with multiple predictor variables.  However,
    #this creates risks similar to those of flexible models - namely, overfitting.
    #(FROM ANSWER KEY: parametric models do not require as much training data as
    #non-parametric.)

#7.) a.) 1 - 3, 2 - 2, 3 - 4, 4 - 3, 5 - 0, 6 - 3 (WRONG)
    #b.) Green, since observation 5 has the lowest distance from {0, 0, 0}
    #c.) Red, since the three closest observations have {Red, Green, Red}
    #d.) As the size of k increases, the decision boundary becomes more linear.
    #Therefore, for a non-linear decision boundary, a smaller k would prove more
    #flexible and accurate.

install.packages("ISLR")
library("ISLR")

#8.) a.)
college = College
#b.)
fix(college)
#c.)
summary(college)
pairs(college[,1:10])
plot(college$Outstate ~ college$Private)
elite = rep("No", nrow(college))
elite[college$Top10perc > 50] = " Yes"
elite = as.factor(elite)
college = data.frame(college, elite)
summary(college$elite)
plot(college$Outstate ~ college$elite)
hist(college$Room.Board, breaks = par(mfrow = c(2, 2)))

#9.) a.)
auto = Auto
