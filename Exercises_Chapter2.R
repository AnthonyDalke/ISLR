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