# Data-Mining-Unsupervised-Learning-R
Generalized Association Rules - Modeling using Apriori , Decision Tree 

The entire code be executed in RSTudio or with the help of R console.

This project discusses about how we can use decision trees to convert a unsupervised problem to a supervised learning problem.

Basically it performs two steps:

1)Generate a random distribution from the original dataset, the intention behind this random data generation is yo break the correlation between the variables 
  and make them totally independent.
2)We label the samples from the true distribution as 1 and those from the random distributions as 0, basically we are mapping a unsupervised 
  to supervised learning problem by generating labels for them
3)Then we combine these two distributions and model a decision tree on the combined dataset.
4)We let the model decide which samples are getting extracted from the tru distribiution and which are from random distribution.
5)We have use rpart package here to develop the decision tree.
6)The beauty of using decision trees in such unsupervised learning is that with the help of summary statistics of the rpart package we can easily calculate support,
lift and confidence of an item set.
7)In addition to the above steps wwe separately used Apriori Algorithms on the dataset which performs automatic calculation of support,confidence and lift
8)We compared the results from both the strategies used above, though the result was comparable using decision tree gives a better model interpretation.

The overall goal of this project because unsupervised learning problems are hard to deal with ,so we can frame them as a supervised learning problem by using the above techniques.

