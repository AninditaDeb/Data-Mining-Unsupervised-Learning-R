######################Task1#########################################################################################
##########################Visualizing the data #####################################################################
data(MovieLense)
head(MovieLense)
## look at the first few ratings of the first user##
head(as(MovieLense[1,], "list")[[1]])

## visualize part of the matrix#####################
image(MovieLense[1:100,1:100])
## number of ratings per user#######################
hist(rowCounts(MovieLense))
## number of ratings per movie######################
hist(colCounts(MovieLense))
## mean rating (averaged over users)################
mean(rowMeans(MovieLense))
## available user meta information##################
head(MovieLenseUser)


dim(getRatingMatrix(MovieLense))
getRatingMatrix(MovieLense)[1:10, 1:10]

#####################Normalizing the matrix to get a better rating representation########################
MovieLense_Normalize <- normalize(MovieLense)
getRatingMatrix(MovieLense_Normalize)[1:10, 1:10]
######Visualize raw ratings and normalized ratings#######################################################

x11()
image(MovieLense_Normalize[1:100,1:100], main = "Normalized ratings")

x11()
image(MovieLense_Normalize[1:100, 1:100], main = "Raw Ratings")
MovieLense_denormalize <- denormalize(MovieLense_Normalize)
###################Creating a binary matrix #############################################################

MovieLense_binarize <- binarize(MovieLense_denormalize, minRating = 4)
getRatingMatrix(MovieLense_binarize)[1:10, 1:10]
image(MovieLense_binarize[1:100,1:100], main = "Binarized ratings")
#######################Histogram plot of normalized ratings##############################################
## number of normalized ratings per user#######################
x11()
hist(rowCounts(MovieLense_Normalize),main="Count of normalized ratings per user")
## number of normalized ratings per movie######################
x11()
hist(colCounts(MovieLense_Normalize),main="Count of normalized ratings per movie")
################Splitting data into train and test###################################################
dim(R)
set.seed(1)
MovieLense_sample <- sample(MovieLense,4000,replace=TRUE)
dim(MovieLense_sample)
eval<- evaluationScheme(MovieLense_sample,method = "cross-validation", k=10,given = -1,goodRating=4)
eval

##############################Building a recommender model using user based collaborative filtering######################################################################
recommender_user_based <- Recommender(getData(eval,"train"), "UBCF")
names(getModel(recommender_user_based))
############Predict the missing ratings#########################################################################################
recommender_user_based.predict<- predict(recommender_user_based, getData(eval, "known"), type="ratings")
ERROR<- rbind(UBCF = calcPredictionAccuracy(recommender_user_based.predict, getData(eval,"unknown")))
################## evaluating the top 1, 3, 5, 10,15,20 recommendation lists using user based collaborative filtering###########
results<- evaluate(eval, method = "UBCF", type="topNList", n=c(1,3,5,10,15,20))
########################Plotting the ROC curve################################################################################
x11()
plot(results,annotate=TRUE,main="ROC curve")
graphics.off()
x11()
plot(results, "prec/rec", annotate=TRUE,main="Prec/Rec curve")
graphics.off()
######################Create top 10 recommendations for 3 users##################################################
recom <- predict(recommender_user_based, MovieLense[800:802], n=10)
