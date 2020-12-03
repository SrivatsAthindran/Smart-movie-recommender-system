install.packages('dplyr')
library(dplyr)

# Data load and EDA
setwd('F:/Visual Analytics and Applications/Project/movielens-ratings-reviews/movielens-ratings-reviews/small-version-dataset')
movies = read.csv('movies.csv')
ratings = read.csv('ratings.csv')
head(movies)
head(ratings)
data=  merge(ratings,movies,by="movieId")
head(data)
nrow(ratings)
nrow(data)
data$rating = data$rating[data$rating > 0]
attach(data)
library('tidyverse')
qplot(rating) + ggtitle("Distribution of Ratings")
df = data.frame(userId, title, rating)
head(df,10)

library('recommenderlab')
rec_data = as(df, "realRatingMatrix")
rec_data
class(rec_data)
nrow(rec_data)
ncol(rec_data)

average_ratings<-colMeans(rec_data)
qplot(average_ratings) + stat_bin(binwidth = 0.1) + ggtitle("Distribution of average movie rating")

# So, for a relevant average rating distribution, let us remove movies which have been rated by less than 50 users
average_ratings_relevant<- average_ratings[colCounts(rec_data)>50]
qplot(average_ratings_relevant) + stat_bin(binwidth = 0.1) + ggtitle("Distribution of average movie rating")
rec_data <- rec_data[rowCounts(rec_data) > 50, colCounts(rec_data) > 50]

# User based collaborative filtering
# Split data into train and test
which_train <- sample(x = c(TRUE, FALSE), size = nrow(rec_data), replace = TRUE, prob = c(0.8, 0.2))
recc_data_train <- rec_data[which_train, ]
recc_data_test <- rec_data[!which_train, ]

# Calling the Recommender function
recc_model_ubcf = Recommender(data = recc_data_train, method = "UBCF")
n_recommended_ubcf = 5
recc_predicted_ubcf = predict(object = recc_model_ubcf, newdata = recc_data_test, n = n_recommended_ubcf)
recc_predicted_ubcf

# Getting predictions for a user
recc_predicted_ubcf@items[[1]]
recc_user_1_ubcf <- recc_predicted_ubcf@items[[1]]
movies_user_1_ubcf <- recc_predicted_ubcf@itemLabels[recc_user_1_ubcf]
movies_user_1_ubcf

# Item based collaborative filtering
# Split data into train and test
which_train <- sample(x = c(TRUE, FALSE), size = nrow(rec_data), replace = TRUE, prob = c(0.8, 0.2))
recc_data_train <- rec_data[which_train, ]
recc_data_test <- rec_data[!which_train, ]
recc_model_ibcf <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 30))

# Applying recommendation model on test set
n_recommended_ibcf <- 5
recc_predicted_ibcf <- predict(object = recc_model_ibcf, newdata = recc_data_test, n = n_recommended_ibcf)

# Getting predictions for a user
recc_predicted_ibcf@items[[1]]
recc_user_1_ibcf <- recc_predicted_ibcf@items[[1]]
movies_user_1_ibcf <- recc_predicted_ibcf@itemLabels[recc_user_1_ibcf]
movies_user_1_ibcf