################################
# Downloading data
################################
 
# Note: this process could take a couple of minutes

# Installing required packages 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(anytime)) install.packages("anytime", repos = "http://cran.us.r-project.org")
library(lubridate)
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
library(kableExtra)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Creating a temporary file and downloading MovieLens 10M dataset
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Creating a data.frame 'ratings' from ratings.dat file
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

# Creating a matrix called 'movies' from movies.dat file
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

# Adding column names 
colnames(movies) <- c("movieId", "title", "genres")

# movies as data.frame
movies <- as.data.frame(movies) %>% 
  mutate(movieId = as.numeric(levels(movieId))[movieId],
         title = as.character(title),
         genres = as.character(genres))

# Glimpsing data sets
glimpse(ratings)
glimpse(movies)


################################
# Wrangle data
################################

# Note: this process could take a couple of minutes

# defining a pattern to extract the movie year from the title
pattern <- "\\((\\d{4})\\)$"
# \\( = escape of '(' 
# ( = start of the group
# \\d{4} = 4 digits
# ) = end of the group
# \\) = escape of ')'
# $ = end of the string

# Adding a new column with the year of release 
movies <- mutate (movies, yearRelease = as.numeric(str_match(str_trim(title), pattern)[,2]))  

# Adding a new column with movie age 
yearToday <- year(today())
movies <- mutate (movies, movieAge = yearToday - yearRelease)  

# Adding title and genres to ratings into a data.frame called movielens
movielens <- left_join(ratings, movies, by = "movieId")

glimpse(movielens)

#Setting the seed of R‘s random number generator in order to be able to reproduce the random objects like test_index
set.seed(1, sample.kind="Rounding")

# createDataPartition: generates indexes for randomly splitting the data into training and test sets
# Validation set will be 10% of MovieLens data
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Number of users
ratings %>% summarize(n_users = n_distinct(userId))

# Glimpsing edx
glimpse(edx)

# movielens and edx sizing
# edx dataset is 90% of movilens and contains samples of all the movies and users
movielens %>% summarize(n_users = n_distinct(userId),
                        n_movies = n_distinct(movieId),
                        n_genres = n_distinct(genres),
                        n_ratings = n())

edx %>% summarize(n_users = n_distinct(userId),
                  n_movies = n_distinct(movieId),
                  n_genres = n_distinct(genres),
                  n_ratings = n())

# Movies with no rating
anti_join(movies,ratings)

#first 10 obs, there might be more than one genre per movie
movies %>% slice(1:10) 

# Summary
summary(edx$rating)

# Adding proportion of rating
tab <- edx %>% count(rating) %>% mutate(proportion = n/sum(n))

# Ordering by proportion
tab %>% arrange(desc(proportion)) 

# Distribution of Rating by Users
edx %>% group_by(userId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + 
  geom_histogram(binwidth=0.1, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of users") +
  ggtitle("Ratings by Users") 

# Distribution of Rating by Movie
edx %>% group_by(movieId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(binwidth=0.1, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Ratings by Movie")

# Creating the function RMSE that computes the RMSE 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# calculating predicted ratings
mu_hat <- mean(edx$rating)

#RMSE of model 1
model_1_rmse <- RMSE(validation$rating, mu_hat)
model_1_rmse

# Storing RMSE
rmse_results <- data_frame(method = "Model 1: Average rating", RMSE = model_1_rmse)


# movie_avg: estimator for b_i
movie_avgs <- edx %>%
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_hat))

# b_i histogram
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

# calculating predicted ratings
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>% 
  mutate(pred = mu_hat + b_i) %>% 
  pull(pred)

#RMSE of model 2
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
model_2_rmse

# Storing RMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 2: Movie Effect", 
                                     RMSE = model_2_rmse))


# Average rating for user u
edx %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")

# user_avg: estimator for b_u
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

# Calculating predicted ratings
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  mutate(pred = mu_hat + b_i + b_u) %>% 
  pull(pred)

# RMSE of model 3
model_3_rmse <- RMSE(predicted_ratings, validation$rating)
model_3_rmse

# Storing RMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 3: Movie & User Effect", 
                                     RMSE = model_3_rmse))
# Regularization, Penalized Least Squares 

# We use cross-validaton to choose the parameter lambda
lambdas <- seq(0, 10, 0.25)
mu_hat <- mean(edx$rating) 

#for each lambda of lambdas we get the RMSE
rmses <- sapply(lambdas, function(l){
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat)/(n()+l))
  
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat)/(n()+l))
  
  # Predicted ratings (lambda) on test set 
  predicted_ratings <- edx %>%  
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>% 
    mutate(pred = mu_hat + b_i + b_u) %>% 
    pull(pred)
  
  # RMSE (lambda)
  return(RMSE(predicted_ratings, edx$rating)) 
})

# Choosing lambda which minimize RMSES
qplot(lambdas, rmses) 

lambda <- lambdas[which.min(rmses)]
lambda

rmses[which.min(rmses)]

# Applying lambda on validation set
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda))

b_u <- edx %>%
  left_join(b_i, by="movieId") %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_hat)/(n()+lambda))

predicted_ratings <- validation %>%
  left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") %>% 
  mutate(pred = mu_hat + b_i + b_u) %>% 
  pull(pred)

# RMSE of model 4
model_4_rmse <- RMSE(predicted_ratings, validation$rating)
model_4_rmse

# Storing RMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 4: Movie + User Effect + Reg", 
                                     RMSE = model_4_rmse))


lambdas <- seq(0, 10, 0.25)
mu_hat <- mean(edx$rating) 

rmses <- sapply(lambdas, function(l){
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat)/(n()+l))
  
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat)/(n()+l))
  
  b_j <- edx %>%
    left_join(b_i, by="movieId") %>% 
    left_join(b_u, by="userId") %>% 
    group_by(yearRelease) %>%
    summarize(b_j = sum(rating - b_i - b_u - mu_hat)/(n()+l))
  
  b_k <- edx %>%
    left_join(b_i, by="movieId") %>% 
    left_join(b_u, by="userId") %>% 
    left_join(b_j, by="yearRelease") %>%
    group_by(genres) %>%
    summarize(b_k = sum(rating - b_i - b_u - b_j - mu_hat)/(n()+l))
  
  predicted_ratings <- edx %>%
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>% 
    left_join(b_j, by = "yearRelease") %>%
    left_join(b_k, by = "genres") %>%
    mutate(pred = mu_hat + b_i + b_u + b_j + b_k) %>% 
    pull(pred)
  
  return(RMSE(predicted_ratings, edx$rating)) 
})

# Choosing lambda which minimize RMSES
qplot(lambdas, rmses) 
lambda <- lambdas[which.min(rmses)]
rmses[which.min(rmses)]

# Applying lambda on validation set
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda))

b_u <- edx %>%
  left_join(b_i, by="movieId") %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_hat)/(n()+lambda))

b_j <- edx %>%
  left_join(b_i, by="movieId") %>% 
  left_join(b_u, by="userId") %>% 
  group_by(yearRelease) %>%
  summarize(b_j = sum(rating - b_i - b_u - mu_hat)/(n()+lambda))

b_k <- edx %>%
  left_join(b_i, by="movieId") %>% 
  left_join(b_u, by="userId") %>% 
  left_join(b_j, by="yearRelease") %>%
  group_by(genres) %>%
  summarize(b_k = sum(rating - b_i - b_u - b_j - mu_hat)/(n()+lambda))

predicted_ratings <- validation %>%
  left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") %>% 
  left_join(b_j, by = "yearRelease") %>%
  left_join(b_k, by = "genres") %>%
  mutate(pred = mu_hat + b_i + b_u + b_j + b_k) %>% 
  pull(pred)

# RMSE of model 5
model_5_rmse <- RMSE(predicted_ratings, validation$rating)
model_5_rmse

# Storing RMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 5: Movie + User Effect + year release + Reg", 
                                     RMSE = model_5_rmse))

# Printing RMSE
rmse_results %>% knitr::kable()

