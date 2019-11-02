################################
# Wrangle data
################################

# Installing required packages 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(anytime)) install.packages("anytime", repos = "http://cran.us.r-project.org")
library(lubridate)
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
library(devtools)

#data structure
glimpse(edx)
glimpse(validation)

# Number of users
ratings %>% summarize(n_users = n_distinct(userId))

# movielens & edx  sizing
movielens %>% summarize(n_users = n_distinct(userId),
                        n_movies = n_distinct(movieId),
                        n_genres = n_distinct(genres),
                        n_ratings = n())

edx %>% summarize(n_users = n_distinct(userId),
                  n_movies = n_distinct(movieId),
                  n_genres = n_distinct(genres),
                  n_ratings = n())

#first 10 obs, there might be more than one genre per movie
movies %>% slice(1:10) 

# Movies with no rating
anti_join(movies,ratings) 

# Rating distribution
tab <- edx %>% count(rating) %>% mutate(proportion = n/sum(n))
# Ordering by proportion
tab %>% arrange(desc(proportion)) %>%
  kable(booktabs = T) %>%
  kable_styling( "latex", latex_options = "striped", full_width = "F", position = "left")
summary(edx$rating) 

# Visualizing rating distribution
tab %>%
  ggplot(aes(rating,proportion)) +
  geom_bar(color = "black", stat = "identity") +
  xlab("Rating") +
  ylab("Proportion") +
  ggtitle("Rating Distribution")

# Some users rate more movies than others
# Distribution of Rating by Users
edx %>% group_by(userId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + 
  geom_histogram(binwidth=0.1, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of users") +
  ggtitle("Ratings by Users") 

# Some movies get rated more than others
# Distribution of Rating by Movie
edx %>% group_by(movieId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(binwidth=0.1, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Ratings by Movie")

# Movie rating predictions will be compared to the true ratings in the validation set using RMSE
# Creating the function RMSE that computes the RMSE 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Model 1: Average rating
# calculating predicted ratings
mu_hat <- mean(edx$rating)

#RMSE of model 1
model_1_rmse <- RMSE(validation$rating, mu_hat)

# Storing RMSE
rmse_results <- data_frame(method = "Model 1: Average rating", RMSE = model_1_rmse)

# Printing RMSE
rmse_results %>% knitr::kable()


# Model 2: Movie Effect
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

# RMSE of model 2
model_2_rmse <- RMSE(predicted_ratings, validation$rating)

# Storing RMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 2: Movie Effect", 
                                     RMSE = model_2_rmse))
# Printing RMSE
rmse_results %>% knitr::kable()

# Model 3: Movie & User Effect
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

# Storing RMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 3: Movie & User Effect", 
                                     RMSE = model_3_rmse))

# Printing RMSE
rmse_results %>% knitr::kable()

# Model 4: Movie & User Effect + Regularization
# We use cross-validaton to choose the parameter lambda
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

# Predicted ratings (lambda) on test set 
  predicted_ratings <- edx %>%  
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>% 
    mutate(pred = mu_hat + b_i + b_u) %>% 
    pull(pred)

# RMSE (lambda)
  return(RMSE(predicted_ratings, edx$rating)) 
})

# Choosing lambda which minimize RMSE
qplot(lambdas, rmses) 
lambda <- lambdas[which.min(rmses)]
lambda
rmses[which.min(rmses)]
# min(rmses) == rmses[which.min(rmses)]

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

# Storing RMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 4: Movie + User Effect + Reg", 
                                     RMSE = model_4_rmse))

# Printing RMSE
rmse_results %>% knitr::kable()

# Model 5: Movie & User & Release Year & Genres Effect + Regularization
# We use cross-validaton to choose the parameter lambda

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

# Storing RMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 5: Movie + User Effect + year release + Reg", 
                                     RMSE = model_5_rmse))

# Printing RMSE
rmse_results %>% 
  kable(booktabs = T) %>%
  kable_styling( "latex", latex_options = "striped", full_width = "F", position = "left")

# % Improvement from model 1 to model 5
100*(1 - (rmse_results[5,2]/rmse_results[1,2]))

