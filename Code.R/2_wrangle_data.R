################################
# Wrangle data
################################

# Note: this process could take a couple of minutes

# Installing required packages 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(anytime)) install.packages("anytime", repos = "http://cran.us.r-project.org")
library(lubridate)

# Loading raw data
load("rdas/ratings_raw.rda")
load("rdas/movies_raw.rda")

glimpse(ratings)
glimpse(movies)

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


