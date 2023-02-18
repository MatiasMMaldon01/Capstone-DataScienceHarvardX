library(tidyverse)
library(caret)
library(lubridate)
library(stringr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

edx %>% select(rating, title) %>% group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Function that return the RMSE value
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Pre-Proccesing data
# Convert timestamp predictor into a human most readable format
edx$year <- edx$timestamp %>% as_datetime() %>% year()
edx$month <- edx$timestamp %>% as_datetime() %>% month()

#Extract the release date from title to a new predictor
edx <- edx %>% mutate(release_date = title %>% str_extract_all("\\([0-9]{4}\\)") %>%
                 str_extract("[0-9]{4}") %>% as.numeric(),
               title = title %>% str_remove("\\([0-9]{4}\\)")%>% str_trim("right"))

# Doing the same with the validation dataset in one step
final_holdout_test <- final_holdout_test %>% mutate(release_date = title %>% str_extract_all("\\([0-9]{4}\\)") %>%
                               str_extract("[0-9]{4}") %>% as.numeric(),
                             title = title %>% str_remove("\\([0-9]{4}\\)")%>% str_trim("right"),
                             year = timestamp %>% as_datetime() %>% year(),
                             month = timestamp %>% as_datetime() %>% month())

# Analizing data

edx %>% group_by(release_date) %>% summarize(count_rating = n()) %>% 
  ggplot(aes(release_date, count_rating)) +
  geom_col()

# Most 20 rated movies

edx %>% group_by(movieId, title) %>%
  summarize(count_rates = n()) %>%
  arrange(desc(count_rates)) %>% head(20) %>%
  ggplot(aes(reorder(title, count_rates, decreasing = TRUE), count_rates)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  labs(title = "Ratings Frequency Distribution - TOP 20 Movies",
       x = "Title", y = "Frequency")

#################################################################################

edx_temp <- edx %>% select(-timestamp)

# Create data partition

index_test <- createDataPartition(edx_temp$rating, times = 1, p=.25, list=FALSE)

train_set <- edx_temp %>% slice(-index_test)
test_set <- edx_temp %>% slice(index_test) 

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#################################################################################

# Let's start with a naive approach 
mu <- mean(train_set$rating)

naive_rmse <- RMSE(test_set$rating, mu)

results <- tibble(method = "Just the average", RMSE = naive_rmse)

# Movie effect method
movie_avgs <- train_set %>% group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

movie_effect <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

rmse_movie_effect <- RMSE(test_set$rating, movie_effect)

results <- results %>% add_row(method="Movie Effect Model", RMSE=rmse_movie_effect)

# Movie + User effect method

user_avgs <- train_set %>% left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i ))

user_effect <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by = 'userId') %>% 
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_user_effect <- RMSE(test_set$rating, user_effect)

results <- results %>% add_row(method="Movie + User Effect Model", RMSE=rmse_user_effect)

# Movie + User + Release Date effect method

release_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  group_by(release_date) %>%
  summarize(b_r = mean(rating - mu - b_i - b_u ))

release_effect <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by = 'userId') %>% 
  left_join(release_avgs, by = 'release_date') %>% 
  mutate(pred = mu+ b_i + b_u + b_r) %>%
  pull(pred)

rmse_release_effect <- RMSE(test_set$rating, release_effect)

results <- results %>% add_row(method="Movie + User + Release Date Effect Model", RMSE=rmse_release_effect)

# Regularization

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  release_avgs <- train_set %>% left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by = 'userId') %>%
    group_by(release_date) %>%
    summarize(b_r = sum(rating - mu - b_i - b_u )/ n()+ l)
  
  
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(release_avgs, by = 'release_date') %>% 
    mutate(pred = mu + b_i + b_u + b_r) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses) 

optimal_lambda <- lambdas[which.min(rmses)]

results <- results %>% add_row(method= 'Regularized Movie + User Effect Model', RMSE = min(rmses))

#################################################################################

# Final test
final_holdout_test <- final_holdout_test %>% select(-timestamp)

final_holdout_test <- train_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

final_b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+ optimal_lambda))

final_b_u <- train_set %>% 
  left_join(final_b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+ optimal_lambda))

final_rmse <- final_holdout_test %>% 
  left_join(final_b_i, by = "movieId") %>%
  left_join(final_b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>% 
  .$pred
#################################################################################

# Final Result
RMSE(final_holdout_test$rating, final_rmse)

