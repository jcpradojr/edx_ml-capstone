############################################################## Create edx set, validation set, and submission file#############################################################
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/R/curso/capstone")

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
#https://grouplens.org/datasets/movielens/10m/
#http://files.grouplens.org/datasets/movielens/ml-10m.zip

  #creating temp file  
  dl <- tempfile()
  
  #download the dataset 
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                        col.names = c("userId", "movieId", "rating", "timestamp"))
  movies <- str_split_fixed(readLines("ml-10M100K/movies.dat"), "\\::", 3)
  
  #setting the column names
  colnames(movies) <- c("movieId", "title", "genres")
  
  #correct the movieId type of variable 
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId)[movieId]))
  
  movielens <- left_join(ratings, movies, by = "movieId")
  head(movielens)
  
  # Validation set will be 10% of MovieLens data
  set.seed(1)
  
  #We use the caret package using this code.
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  edx <- movielens[-test_index,]
  temp <- movielens[test_index,]
  
  #To make sure we don't include users and movies in the test set that do not appear in the training set, we removed these using the semi_join function
  validation <- temp %>%
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")

  # Add rows removed from validation set back into edx set  
  removed <- anti_join(temp, validation)
  edx <- rbind(edx, removed)


  #Q1 How many rows and columns are there in the edx dataset?
  dim(edx)
#  table(edx)
  head(edx)  
  
  summary(edx)

  # We can see the number of unique users that provide ratings and for how many unique movies they provided them using this code
  edx %>%
    summarize(n_users = n_distinct(userId),
              n_movies = n_distinct(movieId))
  
  #If we multiply those two numbers, we get 746.087.406 movies but the data set has 9.000.063 lines
  n_distinct(edx$movieId)* n_distinct(edx$userId)
  
  #This implies that not every user rated every movie
  edx %>% group_by(movieId) %>%
    summarize(n=n()) %>%
    qplot(n, data= ., geom="histogram")
  
  edx %>% group_by(userId) %>%
    summarize(n=n()) %>%
    qplot(n, data= ., geom="histogram")
  
  #Which movie has the greatest number of ratings?
  edx %>% group_by(title) %>%
    summarize(count=n()) %>%
    top_n(5) %>%
    arrange(desc(count))
  
  #What are the five most given ratings in order from most to least?
  edx %>% group_by(rating) %>%
    summarize(count=n()) %>%
    top_n(5) %>%
    arrange(desc(count))
  
  #In general, half star ratings are less common than whole star ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).
  edx %>% group_by(rating) %>%
    summarize(count=n()) %>%
    ggplot(aes(x=rating, y=count))+
    geom_line()
  
  #RMSE
  RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings-predicted_ratings)^2)) } 

  #RMSE>1 not a good prediction  
   
  ## ---> MATRIX FACTORIZATION
  #building the simplest possible recommendation system.
  
  #Yu,i=mu+SDu,i
  #Here epsilon represents independent errors sampled from the same distribution centered at zero, and mu represents the true rating for all movies and users.
  #We know that the estimate that minimizes the residual mean squared error is the least squared estimate of mu.
  
  #calculate mu_hat
  mu_hat <- mean(edx$rating)
  mu_hat
  #We compute this average on the training data.
  #And then we compute the residual mean squared error on the test set data.
  
  #naive RMSE
  naive_rmse<-RMSE(validation$rating, mu_hat)
  naive_rmse
  
  ### predictions using just the average
  predictions <- rep(2.5, nrow(validation))
  RMSE(validation$rating, predictions)
  
  rmse_results <- data_frame(method="Just the average", RMSE = naive_rmse)
  rmse_results
  
  #BIAS
  mu <- mean(edx$rating)
  movie_avgs <- edx %>%
    group_by(movieId) %>%
    summarize(b_i=mean(rating -  mu))

  predicted_ratings <- mu + validation %>%
    left_join(movie_avgs, by='movieId')%>%
    .$b_i
  
  model_1_rmse <- RMSE(predicted_ratings, validation$rating)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Movie Effect Model",
                                       RMSE = model_1_rmse))
  rmse_results %>% knitr::kable()
  
  user_avgs <- validation %>%
    left_join(movie_avgs, by='movieId')%>%
    group_by(userId) %>%
    summarize(b_u=mean(rating - mu - b_i))
  
  predicted_ratings <- validation %>%
    left_join(movie_avgs, by='movieId')%>%
    left_join(user_avgs, by='userId')%>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  
  model_2_rmse <- RMSE(predicted_ratings, validation$rating)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Movie + User Effects Model",
                                       RMSE = model_2_rmse))
  rmse_results %>% knitr::kable()
  
  
  #- [x] Regularization
  validation %>%
    left_join(movie_avgs, by='movieId') %>%
    mutate(residual=rating - (mu+b_i)) %>%
    arrange(desc(abs(residual))) %>%
    select(title, residual) %>% slice(1:10) %>% knitr::kable()
  
  movie_titles <- movielens %>%
    select(movieId, title)%>%
    distinct()
  
  movie_avgs %>% left_join(movie_titles, by='movieId') %>%
    arrange(desc(b_i)) %>%
    select(title, b_i) %>%
    slice(1:10) %>%
    knitr::kable()
  
  #good movies
  edx %>% count(movieId) %>%
    left_join(movie_avgs) %>%
    left_join(movie_titles, by='movieId') %>%
    arrange(desc(b_i)) %>%
    select(title, b_i, n) %>% 
    slice(1:10) %>% 
    knitr::kable()
  
  #bad movies
  edx %>% count(movieId) %>%
    left_join(movie_avgs) %>%
    left_join(movie_titles, by='movieId') %>%
    arrange(b_i) %>%
    select(title, b_i, n) %>% 
    slice(1:10) %>% 
    knitr::kable()
  
 #regularization
  lambda <- 3
  mu <- mean(edx$rating)
  movie_reg_avgs <- edx %>%
    group_by(movieId) %>%
    summarize(b_i=sum(rating-mu)/(n()+lambda), n_i = n())
  
  predicted_ratings <- validation %>%
    left_join(movie_reg_avgs, by='movieId') %>%
    mutate(pred=mu + b_i) %>%
    .$pred
  
  model_3_rmse <- RMSE(predicted_ratings, validation$rating)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Regularized Movie Effect Model",
                                       RMSE = model_3_rmse))
  
  rmse_results %>% knitr::kable()
  #Now note that lambda is a tuning parameter
  #We can use cross-fertilization to choose it.
  #We can use this code to do this.
  
  
  lambdas <- seq(0, 10, 0.25)
  mu <- mean(edx$rating)
  just_the_sum <-edx %>%
    group_by(movieId) %>%
    summarize(s=sum(rating - mu), n_i = n())
  
  rmses <- sapply(lambdas, function(l){
    predicted_ratings <- validation %>%
      left_join(just_the_sum, by='movieId') %>%
      mutate(b_i = s/(n_i+l)) %>%
      mutate(pred = mu + b_i) %>%
      .$pred
    return(RMSE(predicted_ratings, validation$rating))
  })
  
  qplot(lambdas,rmses)
  
  lambdas <- seq(0, 10, 0.25)
  rmses <- sapply(lambdas, function(l){
    mu <- mean(edx$rating)
    
    b_i <- edx %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    
    b_u <- edx %>%
      left_join(b_i, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    predicted_ratings <- validation %>%
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      mutate(pred = mu + b_i + b_u) %>%
      .$pred

    return(RMSE(predicted_ratings, validation$rating))
  })
  
  qplot(lambdas,rmses)
  
  
  lambda <- lambdas[which.min(rmses)]
  lambda
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))
  
  b_u <- edx %>%
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
  
  head(validation)
  predicted_ratings <- validation %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  model_3_rmse_adjusted <- RMSE(predicted_ratings, validation$rating)
  #model_3_rmse_adjusted <- rmses[which.min(rmses)]
  
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Regularized Movie Effect Model - adjusted",
                                       RMSE = model_3_rmse_adjusted))
  
  rmse_results %>% knitr::kable()

  # For grading, learners will run algorithm on validation set to generate ratings
  predicted_ratings
  validation2 <- validation %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    select(userId, movieId, pred) %>%
    mutate(rating=pred)%>%
    select(-pred)
    
  # Ratings will go into the CSV submission file below:
  write.csv(validation2,
            "submission.csv", na = "", row.names=FALSE)
  
  rm(dl, ratings, movies, test_index, temp, movielens, removed)

  