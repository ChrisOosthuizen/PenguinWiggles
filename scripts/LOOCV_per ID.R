# Load required library
library(caret)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Simulate data
num_animals <- 8
num_iterations <- 20

animal_ids <- rep(LETTERS[1:num_animals], each = num_iterations)
iterations <- rep(1:num_iterations, times = num_animals)
observed_N <- rnorm(num_animals * num_iterations, mean = 10, sd = 2)
predicted_N <- observed_N + rnorm(num_animals * num_iterations, mean = 0, sd = 1)

# Create DataFrame
df <- data.frame(animal_ID = animal_ids, 
                 iteration = iterations, 
                 observed_N = observed_N,
                 predicted_N = predicted_N)

# Print first few rows of the DataFrame
print(head(df))

df

loocv <- function(data, model_func) {
  animal_ids <- unique(data$animal_ID)
  predictions <- numeric(nrow(data))
  
  for (i in 1:length(animal_ids)) {
    test_id <- animal_ids[i]
    train_data <- data[data$animal_ID != test_id, ]
    test_data <- data[data$animal_ID == test_id, ]
    
    model <- model_func(train_data)
    predictions[data$animal_ID == test_id] <- predict(model, test_data)
  }
  
  return(predictions)
}


lm_model <- function(data) {
  lm(observed_N ~ predicted_N, data = data)
}

# Perform LOOCV
cv_predictions <- loocv(df, lm_model)

df$cv_predictions = cv_predictions

cv_r_squared <- cor(df$observed_N, df$cv_predictions)^2
print(paste("Cross-validated R-squared:", cv_r_squared))

cv_rmse <- sqrt(mean((df$observed_N - df$cv_predictions)^2))
print(paste("Cross-validated RMSE:", cv_rmse))
