# Set up

# Load required library
library(caret)

#create data frame
df <- data.frame(y=c(6, 8, 12, 14, 14, 15, 17, 22, 24, 23,6, 8, 12, 14, 14, 15, 17, 22, 24, 23),
                 x1=c(1, 2, 2, 3, 14, 15, 17, 22, 24, 22,6, 8, 12, 14, 14, 15, 17, 22, 24, 25))

#view data frame
df

#specify the cross-validation method
set.seed(123)

ctrl <- trainControl(method = "LOOCV")

#fit a regression model and use LOOCV to evaluate performance
model <- train(y ~ x1 , data = df, method = "lm", trControl = ctrl)

#view summary of LOOCV               
print(model)


# Write own function to do the above:

loocv <- function(data, model_func) {
  predictions <- numeric(nrow(data))
  
  for (i in 1:nrow(data)) {
    train_data <- data[-i, ]  # Leave out the i-th row
    test_data <- data[i, , drop = FALSE]  # Keep only the i-th row
    
    model <- model_func(train_data)
    predictions[i] <- predict(model, test_data)
  }
  
  return(predictions)
}


lm_model <- function(data) {
  lm(y ~ x1, data = data)
}

# Perform LOOCV
cv_predictions <- loocv(df[, c("y", "x1")], lm_model)

# Calculate R-squared
observed <- df$y
cv_r_squared <- cor(observed, cv_predictions)^2
print(paste("Cross-validated R-squared:", cv_r_squared))

cv_rmse <- sqrt(mean((observed - cv_predictions)^2))
print(paste("Cross-validated RMSE:", cv_rmse))


