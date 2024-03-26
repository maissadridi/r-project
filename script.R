# Load required libraries
library(quarto)
library(tidyverse)

# Step 1: Import the dataset
dataset <- read_excel("C:/Users/Maissa/Downloads/exercise_dataset.xlsx")

# Step 2: Explore the dataset
print("Exploring the dataset:")
print(head(dataset))
print(summary(dataset))
print(str(dataset))

# Step 3: Data preprocessing (if needed)

# Step 4: Split the dataset into training and testing sets
set.seed(123) # for reproducibility
train_indices <- sample(1:nrow(dataset), 0.8 * nrow(dataset))
train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]

# Step 5: Choose the predictive modeling technique
# For demonstration purposes, let's use linear regression
model <- lm(Actual_Weight ~ Exercise + Age + Gender + Duration + Heart_Rate + BMI + Weather_Conditions + Exercise_Intensity, data = train_data)

# Step 6: Evaluate the model
predictions <- predict(model, newdata = test_data)
rmse <- sqrt(mean((test_data$Actual_Weight - predictions)^2))
mae <- mean(abs(test_data$Actual_Weight - predictions))
r_squared <- cor(test_data$Actual_Weight, predictions)^2

print("Model evaluation:")
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R-squared:", r_squared))

# Step 7: Generate a report using Quarto
quarto::quarto_script("report.Rmd") %>%
  quarto::quarto_pdf(output = "report.pdf")

# Step 8: Prepare a PowerPoint presentation
# You can create a PowerPoint file manually or use additional libraries like officer or powerpointR to automate this process.

