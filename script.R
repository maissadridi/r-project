# Load required libraries
library(quarto)
library(tidyverse)
library(magrittr)  # Ensure magrittr is loaded
# Step 1: Import the dataset
dataset <- read_excel("C:/Users/Maissa/Downloads/exercise_dataset.xlsx")
library(ggplot2)

# Graphique à barres
ggplot(data = dataset, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  labs(title = "Répartition par genre")

# Graphique linéaire
ggplot(data = dataset, aes(x = Age, y = 'Actual Weight')) +
  geom_line() +
  labs(title = "Évolution du poids en fonction de l'âge")

# Diagramme en boîte
ggplot(data = dataset, aes(x = Gender, y = BMI, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Distribution de l'IMC par genre")

# Graphique à secteurs
ggplot(data = dataset, aes(x = "", fill ='Weather Conditions')) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Répartition des conditions météorologiques")

# Graphique en nuage de points
ggplot(data = dataset, aes(x = Age, y = 'Actual Weight')) +
  geom_point() +
  labs(title = "Relation entre l'âge et le poids") +
  theme_minimal()

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
model <- lm(`Actual Weight` ~ Exercise + Age + Gender + Duration + `Heart Rate`+ BMI + `Weather Conditions` + `Exercise Intensity`, data = train_data)

# Step 6: Evaluate the model
predictions <- predict(model, newdata = test_data)
rmse <- sqrt(mean((test_data$`Actual Weight` - predictions)^2))
mae <- mean(abs(test_data$`Actual Weight` - predictions))
r_squared <- cor(test_data$`Actual Weight`, predictions)^2

print("Model evaluation:")
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R-squared:", r_squared))


# Render the Quarto script to a PDF file
quarto::quarto_render("report.Rmd", output_format = "pdf_document", output_file = "report.pdf")


# Step 8: Prepare a PowerPoint presentation
# You can create a PowerPoint file manually or use additional libraries like officer or powerpointR to automate this process.

