# Install necessary package if not installed
#install.packages("dplyr")  # For data manipulation (optional)

# Load the library
library(dplyr)
library(MASS)
setwd("D:/Usask Courses/822/Assignment/Data Analysis/Statistical Analysis")
# Convert Aspect to a factor

# Load your data (assuming your file is named 'data.csv')
data <- read.csv("All_data.csv")
View(data)

#data <- data[ , -c(1:3)]
head(data)

full_model <- glm(Type ~ ., data = data, family = binomial)
summary(full_model)

# Perform stepwise regression to select the best model based on AIC
best_model <- stepAIC(full_model, direction = "both")
summary(best_model)

model <- glm(Type ~ P_PL_Dis + O_Road + H_Way + River + Slope..deg., 
             data = data, 
             family = binomial)

summary(model)


