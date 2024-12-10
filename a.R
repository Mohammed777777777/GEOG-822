# Install necessary package if not installed
#install.packages("dplyr")  # For data manipulation (optional)

# Load the library
library(dplyr)
library(MASS)
setwd("D:/Usask Courses/822/Assignment/Data Analysis/Statistical Analysis")
# Convert Aspect to a factor

# Load your data (assuming your file is named 'data.csv')
data <- read.csv("All_data - Final.csv")
View(data)

#data <- data[ , -c(1:3)]
head(data)
names(data)
# Convert Aspect to a factor (if applicable)
data$Aspect <- as.factor(data$Aspect)
data$Ecosites <- as.factor(data$Ecosites)
# Check dataset structure
head(data)


##############
numeric_vars <- data[, c("Elevation", "Slope","Lf_Dis",
                         "HYDR_Dis","B_Dis","FireDis")]

cor_matrix <- cor(numeric_vars)
print(cor_matrix)

# Check associations for categorical variables
chisq_test <- chisq.test(table(data$Ecosites, data$Aspect))
print(chisq_test)


# Combine numeric and encoded categorical variables for VIF
data_encoded <- model.matrix(~ Ecosites + Aspect + Elevation + Slope
                             + Lf_Dis + HYDR_Dis + B_Dis + FireDis , data = data)
vif_values <- vif(lm(Elevation ~ ., data = as.data.frame(data_encoded)))
print(vif_values)

##########################################
#Every variables
# Specify topographical variables (adjust column names as per your dataset)
all_vars <- c("Elevation","Slope","Ecosites","Lf_Dis","HYDR_Dis",
              "FireDis","Aspect","B_Dis")

# Select Type and topographical variables
all_vars_data <- data[, c("Type", all_vars)]
View(all_vars_data)

# Logistic regression for topographical variables
all_vars_data_model <- glm(Type ~ ., data = all_vars_data, family = binomial)
summary(all_vars_data_model)

# Perform stepwise selection based on AIC
best_all_vars_data_model_model <- stepAIC(all_vars_data_model, direction = "both")
summary(best_all_vars_data_model_model)

predicted_probabilities <- predict(all_vars_data_model, type = "response")
head(predicted_probabilities)

#install.packages("pscl")
library(pscl)
model <- glm(Type ~ Slope + Elevation, data = all_vars_data, family = binomial)
pR2(model)

odds_ratios <- exp(coef(model))
print(odds_ratios)

# Calculate VIF for all predictors
vif_values <- vif(all_vars_data_model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
##########################################
#Every variables (with linear features, hydrological features)
# Specify topographical variables (adjust column names as per your dataset)
all_vars_m <- c("B_Dis","Slope", "Elevation","LF_Dis", "HyD_Dis", "Fire_Y.N", "Ecosites")

# Select Type and topographical variables
all_vars_data_m <- data[, c("Type", all_vars_m)]
View(all_vars_data_m)

# Logistic regression for topographical variables
all_vars_m_data_model <- glm(Type ~ ., data = all_vars_data_m, family = binomial)
summary(all_vars_m_data_model)

# Perform stepwise selection based on AIC
best_all_vars_m_data_model_model <- stepAIC(all_vars_m_data_model, direction = "both")
summary(best_all_vars_m_data_model_model)

install.packages("pscl")
library(pscl)

#########################
####################################################################
# Separate Categories 
# Distance based (Linear, Hydrological, boundary)

#Every variables (with linear features, hydrological features)
# Specify topographical variables (adjust column names as per your dataset)
D_vars_m <- c("LF_Dis", "HyD_Dis", "BndDis")

# Select Type and topographical variables
D_vars_m_data_m <- data[, c("Type", D_vars_m)]
View(D_vars_m_data_m)

# Logistic regression for topographical variables
D_vars_m_data_model <- glm(Type ~ ., data = D_vars_m_data_m, family = binomial)
summary(D_vars_m_data_model)

# Perform stepwise selection based on AIC
best_D_vars_m_data_model_model <- stepAIC(D_vars_m_data_model, direction = "both")
summary(best_D_vars_m_data_model_model)

########################################################
########################################################
# Separate Categories 
# Distance based (Linear, Hydrological, boundary)

#Every variables (with categorical var)
# Specify topographical variables (adjust column names as per your dataset)
C_vars_m <- c("Ecosites", "Fire_Y.N")

# Select Type and topographical variables
C_vars_m_data_m <- data[, c("Type", C_vars_m)]
View(C_vars_m_data_m)

# Logistic regression for topographical variables
C_vars_m_data_model <- glm(Type ~ ., data = C_vars_m_data_m, family = binomial)
summary(C_vars_m_data_model)

# Perform stepwise selection based on AIC
best_C_vars_m_data_model_model <- stepAIC(C_vars_m_data_model, direction = "both")
summary(best_C_vars_m_data_model_model)

########################################################
########################################################
# Separate Categories 
# Topographical based (Elevation, Slope)

#Every variables (with categorical var)
# Specify topographical variables (adjust column names as per your dataset)
T_vars_m <- c("Elevation", "Slope","Aspect")

# Select Type and topographical variables
T_vars_m_data_m <- data[, c("Type", T_vars_m)]
View(T_vars_m_data_m)

# Logistic regression for topographical variables
T_vars_m_data_model <- glm(Type ~ ., data = T_vars_m_data_m, family = binomial)
summary(T_vars_m_data_model)

# Perform stepwise selection based on AIC
best_T_vars_m_data_model_model <- stepAIC(T_vars_m_data_model, direction = "both")
summary(best_T_vars_m_data_model_model)

########################################################
########################################################

#########################
## Elisee's Suggestion###
#########################
#########################
#Topographical Variables
# Specify topographical variables (adjust column names as per your dataset)
topo_vars <- c("Slope", "Aspect", "Elevation")

# Select Type and topographical variables
topo_data <- data[, c("Type", topo_vars)]
View(topo_data)

# Logistic regression for topographical variables
topo_model <- glm(Type ~ ., data = topo_data, family = binomial)
summary(topo_model)

# Perform stepwise selection based on AIC
best_topo_model <- stepAIC(topo_model, direction = "both")
summary(best_topo_model)

#############################################################
# Linear Distance
# Specify distance-based variables (adjust column names if needed)
distance_vars <- c("OtherRoadDis", "PipelineDis", "PowerlineDis","HighwayDis",
                   "TrailDis","BndDis")

# Select Type and topographical variables
distance_data3 <- data[, c("Type", distance_vars)]

# Logistic regression for distance-based variables
distance_model <- glm(Type ~ ., data = distance_data3, family = binomial)
summary(distance_model)

# For distance-based variables
best_distance_model <- stepAIC(distance_model, direction = "both")
summary(best_distance_model)
best_distance_model

#############################################
#############################################
# Hydrological Features
# Specify distance-based variables (adjust column names if needed)
hydrol_vars <- c("WatercourseDis", "RiverDis")

# Select Type and topographical variables
hdistance_data3 <- data[, c("Type", hydrol_vars)]

# Logistic regression for distance-based variables
hdistance_model <- glm(Type ~ ., data = hdistance_data3, family = binomial)
summary(hdistance_model)

# For distance-based variables
best_hdistance_model <- stepAIC(hdistance_model, direction = "both")
summary(best_hdistance_model)
best_distance_model

############################################
############################################
# Landcover Features
# Specify distance-based variables (adjust column names if needed)
landcover_vars <- c("Fire_Y.N", "Ecosites")

# Select Type and topographical variables
landcover_data3 <- data[, c("Type", landcover_vars)]

# Logistic regression for distance-based variables
landcover_model <- glm(Type ~ ., data = landcover_data3, family = binomial)
summary(landcover_model)

# For distance-based variables
best_landcover_model <- stepAIC(landcover_model, direction = "both")
summary(best_landcover_model)
best_landcover_model
############################################
############################################

