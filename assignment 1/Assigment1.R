
# Datasets info:

# The dataset contains 9358 instances of hourly averaged responses from an array of 5 metal oxide chemical
# sensors embedded in an Air Quality Chemical Multisensor Device. The device was located on the
# field in a significantly polluted area, at road level,within an Italian city.
# Data were recorded from March 2004 to February 2005 (one year)representing the longest freely available recordings
# of on field deployed air quality chemical sensor devices responses. Ground Truth hourly averaged concentrations for CO,
# Non Metanic Hydrocarbons, Benzene, Total Nitrogen Oxides (NOx) and Nitrogen Dioxide (NO2)  and were provided by a co-located
# reference certified analyzer. Evidences of cross-sensitivities as well as both concept and sensor drifts are present as described
# in De Vito et al., Sens. And Act. B, Vol. 129,2,2008 (citation required) eventually affecting sensors concentration estimation capabilities.
# Missing values are tagged with -200 value.
#This dataset can be used exclusively for research purposes. Commercial purposes are fully excluded.

# link: https://archive.ics.uci.edu/dataset/360/air+quality

### Problem 1. Summary Statistics Table
# upload dataset
# ----
data <- read.csv("/Users/gabrieledurante/Documents/UiO/STK-IN4300 - Statistical Learning/assigment 1/air quality dataset/AirQualityUCI.csv", sep=";")
data <- AirQualityUCI
data <- data[, !names(data) %in% c("X", "X.1", 'Date', 'Time')]
data

# install.packages("data.table")
library(data.table)
setnames(data, "T", "Temperature")

head(data)
dim(data)
summary(data)
str(data)


# check the NA's values
colSums(is.na(data)) # we have values to handle

# install.packages("ggplot2")
library(ggplot2)

na_count <- sapply(data, function(x) sum(is.na(x)))
na_count_df <- data.frame(Column = names(na_count), NA_Count = na_count)
ggplot(na_count_df, aes(x = Column, y = NA_Count)) +
  geom_bar(stat = "identity") +
  labs(title = "NA's per col", x = "Feature", y = "NA's") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_clean <- na.omit(data)
colSums(is.na(data_clean))


## Bad plot visual

## Barplot
barplot(table(data_clean$Temperature), col = "skyblue", main = "Barplot Temp", xlab = "Temperature", ylab = "Frequency")
# Pie chart
pie(table(data_clean$Temperature), col = rainbow(length(unique(data_clean$Temperature))), main = "Pie Plot Temp")

## Barplot
barplot(table(data_clean$RH), col = "skyblue", main = "Barplot RH (Relative Humidity)", xlab = "RH", ylab = "Frequency")
# Pie chart
pie(table(data_clean$RH), col = rainbow(length(unique(data_clean$RH))), main = "Pie Plot RH")



str(data_clean)# we need to transform data to perform a graphical rappresentation

data_clean$Temperature <- gsub(",", ".", data_clean$Temperature)
data_clean$Temperature <- as.numeric(data_clean$Temperature)

data_clean$CO.GT. <- gsub(",", ".", data_clean$CO.GT.)
data_clean$CO.GT. <- as.numeric(data_clean$CO.GT.)

data_clean$C6H6.GT. <- gsub(",", ".", data_clean$C6H6.GT.)
data_clean$C6H6.GT. <- as.numeric(data_clean$C6H6.GT.)

data_clean$RH <- gsub(",", ".", data_clean$RH)
data_clean$RH <- as.numeric(data_clean$RH)

data_clean$AH <- gsub(",", ".", data_clean$AH)
data_clean$AH <- as.numeric(data_clean$AH)

summary(data_clean)
str(data_clean)

# trasform data and put them in interval
RH_chr <- cut(
  data_clean$RH,
  breaks = c(-Inf, 24, 49, Inf),
  labels = c("low", "medium", "high"),
  right = TRUE
)
print(RH_chr)

# plotting
barplot(table(RH_chr), col = "skyblue", main = "Barplot RH (Relative Humidity)", xlab = "Relative Humidity (%)", ylab = "Frequency")

rh_freq <- table(RH_chr)
pie(
  rh_freq,
  main = "Pie Chart of Relative Humidity (RH)",
  labels = paste(names(rh_freq), "\n", round(100 * rh_freq / sum(rh_freq), 1), "%") # Add labels with percentages
)



# ----
# Statistical exploration of the data
# install.packages("psych")
# install.packages("skimr")
# install.packages("xtable")
library(psych)
library(skimr)
library(xtable)
describe(data_clean)
skim(data_clean)

#latex_summarytable <- xtable(summary(data))
#sink("tab.tex")
#print(latex_summarytable, type = "latex")
#sink()

# ----
### Problem 2: Bad Data Visualization

plot(data_clean$Temperature, data_clean$NOx.GT.,
     main = 'Temperature vs NOx.GT.',
     xlab = 'Temperature (°C)',
     ylab = 'NOx(GT) (ppb)',
     col = 'black',
     pch = 19,
     cex = 5)

plot(data_clean$Temperature, data_clean$PT08.S1.CO.,
     main = 'Temperature vs PT08.S1(CO)',
     xlab = 'Temperature (°C)',
     ylab = 'hourly averaged sensor response',
     col = 'black',
     pch = 19,
     cex = 5)

hist(data_clean$Temperature, 
     main='Temperature (°C)', 
     xlab="(°C)", 
     col="pink", 
     border="red")

hist(data_clean$PT08.S3.NOx., 
     xlim = c(-10, 50),  # Arbitrary range that may not fit the data
     main = "Histogram with Inappropriate Range", 
     xlab = "PT08.S3(NOx) hourly averaged sensor response", 
     col = "pink", 
     border = "red")

hist(data_clean$PT08.S3.NOx., 
     breaks = 1,  # Too few bins
     main = "PT08.S3(NOx) with Large Bins", 
     xlab = "",
     ylab = "",
     col = "pink", 
     border = "red")

boxplot(data_clean$Temperature, 
        main="Temperature",
        col="gray")

boxplot(data_clean$PT08.S3.NOx., 
        main="PT08.S3(NOx)", 
        col="gray",
        outline = FALSE)

# ----
### Problem 3. Good Data Visualization
## Bad plot rappresentation
data[data == -200] <- NA
data_clean <- na.omit(data)
data_clean
# data_clean <- data_clean[data_clean$Temperature != -200, ]
data_clean$Temperature
summary(data_clean)

data_clean$Temperature <- gsub(",", ".", data_clean$Temperature)
data_clean$Temperature <- as.numeric(data_clean$Temperature)

data_clean$CO.GT. <- gsub(",", ".", data_clean$CO.GT.)
data_clean$CO.GT. <- as.numeric(data_clean$CO.GT.)

data_clean$C6H6.GT. <- gsub(",", ".", data_clean$C6H6.GT.)
data_clean$C6H6.GT. <- as.numeric(data_clean$C6H6.GT.)

data_clean$RH <- gsub(",", ".", data_clean$RH)
data_clean$RH <- as.numeric(data_clean$RH)

data_clean$AH <- gsub(",", ".", data_clean$AH)
data_clean$AH <- as.numeric(data_clean$AH)

str(data_clean)

describe(data_clean)
skim(data_clean)
# ----
plot(data_clean$Temperature, data_clean$NOx.GT.,
     main = 'Temperature vs NOx.GT.',
     xlab = 'Temperature (°C)',
     ylab = 'NOx(GT) (ppb)',
     col = 'black',
     pch = 19)

plot(data_clean$Temperature, data_clean$PT08.S1.CO.,
     main = 'Temperature vs PT08.S1(CO)',
     xlab = 'Temperature (°C)',
     ylab = 'hourly averaged sensor response',
     col = 'black',
     pch = 19)

hist(data_clean$Temperature, 
     main='Temperature (°C)', 
     xlab="(°C)", 
     col="gray", 
     border="black")

hist(data_clean$PT08.S3.NOx., 
     main="PT08.S3(NOx)", 
     xlab="PT08.S3(NOx) hourly averaged sensor response", 
     col="gray", 
     border="black")

boxplot(data_clean$Temperature, 
        main="Temperature", 
        xlab="Temperature (°C)", 
        col="gray",
        horizontal = TRUE)

boxplot(data_clean$PT08.S3.NOx., 
        main="PT08.S3(NOx)", 
        xlab="PT08.S3(NOx) hourly averaged sensor response", 
        col="gray",
        horizontal = TRUE)

hist(data_clean$CO.GT., 
     main="PT08.S3(NOx)", 
     xlab="PT08.S3(NOx) hourly averaged sensor response", 
     col="gray", 
     border="black")


### Problem 4: Simple analysis
# ----
set.seed(15555029)
# build the dataset for the training
index <- sample(seq_len(nrow(data_clean)), size = 0.8 * nrow(data_clean))
trainData <- data_clean[index, ]
testData <- data_clean[-index, ]

y_train <- trainData$CO.GT.
X_train <- trainData[, !names(trainData) %in% c("CO.GT.")]

y_test <- testData$CO.GT.
X_test <- testData[, !names(testData) %in% c("CO.GT.")]

data.train <- as.data.frame(cbind(y_train, X_train))
colnames(data.train)[1] <- 'y'

# train the model
model <- lm(y ~ ., data = data.train)
summary(model)
predictions <- predict(model, newdata = X_test)

# MSE linear model
rmse_lm <- sqrt(mean((predictions - y_test)^2))
rmse_lm

# Plotting the results
plot(y_test, predictions, main = "Linear Model, y_test vs Predictions",
     xlab = "pred", ylab = "rv", pch = 19, col = "black")
abline(a = 0, b = 1, col = "red")

# now we look at teh best subset, evenif the variables are all significant for the model
# as we can see in summary(model)

library(MASS)
full.model <- lm(y ~ ., data = data.train)
null.model <- lm(y ~ 1, data = data.train)

# Stepwise Selection
mod.stepwise <- stepAIC(full.model, scope = list(lower = null.model, upper = full.model))
summary(mod.stepwise)
print(mod.stepwise)

predictions_stepwise <- predict(mod.stepwise, newdata = X_test)
plot(y_test, predictions_stepwise, main = "Stepwise, y_test vs Predictions",
     xlab = "pred", ylab = "rv", pch = 19, col = "black")
abline(a = 0, b = 1, col = "red")

rmse_sw <- sqrt(mean((predictions_stepwise - y_test)^2))

# CV
library(caret)
train_control <- trainControl(method = "cv", number = 10) # <- 10 folds
cv_model <- train(y ~ ., data = data.train, method = "lm", trControl = train_control)
print(cv_model)
predictions_cv <- predict(cv_model, newdata = X_test)
plot(y_test, predictions_cv, main = "Cross Validation, y_test vs Predictions",
     xlab = "pred", ylab = "rv", pch = 19, col = "black")
abline(a = 0, b = 1, col = "red")

rmse_cv <- sqrt(mean((predictions_cv - y_test)^2))

print(rmse_lm)
print(rmse_sw)
print(rmse_cv)



# ----
# now we try to use other methods to perform this regression problem

# Ridge
library(glmnet)
X_train_matrix <- as.matrix(X_train)
X_test_matrix <- as.matrix(X_test)
lambda.cv <- cv.glmnet(x = X_train_matrix, y = y_train, family = 'gaussian', alpha = 0)$lambda.min
mod.ridge <- glmnet(y = y_train, x = X_train_matrix, lambda = lambda.cv, family = 'gaussian', alpha = 0)
mod.ridge

predictions_ridge <- predict(mod.ridge, newx = X_test_matrix)
rmse_ridge <- sqrt(mean((predictions_ridge - y_test)^2))
rmse_ridge

# Lasso
cross.validation.result <- cv.glmnet(x = X_train_matrix, y = y_train, family = 'gaussian', alpha = 1)
lambda.cv <- cross.validation.result$lambda.min
mod.lasso <- glmnet(y = y_train, x = X_train_matrix, lambda = lambda.cv, family = 'gaussian', alpha = 1)
mod.lasso

predictions_lasso <- predict(mod.lasso, newx = X_test_matrix)
rmse_lasso <- sqrt(mean((predictions_lasso - y_test)^2))
rmse_lasso

print(rmse_lm)
print(rmse_sw)
print(rmse_cv)
print(rmse_ridge)
print(rmse_lasso)




