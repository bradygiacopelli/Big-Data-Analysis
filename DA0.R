#### Load Data ####
library(ggplot2)
library(data.table)
library(dplyr)
library(randomForest)
library(caret)

games = data.table::fread("~/Documents/Class/Stat 4970W/data for datafest 2019/games.csv")
gps = data.table::fread("~/Documents/Class/Stat 4970W/data for datafest 2019/gps.csv")
rpe = data.table::fread("~/Documents/Class/Stat 4970W/data for datafest 2019/rpe.csv")
wellness_data <- data.table::fread("~/Documents/Class/Stat 4970W/data for datafest 2019/wellness.csv")


#### Looking at Wellness Data and Predicting  ####
wellness_data$TrainingReadiness <- as.numeric(gsub("%", "", wellness_data$TrainingReadiness))

cols <- c("Fatigue", "Soreness", "Desire", "Irritability", "SleepHours", "SleepQuality", "MonitoringScore", 
          "Pain", "Illness", "Menstruation", "Nutrition", "NutritionAdjustment", "TrainingReadiness")

wellness <- wellness_data[, ..cols]

wellness <- na.omit(wellness)

categorical_columns <- c("Fatigue", "Pain", "Illness", "Menstruation", "Nutrition", "NutritionAdjustment")
wellness[, (categorical_columns) := lapply(.SD, as.factor), .SDcols = categorical_columns]

# Splitting data into training and testing sets
set.seed(614)
index <- caret::createDataPartition(wellness$Fatigue, p=0.8, list=FALSE)
trainData <- wellness[index,]
testData <- wellness[-index,]

trainData$Fatigue <- as.factor(trainData$Fatigue)
testData$Fatigue <- as.factor(testData$Fatigue)
# Building the Random Forest model using specified columns
fatigue_model <- randomForest(Fatigue ~ ., data=trainData, ntree=500, mtry=3, importance=TRUE)

# Predicting on test data
predictions <- predict(fatigue_model, testData)

# Confusion matrix and accuracy
confusion_matrix <- table(Predicted = predictions, Actual = testData$Fatigue)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))


varImpPlot(fatigue_model)

#### EDA On RPE AND WELLNESS ####
combined_rpe_wellness <- merge(rpe, wellness_data, by = c("Date", "PlayerID"), all = FALSE)

head(combined_rpe_wellness)

# Summary statistics for numeric variables
numeric_cols <- names(which(sapply(combined_rpe_wellness, is.numeric)))

# Calculate summary statistics only for numeric columns
summary_stats <- combined_rpe_wellness[, lapply(.SD, function(x) list(
  Mean = mean(x, na.rm = TRUE), 
  Median = median(x, na.rm = TRUE),
  SD = sd(x, na.rm = TRUE),
  Min = min(x, na.rm = TRUE),
  Max = max(x, na.rm = TRUE)
)), .SDcols = numeric_cols]

# Print the summary statistics
print(summary_stats)


cor_matrix <- cor(na.omit(combined_rpe_wellness[, c("RPE", "SessionLoad", "Fatigue", "Soreness", "SleepHours", "SleepQuality"), with = FALSE]))
print(cor_matrix)

##### Video Metrics ####
view(rpe)
view(wellness_data)
view(combined_rpe_wellness)
print(fatigue_model)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))
print(summary_stats)

