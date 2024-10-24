#------------------------------------------------------ Section 01 ---------------------------------------------------
# Set working directory
setwd(dirname(file.choose()))  # Opens a file dialog to choose a directory, and sets it as the working directory
getwd()  # Displays the current working directory

#------------------------------------------------------ Section 02 ---------------------------------------------------
# Load and inspect data
dataset <- read.csv("Student_data.csv", stringsAsFactors = FALSE)  # Read CSV data

# Inspect the structure, first few rows, and summary statistics
str(dataset)  # Check the structure of the dataset
head(dataset)  # Show the first 6 rows of the dataset
summary(dataset)  # Summarize each column with basic stats

#------------------------------------------------------ Section 03 ---------------------------------------------------
# Check for missing data and handle missing values
library(Amelia)
apply(dataset, MARGIN = 2, FUN = function(x) sum(is.na(x)))  # Count missing values in each column

# Visualize missing data
missmap(dataset, col = c("red", "blue"), legend = TRUE)  # Create a heatmap showing missing data

# Remove rows with missing values
dataset <- na.omit(dataset)

# Boxplot of independent variables before normalization
boxplot(dataset[-37])  # Exclude column 37 (presumed 'Target') from boxplot

#------------------------------------------------------ Section 04 ---------------------------------------------------
# Encoding the target variable (for correlation testing)
dataset$Target <- factor(dataset$Target)  # Convert the 'Target' column to a factor
dataset$Target <- as.integer(dataset$Target)  # Convert 'Target' to integer for correlation analysis

# Correlation Matrix
library(corrplot)
cor_matrix1 <- cor(dataset, method = "spearman")  # Calculate Spearman correlation
corrplot(cor_matrix1, method = "circle", type = "upper", tl.col = "black", tl.srt = 50)  # Plot correlation matrix

#------------------------------------------------------ Section 05 ---------------------------------------------------
# Re-encode target variable for better readability
dataset$Target <- factor(dataset$Target, levels = c(1, 2, 3), labels = c("D", "E", "G"))  # Rename target levels

# Display counts and proportions of each target category
table(dataset$Target)  # Count occurrences of each target level
round(prop.table(table(dataset$Target)) * 100, digits = 1)  # Proportions of target levels (in %)

# Create a pie chart of target variable categories
install.packages(plotly)
library(plotly)

# Data for pie chart
df <- data.frame(
  Target = c("Graduate", "Dropout", "Enrolled"),
  Count_T = c(49.9, 32.1, 17.9)
)

# Plot pie chart using plotly
Plot <- plot_ly(df, labels = ~Target, values = ~Count_T, type = "pie", textinfo = "value+label",
                hole = 0.4, pull = c(0, 0.2, 0.1)) %>%
  layout(title = "Distribution of Graduates, Dropouts & Enrolled",
         annotations = list(text = c("Graduate", "Dropout", "Enrolled"),
                            showarrow = FALSE,
                            x = c(0.5, 0.5, 0.5),
                            y = c(0.5, 0.5, 0.5)))

# Show the pie chart
Plot

#------------------------------------------------------ Section 06 ---------------------------------------------------
# Min-Max normalization
dataset01 <- apply(dataset[1:36], MARGIN = 2, FUN = function(x) scale(x, center = TRUE, scale = TRUE))  # Normalize

# Boxplot of normalized variables
boxplot(dataset01)

# Convert matrix back to dataframe
Student_Data <- as.data.frame(dataset01)

# Reattach 'Target' column
Student_Data <- cbind(Student_Data, Target = dataset$Target)
head(Student_Data)

#------------------------------------------------------ Section 07 ---------------------------------------------------
# Splitting the dataset into training and test sets
library(caTools)
set.seed(2010)
split = sample.split(Student_Data$Target, SplitRatio = 0.80)  # 80% training, 20% testing

training_set = subset(Student_Data, split == TRUE)  # Training set
test_set = subset(Student_Data, split == FALSE)  # Test set

#------------------------------------------------------ Section 08 ---------------------------------------------------
# Random Forest Model
library(randomForest)

# Train Random Forest model
RFM <- randomForest(x = training_set[-37], y = training_set$Target)

# Evaluate model performance
predicted_TargetRFM <- predict(RFM, test_set)  # Predict test set results

# Cross-tabulation and Confusion Matrix
library(gmodels)
CrossTable(predicted_TargetRFM, test_set$Target, prop.chisq = FALSE)  # Display cross-table

library(caret)
confusion_matrix <- confusionMatrix(predicted_TargetRFM, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix  # Display confusion matrix

#------------------------------------------------------ Section 09 ---------------------------------------------------
# Support Vector Machine (SVM) Model
library(e1071)

# Train SVM model (Radial kernel)
SVM <- svm(formula = Target ~ ., data = training_set, type = 'C-classification', kernel = 'radial')

# Evaluate SVM performance
predicted_TargetSVM <- predict(SVM, test_set)
CrossTable(predicted_TargetSVM, test_set$Target, prop.chisq = FALSE)  # Cross-tabulation

confusion_matrix <- confusionMatrix(predicted_TargetSVM, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix  # Display confusion matrix

# Check different kernels
SVM1 <- svm(formula = Target ~ ., data = training_set, type = 'C-classification', kernel = 'linear')
predicted_TargetSVM1 <- predict(SVM1, test_set)
CrossTable(predicted_TargetSVM1, test_set$Target, prop.chisq = FALSE)

SVM2 <- svm(formula = Target ~ ., data = training_set, type = 'C-classification', kernel = 'polynomial')
predicted_TargetSVM2 <- predict(SVM2, test_set)
CrossTable(predicted_TargetSVM2, test_set$Target, prop.chisq = FALSE)

SVM3 <- svm(formula = Target ~ ., data = training_set, type = 'C-classification', kernel = 'sigmoid')
predicted_TargetSVM3 <- predict(SVM3, test_set)
CrossTable(predicted_TargetSVM3, test_set$Target, prop.chisq = FALSE)

#------------------------------------------------------ Section 10 ---------------------------------------------------
# K-Nearest Neighbors (KNN) Model
library(class)
set.seed(12345)
KNN <- knn(train = training_set[, -37], test = test_set[, -37], cl = training_set[, 37], k = 59)

# Evaluate KNN performance
CrossTable(KNN, test_set$Target, prop.chisq = FALSE)  # Cross-tabulation

confusion_matrix <- confusionMatrix(KNN, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix  # Confusion matrix

# Test different K values
KNN1 <- knn(train = training_set[, -37], test = test_set[, -37], cl = training_set[, 37], k = 40)
CrossTable(KNN1, test_set$Target, prop.chisq = FALSE)

KNN2 <- knn(train = training_set[, -37], test = test_set[, -37], cl = training_set[, 37], k = 70)
CrossTable(KNN2, test_set$Target, prop.chisq = FALSE)

KNN3 <- knn(train = training_set[, -37], test = test_set[, -37], cl = training_set[, 37], k = 64)
CrossTable(KNN3, test_set$Target, prop.chisq = FALSE)

KNN4 <- knn(train = training_set[, -37], test = test_set[, -37], cl = training_set[, 37], k = 15)
CrossTable(KNN4, test_set$Target, prop.chisq = FALSE)

#------------------------------------------------------ Section 11 ---------------------------------------------------
# Decision Tree Model
library(rpart)
library(rpart.plot)

# Train Decision Tree model
set.seed(12345)
Dtree <- rpart(Target ~ ., data = training_set, method = 'class')

# Evaluate Decision Tree performance
predicted_TargetDtree <- predict(Dtree, test_set, type = "class")
rpart.plot(Dtree, box.palette = "RdBu", digits = 3)  # Plot decision tree

CrossTable(predicted_TargetDtree, test_set$Target, prop.chisq = FALSE)  # Cross-tabulation

confusion_matrix <- confusionMatrix(predicted_TargetDtree, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix  # Confusion matrix

#------------------------------------------------------ Section 12 ---------------------------------------------------
# Naive Bayes Model
library(e1071)

# Train Naive Bayes model
Naive <- naive_bayes(Target ~ ., data = training_set)

# Predict test set results
predictions_nb <- predict(Naive, test_set)

# Evaluate Naive Bayes performance
CrossTable(predictions_nb, test_set$Target, prop.chisq = FALSE)  # Cross-tabulation

confusion_matrix <- confusionMatrix(pred
