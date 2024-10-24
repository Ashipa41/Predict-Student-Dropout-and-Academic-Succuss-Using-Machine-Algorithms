#--------------------------------------------------------Section 01----------------------------------------
# Set working directory to the location of the selected file and display current working directory
setwd(dirname(file.choose()))  # Opens file dialog to select file, sets the directory to file's location
getwd()  # Prints the working directory

#-------------------------------------------------------section 02--------------------------------------
# Load and explore the dataset
dataset <- read.csv("Student_data.csv", stringsAsFactors = FALSE)  # Read CSV file

# Inspect dataset structure, first few rows, and summary statistics
str(dataset)  # Displays the structure of the dataset
head(dataset)  # Shows the first 6 rows of the dataset
summary(dataset)  # Displays summary statistics for each column

#---------------------------------------------------------section 03--------------------------------------
# Checking for missing data
library(Amelia)  # Load library for missing data visualization

# Apply function to calculate number of missing values per column
apply(dataset, MARGIN = 2, FUN = function(x) sum(is.na(x)))

# Visualize missing data in a heatmap
missmap(dataset, col = c("red", "blue"), legend = TRUE)  # Red for missing, blue for present data

# Remove rows with missing data
dataset <- na.omit(dataset)

# Boxplot to explore the independent variables before normalization
boxplot(dataset[-37])  # Plot excluding the 37th column (assumed Target)

#-------------------------------------------------------section 04-------------------------------
# Encoding target variable for correlation testing
# Convert the 'Target' column to a factor
dataset$Target <- factor(dataset$Target)

# Convert the target variable to integers for correlation analysis
dataset$Target <- as.integer(dataset$Target)

# Generate a Spearman correlation matrix and visualize it
library(corrplot)  # Load library for correlation plots

cor_matrix1 <- cor(dataset, method = "spearman")  # Compute correlation matrix
corrplot(cor_matrix1, method = "circle", type = "upper", tl.col = "black", tl.srt = 50)  # Visualize correlation matrix

#-----------------------------------------------------section 05---------------------------------------------
# Encoding target variable back to factor for better readability
# Recode 'Target' as a factor with specific levels and labels
dataset$Target <- factor(dataset$Target, levels = c(1, 2, 3), labels = c("D", "E", "G"))

# Create a frequency table of the target variable
table(dataset$Target)

# Show proportion table of the target variable
round(prop.table(table(dataset$Target)) * 100, digits = 1)

# Pie chart for visualizing the distribution of the target variable
install.packages(plotly)
library(plotly)

df <- data.frame(
  Target = c("Graduate", "Dropout", "Enrolled"),
  Count_T = c(49.9, 32.1, 17.9)
)

# Create a pie chart using plotly
Plot <- plot_ly(df, labels = ~Target, values = ~Count_T, type = "pie", textinfo = "value+label",
                hole = 0.4, pull = c(0, 0.2, 0.1)) %>%
  layout(title = "Distribution of Target variable",
         annotations = list(text = c("Graduate", "Dropout", "Enrolled"),
                            showarrow = FALSE, x = c(0.5, 0.5, 0.5), y = c(0.5, 0.5, 0.5)))

# Display the pie chart
Plot

#--------------------------------------------------------------section 06---------------------------------------------
# Min-max normalization (scaling data)
dataset01 <- apply(dataset[1:36], MARGIN = 2, FUN = function(x) scale(x, center = TRUE, scale = TRUE))  # Standardize the data

# Visualize the boxplots after normalization
boxplot(dataset01)

# Convert normalized matrix to a dataframe
Student_Data <- as.data.frame(dataset01)

# Add the target variable back into the dataset
Student_Data <- cbind(Student_Data, Target = dataset$Target)
head(Student_Data)  # View the first few rows

#----------------------------------------------------------------------section 07------------------------------
# Splitting the dataset into training and test sets
library(caTools)  # Load library for dataset splitting

set.seed(2010)
split = sample.split(Student_Data$Target, SplitRatio = 0.80)  # Split data (80% training, 20% test)

# Create training and test sets
training_set = subset(Student_Data, split == TRUE)
test_set = subset(Student_Data, split == FALSE)

#---------------------section 08---------------------------------------------
# Fitting a Random Forest model and predicting the test set
library(randomForest)  # Load randomForest library

RFM <- randomForest(x = training_set[-37], y = training_set$Target)  # Train random forest model

# Predict target values on the test set
predicted_TargetRFM <- predict(RFM, test_set)

# Create cross-tabulation of predicted vs. actual values
library(gmodels)
CrossTable(predicted_TargetRFM, test_set$Target, prop.chisq = FALSE)

# Create confusion matrix
library(caret)
confusion_matrix <- confusionMatrix(predicted_TargetRFM, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix  # Display confusion matrix

#-----------------section 09--------------------------------------
# Building another model using Support Vector Machine (SVM)
library(e1071)  # Load library for SVM

# Train an SVM model
SVM <- svm(formula = Target ~ ., data = training_set, type = 'C-classification', kernel = 'radial')

# Predict target values using SVM model
predicted_TargetSVM = predict(SVM, test_set)

# Cross-tabulation of predicted vs. actual values
CrossTable(predicted_TargetSVM, test_set$Target, prop.chisq = FALSE)

# Confusion matrix for SVM predictions
confusion_matrix <- confusionMatrix(predicted_TargetSVM, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix

# Repeat with different SVM kernels (linear, polynomial, sigmoid)
# (code continues with more models and evaluations)
