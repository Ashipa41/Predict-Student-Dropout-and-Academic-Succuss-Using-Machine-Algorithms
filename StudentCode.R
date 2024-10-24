#-------------------------------------------------------- Section 01 ----------------------------------------
# Set the working directory to the folder where the file is located
setwd(dirname(file.choose()))  # Allows the user to choose the file interactively
getwd()  # Verify the current working directory

#------------------------------------------------------- Section 02 --------------------------------------
# Read in data from the CSV file named "Student_data.csv"
dataset <- read.csv("Student_data.csv", stringsAsFactors = FALSE)  # Disable automatic conversion to factors

# Display structure, first few rows, and summary statistics of the dataset
str(dataset)  # Structure of the dataset
head(dataset)  # First 6 rows of the dataset
summary(dataset)  # Statistical summary of the dataset

#--------------------------------------------------------- Section 03 --------------------------------------
# Check for missing data and visualize it using a missmap

# Load the required package for missing data visualization
library(Amelia)

# Apply a function to count missing values in each column
apply(dataset, MARGIN = 2, FUN = function(x) sum(is.na(x)))

# Visualize missing data with a heatmap
missmap(dataset, col = c("red", "blue"), legend = TRUE)  # Red indicates missing, blue is present

# Remove rows with missing data
dataset <- na.omit(dataset)

# Create a boxplot for numerical variables before normalization
boxplot(dataset[-37])  # Exclude the 37th column (likely 'Target' or dependent variable)

#------------------------------------------------------- Section 04 ----------------------------------------
# ENCODING TO INTEGER FOR CORRELATION TESTING

# Convert the 'Target' column (dependent variable) to a factor and then to integer
dataset$Target <- factor(dataset$Target)  # Convert 'Target' to a factor
dataset$Target <- as.integer(dataset$Target)  # Convert 'Target' to integer

# Generate the Spearman correlation matrix
library(corrplot)
cor_matrix1 <- cor(dataset, method = "spearman")

# Visualize the correlation matrix using a corrplot
corrplot(cor_matrix1, method = "circle", type = "upper", tl.col = "black", tl.srt = 50)

#----------------------------------------------------- Section 05 ---------------------------------------------
# ENCODING BACK TO CHARACTER FOR BETTER VIEW

# Recode the 'Target' variable as a factor with descriptive labels
dataset$Target <- factor(dataset$Target, levels = c(1, 2, 3),
                         labels = c("D", "E", "G"))  # Example labels (can be customized)

# Display the frequency table of the 'Target' variable
table(dataset$Target)

# Display the proportions of 'Target' variable categories as percentages
round(prop.table(table(dataset$Target)) * 100, digits = 1)

# Create a pie chart of the 'Target' variable distribution
install.packages("plotly")
library(plotly)

df <- data.frame(
  Target = c("Graduate", "Dropout", "Enrolled"),
  Count_T = c(49.9, 32.1, 17.9)  # Example values, adjust based on your dataset
)

# Generate a pie chart using Plotly
Plot <- plot_ly(df, labels = ~Target, values = ~Count_T, type = "pie", textinfo = "value+label",
                hole = 0.4, pull = c(0, 0.2, 0.1)) %>%
  layout(title = "How many dropouts, enrolled & graduates are there in Target column",
         annotations = list(text = c("Graduate", "Dropout", "Enrolled"),
                            showarrow = FALSE,
                            x = c(0.5, 0.5, 0.5),
                            y = c(0.5, 0.5, 0.5)))

# Show the pie chart
Plot

#-------------------------------------------------------------- Section 06 ---------------------------------------------
# MIN-MAX NORMALIZATION

# Normalize the data using z-score normalization (center = mean, scale = standard deviation)
dataset01 <- apply(dataset[1:36], MARGIN = 2, FUN = function(x) scale(x, center = TRUE, scale = TRUE))

# Boxplot for the normalized data
boxplot(dataset01)

# Convert the matrix back to a data frame and restore the 'Target' column
Student_Data <- as.data.frame(dataset01)
Student_Data <- cbind(Student_Data, Target = dataset$Target)
head(Student_Data)

#---------------------------------------------------------------------- Section 07 ------------------------------
# SPLITTING THE DATASET INTO TRAINING AND TEST SETS

# Load the required library for splitting the dataset
library(caTools)
set.seed(2010)

# Split the data into 80% training and 20% test sets
split = sample.split(Student_Data$Target, SplitRatio = 0.80)
training_set = subset(Student_Data, split == TRUE)
test_set = subset(Student_Data, split == FALSE)

#--------------------- Section 08 ---------------------------------------------
# FITTING RANDOM FOREST MODEL

# Load the Random Forest package
library(randomForest)

# Train the Random Forest model
RFM <- randomForest(x = training_set[-37],  # Exclude 'Target' column from predictors
                    y = training_set$Target)

# Evaluate model performance using the test set
predicted_TargetRFM <- predict(RFM, test_set)

# Cross-tabulation of predicted vs actual values
library(gmodels)
CrossTable(predicted_TargetRFM, test_set$Target, prop.chisq = FALSE)

# Create the confusion matrix
library(caret)
confusion_matrix <- confusionMatrix(predicted_TargetRFM, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix

#----------------- Section 09 --------------------------------------
# FITTING SUPPORT VECTOR MACHINE (SVM)

# Load the SVM package
library(e1071)

# Train an SVM with radial kernel
SVM <- svm(formula = Target ~ ., data = training_set, type = 'C-classification', kernel = 'radial')

# Predict on the test set and evaluate performance
predicted_TargetSVM = predict(SVM, test_set)

# Cross-tabulation of predicted vs actual values
CrossTable(predicted_TargetSVM, test_set$Target, prop.chisq = FALSE)

# Create the confusion matrix
confusion_matrix <- confusionMatrix(predicted_TargetSVM, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix

# Repeat the process with different SVM kernels (linear, polynomial, sigmoid)

#---------------------------------------------------------- Section 12 ----------------------------------
# FITTING NAIVE BAYES MODEL

# Load the Naive Bayes package
library(e1071)

# Train a Naive Bayes model
Naive <- naive_bayes(Target ~ ., data = training_set)

# Predict on the test set and evaluate performance
predictions_nb <- predict(Naive, test_set)

# Cross-tabulation of predicted vs actual values
CrossTable(predictions_nb, test_set$Target, prop.chisq = FALSE)

# Create the confusion matrix
confusion_matrix <- confusionMatrix(predictions_nb, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix
