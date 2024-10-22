#--------------------------------------------------------Section 01----------------------------------------
# set working directory
setwd(dirname(file.choose()))
getwd()

#-------------------------------------------------------section 02--------------------------------------
# read in data from csv file
dataset <- read.csv("Student_data.csv", stringsAsFactors = FALSE)

str(dataset)
head(dataset)
summary(dataset)

#---------------------------------------------------------section 03--------------------------------------
# check for missing data 
library(Amelia)
apply(dataset, MARGIN = 2, FUN = function(x) sum(is.na(x)))
missmap(dataset, col = c("red", "blue"), legend = TRUE)
dataset <- na.omit(dataset)


#Boxplot of the independent variable not normalize 
boxplot(dataset[-37])


#-------------------------------------------------------section 04-------------------------------
#----------------ENCODING TO INTEGER FOR CORRELATION TESTING -------------------------------------
#Convert the 'Target' column to a factor
dataset$Target <- factor(dataset$Target)

#Converting the dependent Variable (Target) to integer for corrlation testing
dataset$Target <- as.integer(dataset$Target)

#correlation Matix
#install.packages(corrplot)
library(corrplot)

cor_matrix1 <- cor(dataset, method = "spearman")

# Plot the correlation matrix using corrplot
corrplot(cor_matrix1, method = "circle", type = "upper", tl.col = "black", tl.srt = 50)

#-----------------------------------------------------section 05---------------------------------------------
#-------------------------------ENCODING BACK TO CHR FOR BETTER VIEW--------------------------------

# recode dependent variable (Target) as a factor, indicate all possible levels and label
dataset$Target <- factor(dataset$Target, levels = c(1, 2, 3),
                         labels = c("D", "E", "G"))

# Table of the dependent variable (Target)
table(dataset$Target)


# table or proportions with more informative labels
round(prop.table(table(dataset$Target)) * 100, digits = 1)

#------------------------------creating a pie chart for the dependent variable----------------------
install.packages(plotly)
library(plotly)

df <- data.frame(
  Target = c("Graduate", "Dropout", "Enrolled"),
  Count_T = c(49.9, 32.1, 17.9)
)


# Create a pie chart using plot_ly
Plot <- plot_ly(df, labels = ~Target, values = ~Count_T, type = "pie", textinfo = "value+label",
                hole = 0.4, pull = c(0, 0.2, 0.1)) %>%
  layout(title = "How many dropouts, enrolled & graduates are there in Target column",
         annotations = list(text = c("Graduate", "Dropout", "Enrolled"),
                            showarrow = FALSE,
                            x = c(0.5, 0.5, 0.5),
                            y = c(0.5, 0.5, 0.5)))

# Show the pie chart
Plot

#--------------------------------------------------------------section 06---------------------------------------------
#-------------------------------min- max nomalization------------------------------------- 
#data.mm <- apply(data, MARGIN = 2, FUN = function(x) (x - min(x))/diff(range(x)))
dataset01 <- apply(dataset[1:36], MARGIN = 2, FUN = function(x) scale(x, center = TRUE, scale = TRUE))

boxplot(dataset01)

#change matrix to a dataframe
Student_Data <- as.data.frame(dataset01)


# bring back the removed column
Student_Data <- cbind(Student_Data, Target = dataset$Target)
head(Student_Data)

#----------------------------------------------------------------------section 07------------------------------
#------------------------------Splitting the dataset into the training set and test set-----------------------------
#install.packages('caTools')

library(caTools)

set.seed(2010)
split = sample.split(Student_Data$Target, SplitRatio = 0.80)

training_set = subset(Student_Data, split == TRUE)
test_set = subset(Student_Data, split == FALSE)


#---------------------section 08---------------------------------------------
#Fitting Randomforest to the Training set and predicting the test set result 
library(randomForest)

RFM <- randomForest(x = training_set[-37],
                    y = training_set$Target)


#--------- evaluating model performance------------

#Fitting the predicted data into  
predicted_TargetRFM <- predict(RFM, test_set)
#test_set$predicted_Target = predicted_Target

# Create the cross tabulation of predicted vs. actual
library(gmodels)
CrossTable(predicted_TargetRFM, test_set$Target, prop.chisq=FALSE)

#Making the Confusion Matrix
library(caret)

confusion_matrix <- confusionMatrix(predicted_TargetRFM, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix




#-----------------section 09--------------------------------------
#Building another Model Support Vector Machine (SVM)
library(e1071)
?svm
SVM <- svm(formula = Target ~ ., 
           data = training_set,
           type = 'C-classification',
           kernel = 'radial')

#--------- evaluating model performance--------------
#Fitting the predicted data 
predicted_TargetSVM = predict(SVM, test_set)

# Create the cross tabulation of predicted vs. actual
library(gmodels)
CrossTable(predicted_TargetSVM, test_set$Target, prop.chisq=FALSE)


#Making the Confusion Matrix
library(caret)

confusion_matrix <- confusionMatrix(predicted_TargetSVM, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix

#checking different kernel
SVM1 <- svm(formula = Target ~ ., data = training_set, type = 'C-classification', kernel = 'linear')
predicted_TargetSVM1 = predict(SVM1, test_set)
CrossTable(predicted_TargetSVM1, test_set$Target, prop.chisq=FALSE)

SVM2 <- svm(formula = Target ~ ., data = training_set, type = 'C-classification', kernel = 'polynomial')
predicted_TargetSVM1 = predict(SVM2, test_set)
CrossTable(predicted_TargetSVM2, test_set$Target, prop.chisq=FALSE)

SVM3 <- svm(formula = Target ~ ., data = training_set, type = 'C-classification', kernel = 'sigmold')
predicted_TargetSVM1 = predict(SVM3, test_set)
CrossTable(predicted_TargetSVM3, test_set$Target, prop.chisq=FALSE)


#-----------------------------------------------Section 10--------------------------------------
#Fitting K-NN to the Training set and predicting the test set result = y_pred in the concec
library(class)
set.seed(12345)
KNN = knn(train = training_set[, -37],
             test = test_set[, -37],
             cl = training_set[, 37],
             k = 59)

#--------- evaluating model performance--------------

# Create the cross tabulation of predicted vs. actual
library(gmodels)
CrossTable(KNN, test_set$Target, prop.chisq=FALSE)


#Making the Confusion Matrix
library(caret)

confusion_matrix <- confusionMatrix(KNN, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix

#checking different numbers of K 
KNN1 = knn(train = training_set[, -37], test = test_set[, -37], cl = training_set[, 37], k = 40)
CrossTable(KNN1, test_set$Target, prop.chisq=FALSE)

KNN2 = knn(train = training_set[, -37], test = test_set[, -37], cl = training_set[, 37], k = 70)
CrossTable(KNN2, test_set$Target, prop.chisq=FALSE)

KNN3 = knn(train = training_set[, -37], test = test_set[, -37], cl = training_set[, 37], k = 64)
CrossTable(KNN3, test_set$Target, prop.chisq=FALSE)

KNN4 = knn(train = training_set[, -37], test = test_set[, -37], cl = training_set[, 37], k = 15)
CrossTable(KNN4, test_set$Target, prop.chisq=FALSE)



#--------------------------------------Decision Tree 11-----------------------------------------
library(rpart)
library(rpart.plot)
library(rattle)
set.seed(12345)
Dtree <- rpart(Target ~ ., data = training_set, method = 'class')

#--------- evaluating model performance--------------
predicted_TargetDtree <- predict(Dtree, test_set, type = "class")
rpart.plot(Dtree, box.palette = "RdBu", digits = 3)

# Create the cross tabulation of predicted vs. actual
library(gmodels)
CrossTable(predicted_TargetDtree, test_set$Target, prop.chisq=FALSE)


#Making the Confusion Matrix
library(caret)

confusion_matrix <- confusionMatrix(predicted_TargetDtree, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix

#----------------------------------------------------------section 12----------------------------------
#----------------------------Fitting Naive Bayes to the Training set----------------------
library(e1071)
?naiveBayes()
Naive <- naive_bayes(Target ~ ., data = training_set)

# Predict on the test set
predictions_nb <- predict(Naive, test_set)

# Create the cross tabulation of predicted vs. actual
CrossTable(predictions_nb, test_set$Target, prop.chisq=FALSE)

#Making the Confusion Matrix
confusion_matrix <- confusionMatrix(predictions_nb, test_set$Target, mode = "everything", positive = "Graduate")
confusion_matrix

