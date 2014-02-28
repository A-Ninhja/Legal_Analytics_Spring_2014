# File-Name:       Wine_Classifier.R           
# Due:             02/28/14                                
# By:              Andy Ninh
# Prompt:          Please Replicate the Wine Quality Classifier
# Data Used:       Wine Quality Data Set from UCI Machine Learning Repository, 
#                   Available at http://www.nd.edu/~mclark19/learn/data/goodwine.csv
# Packages Used:    doSNOW, corrplot, caret

# Load the packages

# Package doSNOW (For each parallel adaptor for the snow package)
library(doSNOW)

# Package corrplot (Visualization of a correlation matrix)
library(corrplot)

# Package caret (Classification and Regression Training)
library(caret)

# Import the data set
wine <- read.csv("http://www.nd.edu/~mclark19/learn/data/goodwine.csv")

# Set up R for parallel processing using the registerDoSNOW function
registerDoSNOW(makeCluster(3, type = "SOCK"))

# The corrplot package is a graphical display of a correlation matrix, confidence interval.
corrplot(cor(wine[, -c(13, 15)]), method = "number", tl.cex = 0.5)

# k-nearest neighbors approach gives the value of how far an observation 
# is from some other, given their respective values on a set of variables

set.seed(1234) #so that the indices will be the same when re-run
trainIndices <- createDataPartition(wine$good, p = 0.8, list = F)
wanted <- !colnames(wine) %in% c("free.sulfur.dioxide", "density", "quality", "color", "white")
wine_train <- wine[trainIndices, wanted] #remove quality and color, as well as density and others
wine_test <- wine[-trainIndices, wanted]

# Taking an initial peek at how the predictors separateon the target
wine_trainplot <- predict(preProcess(wine_train[,-10], method = "range"), wine_train[,-10])
featurePlot(wine_trainplot, wine_train$good, "box")

# Using 10-fold cross validation
set.seed(1234)
cv_opts <- trainControl(method="cv", number=10)
knn_opts <- data.frame(.k=c(seq(3, 11, 2), 25, 51, 101)) #odd to avoid ties
results_knn <- train(good~., data=wine_train, method="knn",
                     preProcess="range", trControl=cv_opts,
                     tuneGrid = knn_opts)

# Model test set performance
preds_knn <- predict(results_knn, wine_test[,-10])
confusionMatrix(preds_knn, wine_test[,-10], positive='Good')

# Performance Output
Confusion Matrix and Statistics

Reference
Prediction Bad Good
Bad  284  153
Good 192  669

Accuracy : 0.7342          
95% CI : (0.7093, 0.7581)
No Information Rate : 0.6333          
P-Value [Acc > NIR] : 6.981e-15       

Kappa : 0.4177          
Mcnemar's Test P-Value : 0.04077         

Sensitivity : 0.8139          
Specificity : 0.5966          
Pos Pred Value : 0.7770          
Neg Pred Value : 0.6499          
Prevalence : 0.6333          
Detection Rate : 0.5154          
Detection Prevalence : 0.6633          
Balanced Accuracy : 0.7053          

'Positive' Class : Good  
