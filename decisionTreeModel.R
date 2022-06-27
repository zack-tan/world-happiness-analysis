
OUTPUT_FIELD      <- "Happiness.Score"
TT_SPLIT          <- 0.7
FOREST_SIZE       <- 1500
FILE_NAME         <- "2017-happy.csv"

# Define and load libraries used in this project
# Library from CRAN     Version
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.51.4
# pROC	                 1.15.3
# formattable 	         0.2.01
# stats                  3.6.1
# PerformanceAnalytics   1.5.3
# stringr                1.4.0
# partykit               1.2.5
# C50                    0.2.1
# caret                  6.0.84
# rpart                  4.1-15

MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "pROC",
               "rattle",
               "formattable",
               "stats",
               "caret",
               "PerformanceAnalytics",
               "stringr",
               "partykit",
               "C50",
               "randomForest",
               "caret",
               "e1071",
               "tidyr",
               "stringr",
               "rpart"
               )

# ************************************************
# caretClassifier() :
#
# Builds an implementation of random forest then tests it.
# Creates a single decision tree for visualisation in the Viewer.
# 
# INPUT : 
# : Data Frame - train - Training data
# : Data Frame - test - Test data
#
# ************************************************
caretClassifier<- function(train, test){
  # ---------------------------------------------------------
  # Train control method - Sets parameters for train method below
  # controls the computational nuances of the train() method
  # method - Defines the method to be used for resampling. Using repeatedcv for cross validation
  # number - no. of folds or resampling iterations
  # repeats - no of times to repeat the cross-validation
  #
  # Source: https://www.rdocumentation.org/packages/caret/versions/6.0-84/topics/trainControl
  # ---------------------------------------------------------
  control <- trainControl(method = "repeatedcv", number = 3, repeats = 5)
  
  # ---------------------------------------------------------
  # Train method - Trains the model method below
  # method - Defines the method to be used for training. Using rf as per lab
  # x ~. - Defines the response variable
  # data - training dataset
  # metric - Uses 'accuracy' for classification models
  #
  # Source: https://www.rdocumentation.org/packages/caret/versions/6.0-84/topics/trainControl
  # ---------------------------------------------------------
  forestClassifier <- train(Happiness.Score ~., data = train, 
                     method = "rf",
                     trControl = control,
                     ntree = FOREST_SIZE, 
                     metric = "Accuracy")
  
  # Output information on the randomForest to the console
  cat("\nPrinting random forest information...\n\n")
  print (forestClassifier)

  #Output importance of available predictors to the console
  cat("\nPrinting importance of variables...\n\n")
  importance<-varImp(forestClassifier)
  print(importance)
  
  # Create a single decision tree for visualisation
  visualiseTree(train)
  
  # Test the randomForest for accuracy
  cat("\nTesting classifier on test data...\n")
  testForest <- predict(forestClassifier, newdata = test, type="raw")
  
  print(testForest)
  
  # Factoring response field in test dataset
  responseBins <- factor(test$Happiness.Score)
  
  # Generate confusion matrix and print it to the console
  confusion <- confusionMatrix(testForest, responseBins)
  cat("\nPrinting Confusion Matrix...\n\n\n")
  print(confusion)
  
  cat("\nAccuracy of random forest on testing data: ", (confusion$overall)[['Accuracy']], "\n\n")
}

# ************************************************
# visualiseTree() :
#
# Builds a single decision tree using a CART model from the rpart library
# 
# INPUT : 
# : Data Frame - dataset - Input on which to build the tree on
#
# ************************************************
visualiseTree <- function(dataset)
{
  # Generate single decision tree for visualisation using graphs
  cat("\nGenerating single decision tree for visualisation using graphs...\n\n")
  
  # Set parameters for and train the decision tree
  control <- trainControl(method = "repeatedcv", number = 3, repeats = 5)
  singleTree <- train(Happiness.Score ~., data = dataset, 
                      method = "rpart",
                      parms = list(split = "information"),
                      trControl = control,
                      metric = "Accuracy")
  
  # Print the parameters of the generated decisionTree
  print(singleTree)
  
  # Visualise the tree on the Viewer
  fancyRpartPlot(singleTree$finalModel, sub="", tweak = 1.2, type =5, Margin=0.1)
  
  # Plot diagram to pdf
  cat("\nPlotting diagram to viewer...\n")
  dev.copy(png, "plotsave.png")
  dev.off()
}


main <- function()
{
  # Pre-process function from other file
  combined_df <- preProcess(FILE_NAME)
  
  # Add or remove columns to experiment with accuracy
  # Uncomment a column to remove it from the model
  
  # Geographical Elements
  combined_df$Map.references <- NULL
  #combined_df$border.countries <- NULL
  #combined_df$coastline <- NULL
  #combined_df$climate <- NULL
  
  # Other Elements
  #combined_df$religions <- NULL
  #combined_df$median.age <- NULL
  #combined_df$population.growth.rate <- NULL
  #combined_df$government.type <- NULL
  #combined_df$GDP.growth.rate <- NULL
  
  # Health Elements
  #combined_df$life.expectancy <- NULL
  #combined_df$health.expenditure <- NULL
  #combined_df$obesity.rate <- NULL
  
  # Natural Resources
  combined_df$natural.gas <- NULL
  combined_df$coal <- NULL
  combined_df$petrol <- NULL
  combined_df$oil <- NULL
  combined_df$gold <- NULL
  combined_df$fish <- NULL
  combined_df$iron <- NULL
  combined_df$hydropower <- NULL
  combined_df$diamond <- NULL
  
  # Used to view csv after preprocessing and isolation of fields
  write.csv(combined_df, "processed_data.csv")

  # Split dataset - caret package
  intrain <- createDataPartition(y = combined_df$Happiness.Score, p= TT_SPLIT, list = FALSE)
  train <- combined_df[intrain,]
  test <- combined_df[-intrain,]
  
  cat("\nDataset has been split into train test using ratio of", TT_SPLIT, "\n")
  
  # Build the classifiers
  caretClassifier(train, test)
}

# ************************************************
# This is where R starts execution

gc() # garbage collection to automatically release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# Clear all warning messages
assign("last.warning", NULL, envir = baseenv())

# Clear the console area
cat("\014")

print("Start Random Forest Implementation")

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

# Load additional R script files
source("preProcessClassification.R")

set.seed(69)

# ************************************************
combined_df = main()

print("end")

