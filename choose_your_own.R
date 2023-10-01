
library(tidyverse) 
library(caret)
library(rpart)

# Diagnostic Wisconsin Breast Cancer Database :

#   https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data

dl <- "Breast Cancer Wisconsin (Diagnostic) Data Set"
if(!file.exists(dl))
  download.file("https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data", dl)


data_breast <- read.csv("/Users/hussinalmustafa/Downloads/data.csv")
head(data_breast)

#  remove columns (1,33)

data_breast <- data_breast[,-c(1,33)]




# Changing the postiion of dependent variable .  

data_breast <- data_breast%>% relocate(diagnosis ) 

str (data_breast)


#  clean the data :


# check   missing data :

sapply(data_breast, function(.x) sum(is.na(.x))) 


 



# check the level of character in data set :

data_breast %>% select_if(is.character) %>%
                  map_if(is.character, unique)


# check how balanced is our response variable :


table(data_breast$diagnosis)


# Pre-process the data :


#  change the variables into factors :


data_breast<- data_breast %>% mutate_if(is_character, as.factor)

 
#  remove highly correlated predictors :

library(corrplot)
# correlation for all variables
 

data_corr <-   cor(data_breast[,2:31]) 
 

#  remove the highly correlated ones using the caret package.

data_b <-  data_breast %>% select(-findCorrelation(data_corr, cutoff = 0.9))

str(data_b)

# our data is 21 variables :

 ###  data visualization :

 
library(GGally)

# We have many variables in this dataset.  we will focus only on the first four .

ggpairs( data_b[1:4], aes(color=data_breast$diagnosis , alpha=0.4))



 

dat <- cbind(diagnosis = data_breast$diagnosis, data_b)
str(dat)

# Split titanic_clean into test and training sets:

set.seed(42, sample.kind = "Rounding")


test_index <- createDataPartition(dat$diagnosis, times = 1, p = 0.2, list = FALSE) # create a 20% test set
test_set <- dat[test_index,]
train_set <- dat[-test_index,]

nrow(train_set)

# 1-  Logistic regression models :

set.seed(1, sample.kind = "Rounding")  
train_glm <- train(diagnosis ~ ., method = "glm", data = train_set)

# The accuracy can be calculated using the following code:
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$diagnosis)


# 2- kNN model :

set.seed(6, sample.kind = "Rounding") 

train_knn <- train(diagnosis ~ .,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
train_knn$bestTune

# The accuracy can be calculated using the following code:
knn_preds <- predict(train_knn, test_set)
mean(knn_preds == test_set$diagnosis)


# 3- Classification tree model :




set.seed(10, sample.kind = "Rounding")    
train_rpart <- train(diagnosis ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
train_rpart$bestTune

# the accuracy of the decision tree model on the test set :

rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$diagnosis)

# 4- Random forest model :

set.seed(100, sample.kind = "Rounding")     
train_rf <- train(diagnosis ~ .,
                  data = train_set,
                  method = "rf",
                  ntree = 100,
                  tuneGrid = data.frame(mtry = seq(1:7)))
train_rf$bestTune



# The accuracy can be calculated using the following code:
  rf_preds <- predict(train_rf, test_set)
mean(rf_preds == test_set$diagnosis)

# determine the importance of various predictors to the random
# forest model.

#The most important variable can be found using the following code:
  varImp(train_rf) 
  
  
# we see that the  model with best accuracy is  Logistic
# regression model.
  
  

  
  
  





