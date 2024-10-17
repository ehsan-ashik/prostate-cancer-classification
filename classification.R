# INSTALL PACKAGES
install.packages("caret")
install.packages("doParallel")
install.packages('e1071', dependencies=TRUE)
install.packages("devtools")
install.packages('nnet')  
install.packages('neuralnet') 
install.packages('NeuralNetTools') 
install.packages("ROSE")
install.packages("dplyr")
install.packages('abind')
install.packages('zoo')
install.packages('xts')
install.packages('quantmod')
install.packages('ROCR')
install.packages("DMwR")



# LOAD PACKAGES
library(caret)
library(doParallel)
library(nnet)
library(neuralnet)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/
		raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
library(NeuralNetTools)
library(imbalance)
library(ROSE)
library(dplyr)
library("DMwR")


# INITIALIZE PARALLEL COMPUTING

no_cores <- detectCores() 
cl <- makeCluster(no_cores)
cl

registerDoParallel(cl)


# INPUT DATA
dataset <- read.csv(file = '', header = TRUE)


# COMBINED DATA PREPROC BEGIN

dataset <- subset (dataset, select = -X)
dataset <- subset (dataset, select = -ID_REF)

View(dataset)

train_orig <- dataset


train_orig = cbind(Class = dataset$y, train_orig)

train_orig <- subset (train_orig, select = -y)

View(train_orig)

# COMBINED DATA PREPROC END

# OTHER DATA PREPROC BEGIN

names(dataset)[1] = 'ID_REF'

View(dataset)

dataset_t = setNames(data.frame(t(dataset[,-1])), dataset[,1])

train_orig <- dataset_t

View(train_orig)

train_orig = cbind(Class = 0, train_orig)


# PC VS CTL
train_orig[,1][1:809] = 1 #'PC'
train_orig[,1][810:850] = 2 #'CTL'

# PC VS NPB
train_orig[,1][1:809] = 1 #'PC'
train_orig[,1][810:1050] = 2 #'NPB'

# NPB VS CTL
train_orig[,1][1:241] = 1 #'NPB'
train_orig[,1][242:282] = 2 #'CTL'

# OTHER DATA PREPROC END


train_orig <- data.frame(train_orig)

View(train_orig)

train_orig$Class<-as.numeric(train_orig$Class)
train_orig$Class <- as.factor(train_orig$Class)


head(train_orig[, 1:6])

table(train_orig$Class)


# DATA COMBINATION WORK BEGIN

train_orig$Class = trimws(train_orig$Class, which = c("both"))

train_orig$Class[train_orig$Class == "HCC"] <- "Hepatocellular Carcinoma"
train_orig$Class[train_orig$Class == "non-Cancer"] <- "Healthy"

train_orig <-data.frame(train_orig)

length(which(!(train_orig$Class %in% c("Prostate Cancer", "Healthy")) ))

train_orig$Class <- factor(train_orig$Class)

table(train_orig$Class)

final_combined_data <- train_orig

View(final_combined_data)

train_orig_x <- train_orig

train_orig_x$Class[!(train_orig_x$Class %in% c("Prostate Cancer"))] <- "Healthy"
train_orig_x$Class <- factor(train_orig_x$Class)

as.numeric(train_orig$Class)

table(train_orig_x$Class)

train_orig_x = train_orig_x %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

train_orig_x = train_orig_x %>%
  select_if(~ !any(is.na(.)))

# DATA COMBINATION WORK END



# SEPARATING TRAIN TEST

set.seed(123) #randomization`

#creating indices
trainIndex <- createDataPartition(train_orig_x$Class, p=0.8,list=FALSE)

#splitting data into training/testing data using the trainIndex object
train_sample <- train_orig_x[trainIndex,] #training data (60% of data)

test_sample <- train_orig_x[-trainIndex,] #testing data (40% of data)

train_sample <-data.frame(train_sample)

table(train_sample$Class)
table(test_sample$Class)

# TACKLING IMBALANCE

#imbalanceRatio(train_sample)

#train_sample_2 <- oversample(train_sample, ratio = 1, method = "RWO", filtering = TRUE, iterations = 1000)

#table(train_sample_2$Class)

#train_sample_labels <- train_sample[, 1]
#train_sample_labels <-  as.factor(train_sample$Class)
#summary(train_sample_labels)


# SAMPLING
#n = 1000

#index <- sample(nrow(train_orig), n)

#train_sample <- train_orig[index,]
#train_sample_labels <- factor(train_orig_labels[index])

#summary(train_sample_labels)


# TRAINING PARAM - 10-FOLD CROSS VALIDATION
cv_param <- trainControl(method = "cv", number = 10, allowParallel = T, p = 0.9)
cv_param_2 <- trainControl(method = "cv",
                           number = 1,
                           #repeats = 1,
                           verboseIter = FALSE,
                           allowParallel = T,
                           sampling = "up", #Down
                           p = 0.9)



folds <- 10
cvIndex <- createFolds(factor(train_sample$Class), folds, returnTrain = T)
tc <- trainControl(index = cvIndex,
                   method = 'cv', 
                   number = folds,
                   #repeats = 5,
                   verboseIter = FALSE,
                   allowParallel = T,
                   sampling = "up")


# CLASSIFICATION

# KNN
set.seed(150)

tune_grid_svm_linear = expand.grid(C = seq(0, 2, length = 10)) # method = "svmLinear"
tune_grid_svm_rbf = expand.grid(C = seq(0, 2, length = 10)) # method = "svmRadial"
tune_grid_rf <-  expand.grid(.mtry=c(1:10)) # method = "rf"
tune_grid_mlp = expand.grid(layer1 = 5:10, layer2 = 5:10, layer3 = 5:10) # method = "mlpML"

start.time <- Sys.time()
train_model_3 <- train(Class ~ .,
                     data = train_sample,
                     method = "mlpML", 
                     preProcess = c("scale", "center", "pca"), # , "nzv"
                     tuneGrid = tune_grid_mlp,  # data.frame(k = c(1:2)), # for KNN
                     #tuneLength = 10,
                     trControl = tc
                     )
end.time <- Sys.time()
time.taken <- end.time - start.time

time.taken

train_model_3
train_model_3$resample
getTrainPerf(train_model_3)


train_pred <- predict(train_model, newdata = test_sample, type = "raw")


table(train_pred)
table(test_sample$Class)


# CONFUSION MATRIX

threshold <- 0.5
pred      <- factor( ifelse(train_pred$`1`  > threshold, 1, 2) )
confusionMatrix(pred, test_sample$Class)

confusionMatrix(train_pred, test_sample$Class)



# SVM

# LINEAR

tune_grid = expand.grid(C = seq(0, 2, length = 10))

set.seed(259)

train_svm_linear <- train(train_orig #[,-1]
                          , train_orig_labels, 
                          method = "svmLinear", 
                          preProcess = c("center","scale", "nzv"),
                          tuneGrid = tune_grid,
                          trControl = cv_param)

train_svm_linear

train_svm_linear$resample

getTrainPerf(train_svm_linear)


# RBF

#tune_grid = expand.grid(C = seq(0, 2, length = 10))

set.seed(259)

train_svm_rbf <- train(train_sample[,-1], train_sample_labels, 
                       method = "svmRadial", 
                       preProcess = c("center","scale"),
                       tuneLength = 10,
                       trControl = cv_param)

train_svm_rbf

train_svm_rbf$resample

getTrainPerf(train_svm_rbf)


# RBF

set.seed(1229)

train_svm_poly <- train(train_sample[,-1], train_sample_labels, 
                        method = "svmPoly", 
                        preProcess = c("center","scale"),
                        tuneLength = 3,
                        trControl = cv_param)

train_svm_poly

train_svm_poly$resample

getTrainPerf(train_svm_poly)


# NN

# ONE HIDDEN LAYER

tune_grid = expand.grid(size = c(1:5, 10, 20),
                        decay = c(0, 0.05, 1, 2))

set.seed(259)

train_nn_one <- train(train_sample[,-1], train_sample_labels, 
                      method = "nnet", 
                      preProcess = c("center","scale", "nzv"),
                      tuneGrid = tune_grid,
                      trControl = cv_param)

train_nn_one

train_nn_one$resample

getTrainPerf(train_nn_one)

plot.nnet(train_nn_one)



# TWO HIDDEN LAYER

tune_grid <-  expand.grid(layer1 = c(1, 5),
                          layer2 = c(1, 5),
                          layer3 = 0)

set.seed(259)

train_nn_two <- train(train_sample[,-1], train_sample_labels, 
                      method = "mlpML", 
                      preProcess = c("center","scale", "nzv"),
                      tuneGrid = tune_grid,
                      linear.output = TRUE, 
                      trControl = cv_param)

train_nn_two

train_nn_two$resample

getTrainPerf(train_nn_two)

plot.nnet(train_nn_two)



# RANDOM FOREST

tune_grid <-  expand.grid(.mtry=c(1:10))

set.seed(259)

train_rr <- train(train_sample[,-1], train_sample_labels, 
                  method = "rf", 
                  tuneGrid = tune_grid,
                  trControl = cv_param)

train_rr

train_rr$resample

getTrainPerf(train_rr)

