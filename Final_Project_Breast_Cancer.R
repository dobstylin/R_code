# Assignemnt  : Final Project (Breast Cancer Dataset Group)
# Class       : DSC 424
# Authors     : Yolanda Lewis, Daniel O'Brien, Jose Guzman, Will Rannick, Ross Gibson
# Date        : August 10, 2020

#########################################################
##################### Data Import #######################
#########################################################

#Data Import
setwd("/Users/danielobrien/desktop")

dataset = read.csv("breast_cancer_dataset.csv", header = TRUE, sep = ",")
head(dataset)

#replace w/ correct path for your computer
setwd("D:/School/DSC 424 - Advanced Data Analysis/Final Project")
#import data to 'dataset' variable
dataset <- read.csv("breast_cancer_dataset.csv", sep=",", header=T)

#prints first 6 rows to be sure data is correct
head(dataset)
#prints last 6 rows to be sure data is correct
tail(dataset)
#confirm 'dataset' row and column count (569; 32)
dim(dataset)
#data structures
str(dataset)
#variable names
names(dataset)

#########################################################
##################### Data Clensing #####################
#########################################################

#check for NAs across ALL variables (none found)
sum(is.na(dataset))

#convert 'diagnosis' variable into a new numerical binary variable ('num_diagnosis')
#   where a malignant tumor (one that can spread to the surronding area and the rest
#   of the body) is '1' and a benign tumor (one that cannot spread) is a '0'
dataset$num_diagnosis <- ifelse(dataset$diagnosis == 'M', 1, 0)

#confirm new response variable field is created
head(dataset)
dim(dataset)

#remove 'id' field
dataset <- dataset[,2:33]

#confirm only 'id' field is removed
names(dataset)

#split dataset by field type in prep for exploratory analysis
dataset_mean <- data.frame(dataset$num_diagnosis, dataset$radius_mean, dataset$texture_mean,
                           dataset$perimeter_mean, dataset$area_mean, dataset$smoothness_mean,
                           dataset$compactness_mean, dataset$concavity_mean, dataset$concave.points_mean,
                           dataset$symmetry_mean, dataset$fractal_dimension_mean)

dataset_se <- data.frame(dataset$num_diagnosis, dataset$radius_se, dataset$texture_se,
                           dataset$perimeter_se, dataset$area_se, dataset$smoothness_se,
                           dataset$compactness_se, dataset$concavity_se, dataset$concave.points_se,
                           dataset$symmetry_se, dataset$fractal_dimension_se)

dataset_worst <- data.frame(dataset$num_diagnosis, dataset$radius_worst, dataset$texture_worst,
                           dataset$perimeter_worst, dataset$area_worst, dataset$smoothness_worst,
                           dataset$compactness_worst, dataset$concavity_worst, dataset$concave.points_worst,
                           dataset$symmetry_worst, dataset$fractal_dimension_worst)

#########################################################
################# Exploratory Analysis ##################
#########################################################

#import 'psych' or 'Hmisc' and print descriptive statistics
#describe() function is used in both packages
#library(Hmisc)
library(psych)
describe(dataset[2:32]) #removes categorical 'diagnosis' variable

#run and output correlation plot for split datasets (for easier visualization)
library(corrplot)
corrplot(cor(dataset_mean,method="spearman"), method = "number")
corrplot(cor(dataset_se,method="spearman"), method = "number")
corrplot(cor(dataset_worst,method="spearman"), method = "number")

#check graphs for split datasets (for easier visualization)
ggpairs(dataset_mean)
ggpairs(dataset_se)
ggpairs(dataset_worst)

###############################################
################### PCA #######################
###############################################
PCAdataset = subset(dataset, select = -c(diagnosis, num_diagnosis))

#PCA Code
PCA_Plot = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

PCA_Plot_Psyc = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Psyc_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  print(loadings)
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

#Test KMO Sampling Adequacy

library(psych)
KMO(PCAdataset)
#Overall MSA =  0.83

#Test Bartlett's Test of Sphericity
library(REdaS)
bart_spher(PCAdataset)
#p-value < 2.22e-16


#Test for Reliability Analysis using Cronbach's Alpha
library(psych)
alpha(PCAdataset,check.keys=TRUE)
#raw_alpha = 0.58

library(psych)
comp <- fa.parallel(PCAdataset)
comp

p = prcomp(PCAdataset, center=T, scale=T)
p

#Scree Plot
plot(p)
abline(1, 0)

#Check PCA Summary Information
summary(p)
print(p)

#Eigenvalues Greater than 1
p2$values
table(p2$values> 1)

#PCA Loadings 6 Factors
p2 = psych::principal(PCAdataset, rotate="varimax", nfactors=6, scores=TRUE)
p2
print(p2$loadings, cutoff=.6, sort=T)

#Eigenvalues Greater than 1
p2$values
table(p2$values> 1)


#PCA Loadings 5 Factors
p2 = psych::principal(PCAdataset, rotate="varimax", nfactors=5, scores=TRUE)
p2
print(p2$loadings, cutoff=.6, sort=T)

#PCA loadings 4 factors
p2 = psych::principal(PCAdataset, rotate="varimax", nfactors=4, scores=TRUE)
p2
print(p2$loadings, cutoff=.6, sort=T)

#PCA Loadings 5 Factors
p2 = psych::principal(PCAdataset, rotate="varimax", nfactors=5, scores=TRUE)
p2
print(p2$loadings, cutoff=.6, sort=T)

#PCA Loadings 5 Factors
p2 = psych::principal(PCAdataset, rotate="varimax", nfactors=5, scores=TRUE)
p2
print(p2$loadings, cutoff=.575, sort=T)

#Calculating scores

scores <- p2$scores
scores_1 <- scores[,1]
scores_2 <- scores[,2]
scores_3 <- scores[,3]
scores_4 <- scores[,4]
scores_5 <- scores[,5]

summary(scores_1)

summary(scores_2)

summary(scores_3)

summary(scores_4)

summary(scores_5)

###############################################
#################### FA #######################
###############################################

###############################################
############# Cluster Analysis ################
###############################################

#Perform k-means Cluster Analysis on group project data (Wisconsin Breast Cancer Data)

#remove diagnosis variable from data set so that it is unlabeled. # if using code, plug in correct dataset information.

WS_BreastCancer_data6 <- BreastCancer_data [,c(2:31)]
head(WS_BreastCancer_data6)

#check for multicollinearity (remove correlated variables from the dataset if needed, then use new dataset to do cluster analysis)
cor(WS_BreastCancer_data6)

#K-Means Clustering

#Determining Optimal Number of Clusters
fviz_nbclust(WS_BreastCancer_data6, kmeans,
             method = "gap_stat")

#check elbow method for optimal number of clusters
fviz_nbclust(WS_BreastCancer_data6, kmeans, method = "wss")


#Perform cluster analysis using Kmeans
set.seed(123)
breastCancerData_k2 <- kmeans(WS_BreastCancer_data6, centers=2, iter.max=100, nstart = 25)
breastCancerData_k2

# Cluster size
breastCancerData_k2$size

# Cluster means
breastCancerData_k2$centers

breastCancerData_k2$cluster
print(breastCancerData_k2)


# Visualize the cluster analysis using K-means
fviz_cluster(breastCancerData_k2,data = WS_BreastCancer_data6)

#try again when k is 3
set.seed(123)
breastCancerData_k3 <- kmeans(WS_BreastCancer_data6, centers=3, iter.max=100, nstart = 25)
breastCancerData_k3

# Cluster size
breastCancerData_k3$size

# Cluster means
breastCancerData_k3$centers

print(breastCancerData_k3)

# Visualize the cluster analysis using K-means
fviz_cluster(breastCancerData_k3,data = WS_BreastCancer_data6)

###############################################
############# Logistic Regression #############
###############################################
#install necessary packages
library(cluster)
library(factoextra)

#remove ID column from the data set
WS_BreastCancer_data2 <- BreastCancer_Dataset [,c(2:32)]

#Check for missing values
sum(is.na(WS_BreastCancer_data2))

#Show Structure of Dataset
str(WS_BreastCancer_data2, list.len=ncol(WS_BreastCancer_data3))

#Show column Numbers
names(WS_BreastCancer_data2)
is.factor(WS_BreastCancer_data2$diagnosis)
contrasts(WS_BreastCancer_data2$diagnosis)

#convert diagnosis column into a binary variable (dummy variable)
WS_BreastCancer_data2$diagnosis <- ifelse(WS_BreastCancer_data2$diagnosis == "M", 1,0)
str(WS_BreastCancer_data2)
names(WS_BreastCancer_data2)

#separate data based on measurement types and check correlation
breastCancer_mean <- data.frame(WS_BreastCancer_data2[,c(1:11)])
bcMean_cor<-cor(breastCancer_mean)
corrplot(bcMean_cor, method='circle')

breastCancer_se <- data.frame(WS_BreastCancer_data2[,c(1,12:21)])
bcSE_cor<-cor(breastCancer_se)
corrplot(bcSE_cor, method='circle')

breastCancer_worst <- data.frame(WS_BreastCancer_data2[,c(1,22:31)])
bcWorst_cor<-cor(breastCancer_worst)
corrplot(bcWorst_cor, method='circle')

################################################################################################################################
#logistic regression

#Scale the data to use to check the VIF on an itial linear model before performing logistic regression
scaled_BreastCancer_data<- as.data.frame(scale(WS_BreastCancer_data2))

#check for multicollinearity
#linear model to check VIF
scaled_breastCancerData_model1<- lm(diagnosis~., data = scaled_BreastCancer_data)
car::vif(scaled_breastCancerData_model1)

#check correlation plot for the entire dataset
BC_cor<-cor(WS_BreastCancer_data2)
library(corrplot)
corrplot(BC_cor, method='circle')


#Try logistic regression with all variables dispite multicollinearity
#set.seed(123)
#breastCancerData_model1<- glm(diagnosis~., family = binomial(), data = WS_BreastCancer_data2)
#summary(breastCancerData_model1)
#breastCancerData_model1$rank
#car::vif(breastCancerData_model1)

#The model created presented overfitting due to near perfect multicollinearity with many of the variables.
#So we should remove these variables

# Remove correlated variables from the dataset
names(WS_BreastCancer_data2)
clean_breastCancerData<- WS_BreastCancer_data2 [,c(1,6,10,13,16,20,21,30)]
names(clean_breastCancerData)

#separate data into training and testing
train<- clean_breastCancerData[1:445,]
test<- clean_breastCancerData[446:556,]

#Try logistic regression on clean data
set.seed(123)
BreastCancerData_model2 <- glm(diagnosis~., family = binomial(), data = train)
summary(BreastCancerData_model2)
car::vif(BreastCancerData_model2)


#Use automated variable selection
library(MASS)
breastCancer_var_selection <- step(BreastCancerData_model2, direction = "backward")
breastCancer_var_selection$coefficients
breastCancer_var_selection$anova

#use model from backward selection to perform logistic regression
BreastCancerData_model3 <- glm(diagnosis ~ smoothness_mean + texture_se + symmetry_se + symmetry_worst, data = clean_breastCancerData )
summary(BreastCancerData_model3)
BreastCancerData_model4$formula

#use cross validation to check final model.
library(DAAG)
cv.binary(BreastCancerData_model3,nfolds =10)


#Code Source used to assess logisic regression: https://datascienceplus.com/perform-logistic-regression-in-r/

#analyze table of deviance
anova(BreastCancerData_model2, test = "Chisq")

#assess the fit of the model
install.packages("pscl")
library(pscl)
pR2(BreastCancerData_model3)

#assess the predictability of the model

library(ROCR)
predicted <- predict(BreastCancerData_model3, test, type="response")  # predicted scores
pr <- prediction(predicted, test$diagnosis)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# ROC (Receiver Operating Characteristics Curve, another view)
plotROC(test$diagnosis, predicted)


library(InformationValue)
optCutOff <- optimalCutoff(test$diagnosis, predicted)
optCutOff

# misclassification error 
misClassError(test$diagnosis, predicted, threshold = optCutOff)

confusionMatrix(test$diagnosis, predicted, threshold = optCutOff)

###############################################
#################### LDA ######################
###############################################

#import MASS package used for LDA analysis
library(MASS)

#show names of smaller dataset (removing numerical 'diagnosis' variable)
names(dataset[1:31])

#create LDA model using 'lda' function from MASS package
LDA <- lda(diagnosis ~ ., data=dataset[1:31])
LDA

#plot the two different dignosis types
plot(LDA)

#make diagnosis predictions using original dataset
p = predict(LDA, newdata=dataset[,1:31])$class
p

#compare the results of the prediction (Confusion Matrix)
table(p, dataset$diagnosis)

#calculates prediction accuracy of the LDA model
mean(p == dataset$diagnosis)
#96.48%

#returns the coefficients for decision boundry line
coef(LDA_CV)
