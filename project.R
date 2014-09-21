library(caret)

setwd("C:/Users/martin/Desktop/Data Science/Practical Machine learning/project")

## there are a large number of blank spaces that should be interpreted as NAs
training<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                   na.strings=c(NA, ""))
testing<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
               na.strings=c(NA, ""))

prenames<-names(training)
## partitioning the data set

## K-FOLDS CROSS-VALIDATION. EL TEST SET SERÁ EL DEL SUBMIT DE COURSERA

## EXPLORATORY ANALYSIS

## Missing values
sum(is.na(data))

## columns with missing values
apply(training, 2, function(x){sum(is.na(x))})

## there are several columns all with a number of NAs equal to 19216, that is about only
## 2% of the number of observations. 


nonNAcolumns<-names(training)[apply(training, 2, function(x) sum(is.na(x)))<19216]
                                
training<-training[,nonNAcolumns]

## the same for the testing set. We have to drop "classe" from nonNAcolumns vector first
testing<-testing[,nonNAcolumns[-length(nonNAcolumns)]]

## Dropping the first column, X, since it is an identifier of an observation
training<-training[,-1]
testing<-testing[,-1]

## hour/date column
## this column shows the time when the activity was perfomed for each individual. The only
## reason this column is not perfectly correlated with user_name is because a minute switch
## ocurred during the recording of the session. the timestamp variables also correspond to
## time. we can also drop these variables

with(training, table(user_name, cvtd_timestamp))



## we opt for dropping this column
training<-training[,2:4]
testing<-testing[-,2:4]

## Extreme values
figure_extreme<-"C:/Users/martin/Desktop/Data Science/Practical Machine learning/project/figures/extreme"
for (idx in 3:ncol(training)-1){
  png(paste(figure_extreme, "/", idx, ".png", sep=""))
  plot(training[,idx], col=training$classe)
  dev.off()
}

## Extreme in variable: 34, 35, 36, 41, 46, 47, 48, 49
extreme.obs<-c(which.min(training[,34]),
  which.max(training[,35]),
  which.max(training[,36]),
  which.max(training[,37]),
  which.min(training[,41]),
  which.max(training[,46]),
  which.min(training[,47]),
  which.max(training[,48]),
  which.max(training[,49]))
extreme.obs

## are in three different observations: 5373, 9029 and 9274

training<-training[-unique(extreme.obs),]

## Patterns: repeat the same but without extreme values
figure_extreme<-"C:/Users/martin/Desktop/Data Science/Practical Machine learning/project/figures/extreme"
for (idx in 3:ncol(training)-1){
  png(paste(figure_extreme, "/", idx, ".png", sep=""))
  plot(training[,idx], col=training$classe)
  dev.off()
}



## Histograms
figure_path<-"C:/Users/martin/Desktop/Data Science/Practical Machine learning/project/figures/histograms"
for (idx in 3:ncol(training)){
  png(paste(figure_path, "/",idx, ".png", sep=""))
  print(qplot(training[, idx]))
  dev.off()
}

## Boxplots
figure_path<-"C:/Users/martin/Desktop/Data Science/Practical Machine learning/project/figures/boxplots"
for (idx in 3:ncol(training)){
  png(paste(figure_path, "/",idx, ".png", sep=""))
  print(qplot(classe, training[, idx], data=training, geom="boxplot"))
  dev.off()
}

##histograms and class
figure_histClass<-"C:/Users/martin/Desktop/Data Science/Practical Machine learning/project/figures/histclass"
for (idx in 3:ncol(training)){
png(paste(figure_histClass, "/", idx, ".png", sep=""))
print(qplot(training[,idx], data=training, fill=classe))
dev.off()
}

## Coding new_window as a dummy variable
dum<-dummyVars(classe~new_window, data=training)
head(predict(dum, training))

## PCA (sin new_window y classe). We get rid of the first three (non numeric) columns

training<-training[,-c(1:3)]
testing<-testing[,-c(1,3)]


preProc<-preProcess(training[,-53], method="pca", thresh=0.9)
trainPC<-predict(preProc, training[,-53])
testPC<-predict(preProc, testing[,-53])

## Models
modFit1<-train(classe~., data=training, method="rpart", preProcess=c("center", "scale"))
modFit2<-train(classe~., data=training, method="rf", prox=TRUE)

modFit3<-train(training$classe~., data=trainPC, method="rpart")
modFit4<-train(training$classe~., data=trainPC, method="rf", prox=TRUE)

## hard to compute. Subsampling columnsa based on patterns observed in figures
## 4, 7,9, 10, 13, 15, 20, 35, 37, 38, 44, c(4,7,9,10,13,15,20,35,37,38,44)

training2<-training[,c(4,7,9,10,13,15,20,35,37,38,44, ncol(training))]
testing2<-testing[,c(4,7,9,10,13,15,20,35,37,38,44)]

modelFit5<-train(classe~., data=training2, method="rf", prox=TRUE)


## trying with another library
library(randomForest)
modFit6<-randomForest(classe~., data=training2)

## cross validatios
training3<-training[,c(4,7,9,10,13,15,20,35,37,38,44)]
classe<-training$classe

cv<-rfcv(training3, classe, cv.fold=5)



