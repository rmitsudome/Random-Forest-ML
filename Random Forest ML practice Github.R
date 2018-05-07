#Random Forest ML practice

# Wisconsin Breast Cancer Database

#1. Sample code number: id number 
#2. Clump Thickness: 1 - 10 
#3. Uniformity of Cell Size: 1 - 10 
#4. Uniformity of Cell Shape: 1 - 10 
#5. Marginal Adhesion: 1 - 10 
#6. Single Epithelial Cell Size: 1 - 10 
#7. Bare Nuclei: 1 - 10 
#8. Bland Chromatin: 1 - 10 
#9. Normal Nucleoli: 1 - 10 
#10. Mitoses: 1 - 10 
#11. Class: (2 for benign, 4 for malignant)

BreastCancer = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data",
                          sep = ",")

dim(BreastCancer)
head(BreastCancer)
names(BreastCancer) = c("ID", "Clump_Thick", "Cell_Size", "Cell_Shape"
                        , "Adhesion", "Epithelial_Size", "Nuclei",
                        "Chromatin", "Nucleoli", "Mitoses", "Class")

sum(is.na(BreastCancer))

#delete all ? in data set
Question_row = which(BreastCancer == "?", arr.ind = TRUE)
BreastCancer = BreastCancer[-c(Question_row[1:16,]), ]
nrow(BreastCancer)

#change all predictors to numeric
for(i in 2:10) {
  BreastCancer[, i] <- as.numeric(as.character(BreastCancer[, i]))
}

#make Class out of 0 and 1
BreastCancer$Class <- ifelse(BreastCancer$Class == "4", 1, 0)

#change dependent variable to factor
BreastCancer[,11] = as.factor(BreastCancer[,11])

#need to randomly split the data into training and test samples
#Since response variable is binary categorical variable need to
#make sure training data has approximately = proportion of classes.

table(BreastCancer$Class)

library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.


#randomly put 70% of orig. data in train, the rest in test
set.seed(100)
trainIndex = createDataPartition(BreastCancer$Class, 
                                 p=0.7, list = F)  # 70% training data
train = BreastCancer[trainIndex, ]
test = BreastCancer[-trainIndex, ]

table(train$Class) #around 2x of 0 than 1

#Down sampling
#Majority class randomly down sampled to same size as smaller class

set.seed(100)
#Selects all columns but Class for x
#y must be factor variable
down_train <- downSample(x = train[, colnames(train) %ni% "Class"],
                         y = train$Class)
table(down_train$Class)

#Up Sampling
#rows from minority class repeatedly sampled till reaches 
#equal size as majority class

set.seed(100)
up_train <- upSample(x = train[, colnames(train) %ni% "Class"],
                     y = train$Class)
table(up_train$Class)

#for this example, will use down_train as training data
train = down_train

#test function with train data
install.packages("randomForest")
library(randomForest)
output.forest <- randomForest(Class ~ Clump_Thick + Cell_Size + Cell_Shape
                              + Adhesion + Epithelial_Size + Nuclei
                              + Chromatin + Nucleoli + Mitoses, 
                              data = train)
print(output.forest) #success rate = .985

#if you drop variable, shows how much predic. power reduces by
varImpPlot(output.forest) 

p = predict(output.forest, test)
table(test[,11], p) #0.9655 as well

