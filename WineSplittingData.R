#loading the data set
dataset = read.csv("WineDataSet.csv", header = TRUE)

#assigning column names
colnames(dataset) = c("Class", "Alcohol", "Malic_Acid", "Ash", "Ash_Alcalinity", "Magnesium", "Total_Phenols", "Flavanoids",
                      "Nonflavanoid_Phenols", "Proanthocyanins", "Colour_Intensity", "Hue", "OD280/OD315_of_diluted_wines",
                      "Proline")

#the column names can be called without the $ sign, if preferred
attach(dataset)

#######################################################################
#================= SPLITTING DATA =====================================
#######################################################################
set.seed(31417)
library(caTools)

split = sample.split(Class, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

table(training_set$Class)
table(test_set$Class)

#Alternatively, we could use the code below to make sure we have equal distribution
#of each cultivar in both training and test set.

#L = list()
#training_sets = list()
#test_sets = list()

#for (i in 1:3){
#  temp = dataset[which(dataset$Class == i),]
#  L[[i]] = sample.split(temp$Class, SplitRatio = 2/3)
#  training_sets[[i]] = subset(temp, L[[i]] == TRUE)
#  test_sets[[i]] = subset(temp, L[[i]] == FALSE)
#}

#trainingSet = rbind(training_sets[[1]], training_sets[[2]], training_sets[[3]])
#testSet = rbind(test_sets[[1]], test_sets[[2]], test_sets[[3]])

#table(trainingSet$Class)
#table(testSet$Class)

#Visual representation of equal distribution of cultivars in training/test set
A = matrix(0,2,3)
l = list()

for (i in 1:3){
  l[[i]] = c(sum(training_set$Class == i)/nrow(training_set), sum(test_set$Class == i)/nrow(test_set))
  A[,i] = l[[i]]
}

rownames(A) = c("Training Set","Test Set")
colnames(A) = c("Class 1","Class 2","Class 3")

barplot(A, beside = FALSE, col = c("darkblue","red"), ylim = c(0,1), ylab = "Proportion", xlab = "Class", main = "Distribution of classes")
legend("topright", legend = c("Training Set","Test Set"), cex = 0.8, fill=c("darkblue","red"))
