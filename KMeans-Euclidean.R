#####################################################################
#================= THE K-MEANS ALGORITHM ============================
#####################################################################

summary(dataset)

#How many clusters do we need? Let's find out...

#function to compute total within-cluster sum of square
wcss = function(x) {
  kmeans(dataset[,2:14], x, nstart = 30)$tot.withinss
}

library(purrr)
wcss_val = map_dbl(1:12, wcss)
plot(1:12, wcss_val, type="b", pch = 15, frame = FALSE, xlab="Number of Clusters", ylab="Total WCSS")
#CONCLUSION: we need 3 clusters

############################
#First we deal with RAW DATA
############################

#creating empty lists that we are going to use
L1 = list()
totw1 = list()

library(cclust)
for (i in 1:100) {
  set.seed(i)
  #the cclust function is being used to cluster the data
  L1[[i]] = cclust(as.matrix(training_set[,2:14]), 3, method = "kmeans", dist = "euclidean")
  totw1[[i]] = sum(L1[[i]]$withinss)
}

#finding the minimal total WCSS
min_ss = min(unlist(totw1))

for (i in 1:100){
  if (totw1[[i]] == min_ss){
    pred_train1 = predict(L1[[i]], newdata = as.matrix(training_set[,2:14]))
    pred_test1 = predict(L1[[i]], newdata = as.matrix(test_set[,2:14]))
    print(i)
    print(table(training_set[,1],pred_train1$cluster))
    print(table(test_set[,1],pred_test1$cluster))
  }
}

#we choose L1[[3]]
chosen_pred1train = predict(L1[[3]], newdata = as.matrix(training_set[,2:14]))
chosen_pred1test = predict(L1[[3]], newdata = as.matrix(test_set[,2:14]))

table(training_set[,1],chosen_pred1train$cluster)
table(test_set[,1], chosen_pred1test$cluster)

#the centroids are:
L1[[3]]$centers

class1train_raw = subset(training_set, training_set[,1] == 1)
class2train_raw = subset(training_set, training_set[,1] == 2)
class3train_raw = subset(training_set, training_set[,1] == 3)

#calculating the distance from the centroids
class1train_raw$sse = apply(class1train_raw[,2:14], 1, function(x) sum( (x-L1[[3]]$centers[1,])^2 ))
class2train_raw$sse = apply(class2train_raw[,2:14], 1, function(x) sum( (x-L1[[3]]$centers[2,])^2 ))
class3train_raw$sse = apply(class3train_raw[,2:14], 1, function(x) sum( (x-L1[[3]]$centers[3,])^2 ))

sse_train_raw = rbind(class1train_raw, class2train_raw, class3train_raw)

sse_train_raw$cluster = jitter(chosen_pred1train$cluster)
sse_train_raw$Class = cut(sse_train_raw$Class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3))

library(ggplot2)
jitplot_train_raw = qplot(cluster, sse, data = sse_train_raw, color = Class, alpha = I(2/3), size = I(10))
jitplot_train_raw + coord_cartesian(ylim=c(0, 300000)) + scale_y_continuous(breaks=seq(0, 300000, 10000)) +
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Distance from Centroid") +
  ggtitle("Distance from Closest Cluster Centroid - Training Set")

#similary, we want a jitter plot for the test data
class1test_raw = subset(test_set, test_set[,1] == 1)
class2test_raw = subset(test_set, test_set[,1] == 2)
class3test_raw = subset(test_set, test_set[,1] == 3)

class1test_raw$sse = apply(class1test_raw[,2:14], 1, function(x) sum( (x-L1[[3]]$centers[1,])^2 ))
class2test_raw$sse = apply(class2test_raw[,2:14], 1, function(x) sum( (x-L1[[3]]$centers[2,])^2 ))
class3test_raw$sse = apply(class3test_raw[,2:14], 1, function(x) sum( (x-L1[[3]]$centers[3,])^2 ))

sse_test_raw = rbind(class1test_raw, class2test_raw, class3test_raw)

sse_test_raw$cluster = jitter(chosen_pred1test$cluster)
sse_test_raw$Class = cut(sse_test_raw$Class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3)) #for better coloring
jitplot_test_raw = qplot(cluster, sse, data = sse_test_raw, color=Class, alpha = I(2/3), size = I(10))
jitplot_test_raw + coord_cartesian(ylim=c(0, 300000)) + scale_y_continuous(breaks=seq(0, 300000, 10000)) +
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Distance from Centroid") +
  ggtitle("Distance from Closest Cluster Centroid - Test Set")


#################################
#Second, we deal with SCALED DATA
#################################

mins = sapply(training_set, min)
ranges = sapply(training_set,function(x)diff(range(x)))
train_set_scaled = as.data.frame(scale(training_set, center = mins, scale = ranges))
test_set_scaled = as.data.frame(scale(test_set, center = mins, scale = ranges))

#the Class definitely doesn't have to be scaled, so we retain that column
#from the original unscaled training/test data
train_set_scaled[,1] = training_set[,1]
test_set_scaled[,1] = test_set[,1]

L2 = list()
totw2 = list()

for (i in 1:100) {
  set.seed(i)
  L2[[i]] = cclust(as.matrix(train_set_scaled[,2:14]), 3, method = "kmeans", dist = "euclidean")
  totw2[[i]] = sum(L2[[i]]$withinss)
}

min_ss2 = min(unlist(totw2))

for (i in 1:100){
  if (totw2[[i]] == min_ss2){
    pred_train2 = predict(L2[[i]], newdata = as.matrix(train_set_scaled[,2:14]))
    pred_test2 = predict(L2[[i]], newdata = as.matrix(test_set_scaled[,2:14]))
    print(i)
    print(table(training_set[,1],pred_train2$cluster))
    print(table(test_set[,1],pred_test2$cluster))
  }
}

#we chose L2[[13]] as the most suitable result
chosen_pred2train = predict(L2[[13]], newdata = as.matrix(train_set_scaled[,2:14]))
chosen_pred2test = predict(L2[[13]], newdata = as.matrix(test_set_scaled[,2:14]))

table(train_set_scaled[,1],chosen_pred2train$cluster)
table(test_set_scaled[,1], chosen_pred2test$cluster)

#the centroids are:
L2[[13]]$centers

#graphical representation - jitter plot
#calculating SSE for the training set
class1train = subset(train_set_scaled, train_set_scaled[,1] == 1)
class2train = subset(train_set_scaled, train_set_scaled[,1] == 2)
class3train = subset(train_set_scaled, train_set_scaled[,1] == 3)

class1train$sse = apply(class1train[,2:14], 1, function(x) sum( (x-L2[[13]]$centers[1,])^2 ))
class2train$sse = apply(class2train[,2:14], 1, function(x) sum( (x-L2[[13]]$centers[2,])^2 ))
class3train$sse = apply(class3train[,2:14], 1, function(x) sum( (x-L2[[13]]$centers[3,])^2 ))

sse_train = rbind(class1train, class2train, class3train)

sse_train$cluster = jitter(chosen_pred2train$cluster)
sse_train$Class = cut(sse_train$Class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3)) #for better coloring
jitplot_train = qplot(cluster, sse, data = sse_train, color=Class, alpha = I(2/3), size = I(10))
jitplot_train + coord_cartesian(ylim=c(0, 2)) + scale_y_continuous(breaks=seq(0, 2, .5)) +
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Distance from Centroid") +
  ggtitle("Distance from Closest Cluster Centroid - Training Set")

#similary, we want a jitter plot for the test data
class1test = subset(test_set_scaled, test_set_scaled[,1] == 1)
class2test = subset(test_set_scaled, test_set_scaled[,1] == 2)
class3test = subset(test_set_scaled, test_set_scaled[,1] == 3)

class1test$sse = apply(class1test[,2:14], 1, function(x) sum( (x-L2[[13]]$centers[1,])^2 ))
class2test$sse = apply(class2test[,2:14], 1, function(x) sum( (x-L2[[13]]$centers[2,])^2 ))
class3test$sse = apply(class3test[,2:14], 1, function(x) sum( (x-L2[[13]]$centers[3,])^2 ))

sse_test = rbind(class1test, class2test, class3test)

sse_test$cluster = jitter(chosen_pred2test$cluster)
sse_test$Class = cut(sse_test$Class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3)) #for better coloring
jitplot_test = qplot(cluster, sse, data = sse_test, color=Class, alpha = I(2/3), size = I(10))
jitplot_test + coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 5, .7)) +
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Distance from Centroid") +
  ggtitle("Distance from Closest Cluster Centroid - Test Set")


##################################
#Third, we deal with WHITENED DATA
##################################
library(whitening)
library(steadyICA)

train_set_whitened = whitener(as.matrix(train_set_scaled[,2:14]))

#whitening the test set by using the whitener that was used to whiten the training set
test_set_whitened = as.matrix(test_set_scaled[,2:14]) %*% as.matrix(train_set_whitened$whitener)

K = as.data.frame(train_set_whitened$Z)
K$Class = training_set$Class

P = as.data.frame(test_set_whitened)
P$Class = test_set$Class

L3 = list()
totw3 = list()

for (i in 1:100) {
  set.seed(i)
  L3[[i]] = cclust(as.matrix(K)[,1:13], 3, method = "kmeans", dist = "euclidean")
  totw3[[i]] = sum(L3[[i]]$withinss)
}

min_ss3 = min(unlist(totw3))

for (i in 1:100){
 if (totw3[[i]] == min_ss3){
    pred_train3 = predict(L3[[i]], newdata = as.matrix(K)[,1:13])
    pred_test3 = predict(L3[[i]], newdata = as.matrix(P)[,1:13])
    print(i)
    print(table(training_set[,1],pred_train3$cluster))
    print(table(test_set[,1],pred_test3$cluster))
 }
}

#we chose L3[[75]]
chosen_pred3train = predict(L3[[75]], newdata = as.matrix(K)[,1:13])
chosen_pred3test = predict(L3[[75]], newdata = as.matrix(P)[,1:13])

table(training_set[,1],chosen_pred3train$cluster)
table(test_set[,1], chosen_pred3test$cluster)

#the centroids
L3[[75]]$centers
