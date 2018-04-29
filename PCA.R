##########################################################
#========= PCA ==========
##########################################################

A1 = prcomp(train_set_scaled[,2:14])

#summary of the results
summary(A1)
plot(A1, type="l", main = "Principal Component Analysis")

#the training data is going to be the first two PCs
train.data = data.frame(A1$x)
train.data = train.data[,1:2]
train.data$Class = training_set$Class

#the test data
test.data = predict(A1, newdata = test_set_scaled[,2:14])
test.data = as.data.frame(test.data)
test.data = test.data[,1:2]
test.data$Class = test_set$Class

L4 = list()
totw4 = list()

for (i in 1:100) {
  set.seed(i)
  L4[[i]] = cclust(as.matrix(train.data)[,1:2], 3, method = "kmeans", dist = "euclidean")
  totw4[[i]] = sum(L4[[i]]$withinss)
}

min_ss4 = min(unlist(totw4))

for (i in 1:100){
  if (totw4[[i]] == min_ss4){
    pred_train4 = predict(L4[[i]], newdata = as.matrix(train.data)[,1:2])
    pred_test4 = predict(L4[[i]], newdata = as.matrix(test.data)[,1:2])
    print(i)
    print(table(training_set[,1],pred_train4$cluster))
    print(table(test_set[,1],pred_test4$cluster))
  }
}

#L4[[3]] was chosen
chosen_pred4train = predict(L4[[3]], newdata = as.matrix(train.data)[,1:2])
chosen_pred4test = predict(L4[[3]], newdata = as.matrix(test.data)[,1:2])

table(training_set[,1],chosen_pred4train$cluster)
table(test_set[,1], chosen_pred4test$cluster)

L4[[3]]$centers

#jitter plot
class1train_pca = subset(train.data, train.data[,3] == 1)
class2train_pca = subset(train.data, train.data[,3] == 2)
class3train_pca = subset(train.data, train.data[,3] == 3)

class1train_pca$sse = apply(class1train_pca[,c(1,2)], 1, function(x) sum( (x-L4[[3]]$centers[1,])^2 ))
class2train_pca$sse = apply(class2train_pca[,c(1,2)], 1, function(x) sum( (x-L4[[3]]$centers[2,])^2 ))
class3train_pca$sse = apply(class3train_pca[,c(1,2)], 1, function(x) sum( (x-L4[[3]]$centers[3,])^2 ))

sse_train_pca = rbind(class1train_pca, class2train_pca, class3train_pca)

sse_train_pca$cluster = jitter(chosen_pred4train$cluster)
sse_train_pca$Class = cut(sse_train_pca$Class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3)) #for better coloring
jitplot_train_pca = qplot(cluster, sse, data = sse_train_pca, color=Class, alpha = I(2/3), size = I(10))
jitplot_train_pca + coord_cartesian(ylim=c(0, .8)) + scale_y_continuous(breaks=seq(0, .8, .1)) +
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Distance from Centroid") +
  ggtitle("Distance from Closest Cluster Centroid - Training Set")

#similarly, we want a jitter plot for the test set
class1test_pca = subset(test.data, test.data[,3] == 1)
class2test_pca = subset(test.data, test.data[,3] == 2)
class3test_pca = subset(test.data, test.data[,3] == 3)

class1test_pca$sse = apply(class1test_pca[,c(1,2)], 1, function(x) sum( (x-L4[[3]]$centers[1,])^2 ))
class2test_pca$sse = apply(class2test_pca[,c(1,2)], 1, function(x) sum( (x-L4[[3]]$centers[2,])^2 ))
class3test_pca$sse = apply(class3test_pca[,c(1,2)], 1, function(x) sum( (x-L4[[3]]$centers[3,])^2 ))

sse_test_pca = rbind(class1test_pca, class2test_pca, class3test_pca)

sse_test_pca$cluster = jitter(chosen_pred4test$cluster)
sse_test_pca$Class = cut(sse_test_pca$Class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3)) #for better coloring
jitplot_test_pca = qplot(cluster, sse, data = sse_test_pca, color=Class, alpha = I(2/3), size = I(10))
jitplot_test_pca + coord_cartesian(ylim=c(0, .8)) + scale_y_continuous(breaks=seq(0, .8, .1)) +
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Distance from Centroid") +
  ggtitle("Distance from Closest Cluster Centroid - Test Set")


#PLOTS WITH CLUSTERS
l = pred_train4$cluster
m = pred_test4$cluster

library(clusterSim)
clusplot(train_set_scaled[,2:14], l, color=T, shade=T, labels = 2, lines = 0, main = "Cluster Analysis - Training Set")
clusplot(test_set_scaled[,2:14], m, color=T, shade=T, labels = 2, lines = 0, main = "Cluster Analysis - Test Set")

#pairwise plot
pairs(A1$x[,1:7], col = rainbow(3)[train_set_scaled[,1]], asp = 1)

library(scatterplot3d)
scatterplot3d(A1$x[,c(1,2,3)], color=rainbow(3)[train_set_scaled[,1]])
