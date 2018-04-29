##################################
#===== ICA ======================#
##################################

set.seed(25)

library(caret)
preprocessParams = preProcess(training_set[,2:14], method=c("center", "scale", "ica"), n.comp=13)
print(preprocessParams)

transf = predict(preprocessParams, training_set[,2:14])
summary(transf)

pairs(transf, col = rainbow(3)[train_set_scaled[,1]])

test.data2 = predict(preprocessParams, newdata = test_set[,2:14])
pairs(test.data2, col = rainbow(3)[test_set[,1]])

#plotting the IC6 against itself
plot(transf[,6], transf[,6], col=rainbow(3)[train_set_scaled[,1]], xlab="IC1", ylab="IC1")

#adding a new column "Class"
transf$Class = training_set$Class
test.data2$Class = test_set$Class


M = transf[,c(6,8,14)]
N = test.data2[,c(6,8,14)]

L4_ica = list()
totw4_ica = list()

for (i in 1:100) {
  set.seed(i)
  L4_ica[[i]] = cclust(as.matrix(M)[,c(1,2)], 3, method = "kmeans", dist = "euclidean")
  totw4_ica[[i]] = sum(L4_ica[[i]]$withinss)
}

min_ss4_ica = min(unlist(totw4_ica))

for (i in 1:100){
  if (totw4_ica[[i]] == min_ss4_ica){
    pred_train4_ica = predict(L4_ica[[i]], newdata = as.matrix(M)[,c(1,2)])
    pred_test4_ica = predict(L4_ica[[i]], newdata = as.matrix(N)[,c(1,2)])
    print(i)
    print(table(training_set[,1],pred_train4_ica$cluster))
    print(table(test_set[,1],pred_test4_ica$cluster))
  }
}

chosen_pred4train_ica = predict(L4_ica[[54]], newdata = as.matrix(M)[,c(1,2)])
chosen_pred4test_ica = predict(L4_ica[[54]], newdata = as.matrix(N)[,c(1,2)])

table(training_set[,1],chosen_pred4train_ica$cluster)
table(test_set[,1], chosen_pred4test_ica$cluster)

L4_ica[[54]]$centers

class1train_ica = subset(M, M[,3] == 1)
class2train_ica = subset(M, M[,3] == 2)
class3train_ica = subset(M, M[,3] == 3)

class1train_ica$sse = apply(class1train_ica[,c(1,2)], 1, function(x) sum( (x-L4_ica[[54]]$centers[1,])^2 ))
class2train_ica$sse = apply(class2train_ica[,c(1,2)], 1, function(x) sum( (x-L4_ica[[54]]$centers[2,])^2 ))
class3train_ica$sse = apply(class3train_ica[,c(1,2)], 1, function(x) sum( (x-L4_ica[[54]]$centers[3,])^2 ))

sse_train_ica = rbind(class1train_ica, class2train_ica, class3train_ica)

sse_train_ica$cluster = jitter(chosen_pred4train_ica$cluster)
sse_train_ica$Class = cut(sse_train_ica$Class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3)) #for better coloring
jitplot_train_ica = qplot(cluster, sse, data = sse_train_ica, color=Class, alpha = I(2/3), size = I(10))
jitplot_train_ica + coord_cartesian(ylim=c(0, 10)) + scale_y_continuous(breaks=seq(0, 10, 1)) +
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Distance from Centroid") +
  ggtitle("Distance from Closest Cluster Centroid - Training Set")

#similary, we want a jitter plot for the test data
class1test_ica = subset(N, N[,3] == 1)
class2test_ica = subset(N, N[,3] == 2)
class3test_ica = subset(N, N[,3] == 3)

class1test_ica$sse = apply(class1test_ica[,c(1,2)], 1, function(x) sum( (x-L4_ica[[54]]$centers[1,])^2 ))
class2test_ica$sse = apply(class2test_ica[,c(1,2)], 1, function(x) sum( (x-L4_ica[[54]]$centers[2,])^2 ))
class3test_ica$sse = apply(class3test_ica[,c(1,2)], 1, function(x) sum( (x-L4_ica[[54]]$centers[3,])^2 ))

sse_test_ica = rbind(class1test_ica, class2test_ica, class3test_ica)

sse_test_ica$cluster = jitter(chosen_pred4test_ica$cluster)
sse_test_ica$Class = cut(sse_test_ica$Class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3)) #for better coloring
jitplot_test_ica = qplot(cluster, sse, data = sse_test_ica, color=Class, alpha = I(2/3), size = I(10))
jitplot_test_ica + coord_cartesian(ylim=c(0, 10)) + scale_y_continuous(breaks=seq(0, 10, 2)) +
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Distance from Centroid") +
  ggtitle("Distance from Closest Cluster Centroid - Test Set")
