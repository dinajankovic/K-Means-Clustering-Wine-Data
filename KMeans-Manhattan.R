############################################
#==== K MEANS WITH MANHATTAN DISTANCE =====#
############################################

############################
#First we deal with RAW DATA
############################

L1_manh = list()
totw1_manh = list()

for (i in 1:100) {
  set.seed(i)
  L1_manh[[i]] = cclust(as.matrix(training_set[,2:14]), 3, method = "kmeans", dist = "manhattan")
  totw1_manh[[i]] = sum(L1_manh[[i]]$withinss)
}

min_ss_manh = min(unlist(totw1_manh))

for (i in 1:100){
  if (totw1_manh[[i]] == min_ss_manh){
    pred_train1_manh = predict(L1_manh[[i]], newdata = as.matrix(training_set[,2:14]))
    pred_test1_manh = predict(L1_manh[[i]], newdata = as.matrix(test_set[,2:14]))
    print(i)
    print(table(training_set[,1],pred_train1_manh$cluster))
    print(table(test_set[,1],pred_test1_manh$cluster))
  }
}

#we choose L1_manh[[30]] as the best clustering among the obtained clusterings
chosen_pred1train_manh = predict(L1_manh[[30]], newdata = as.matrix(training_set[,2:14]))
chosen_pred1test_manh = predict(L1_manh[[30]], newdata = as.matrix(test_set[,2:14]))

table(training_set[,1],chosen_pred1train_manh$cluster)
table(test_set[,1], chosen_pred1test_manh$cluster)

L1_manh[[30]]$centers

#jitter plot for the training set
class1train_raw_manh = subset(training_set, training_set[,1] == 1)
class2train_raw_manh = subset(training_set, training_set[,1] == 2)
class3train_raw_manh = subset(training_set, training_set[,1] == 3)

class1train_raw_manh$sse = apply(class1train_raw_manh[,2:14], 1, function(x) sum( abs(x - L1_manh[[30]]$centers[1,]) ))
class2train_raw_manh$sse = apply(class2train_raw_manh[,2:14], 1, function(x) sum( abs(x - L1_manh[[30]]$centers[2,]) ))
class3train_raw_manh$sse = apply(class3train_raw_manh[,2:14], 1, function(x) sum( abs(x - L1_manh[[30]]$centers[3,]) ))

sse_train_raw_manh = rbind(class1train_raw_manh, class2train_raw_manh, class3train_raw_manh)

sse_train_raw_manh$cluster = jitter(chosen_pred1train_manh$cluster)
sse_train_raw_manh$Class = cut(sse_train_raw_manh$Class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3)) #for better coloring
jitplot_train_raw_manh = qplot(cluster, sse, data = sse_train_raw_manh, color=Class, alpha = I(2/3), size = I(10))
jitplot_train_raw_manh + coord_cartesian(ylim=c(0, 800)) + scale_y_continuous(breaks=seq(0, 800, 40)) +
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Distance from Centroid") +
  ggtitle("Distance from Closest Cluster Centroid - Training Set")

#jitter plot for the test set
class1test_raw_manh = subset(test_set, test_set[,1] == 1)
class2test_raw_manh = subset(test_set, test_set[,1] == 2)
class3test_raw_manh = subset(test_set, test_set[,1] == 3)

class1test_raw_manh$sse = apply(class1test_raw_manh[,2:14], 1, function(x) sum( abs(x - L1_manh[[30]]$centers[1,]) ))
class2test_raw_manh$sse = apply(class2test_raw_manh[,2:14], 1, function(x) sum( abs(x - L1_manh[[30]]$centers[2,]) ))
class3test_raw_manh$sse = apply(class3test_raw_manh[,2:14], 1, function(x) sum( abs(x - L1_manh[[30]]$centers[3,]) ))

sse_test_raw_manh = rbind(class1test_raw_manh, class2test_raw_manh, class3test_raw_manh)

sse_test_raw_manh$cluster = jitter(chosen_pred1test_manh$cluster)
sse_test_raw_manh$Class = cut(sse_test_raw_manh$Class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3))
jitplot_test_raw_manh = qplot(cluster, sse, data = sse_test_raw_manh, color=Class, alpha = I(2/3), size = I(10))
jitplot_test_raw_manh + coord_cartesian(ylim=c(0, 800)) + scale_y_continuous(breaks=seq(0, 800, 40)) +
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Distance from Centroid") +
  ggtitle("Distance from Closest Cluster Centroid - Test Set")

#################################
#Second, we deal with SCALED DATA
#################################

L2_manh = list()
totw2_manh = list()

for (i in 1:100) {
  set.seed(i)
  L2_manh[[i]] = cclust(as.matrix(train_set_scaled[,2:14]), 3, method = "kmeans", dist = "manhattan")
  totw2_manh[[i]] = sum(L2_manh[[i]]$withinss)
}

min_ss2_manh = min(unlist(totw2_manh))

for (i in 1:100){
  if (totw2_manh[[i]] == min_ss2_manh){
    pred_train2_manh = predict(L2_manh[[i]], newdata = as.matrix(train_set_scaled[,2:14]))
    pred_test2_manh = predict(L2_manh[[i]], newdata = as.matrix(test_set_scaled[,2:14]))
    print(i)
    print(table(training_set[,1],pred_train2_manh$cluster))
    print(table(test_set[,1],pred_test2_manh$cluster))
  }
}

chosen_pred2train_manh = predict(L2_manh[[4]], newdata = as.matrix(train_set_scaled[,2:14]))
chosen_pred2test_manh = predict(L2_manh[[4]], newdata = as.matrix(test_set_scaled[,2:14]))

table(train_set_scaled[,1], chosen_pred2train_manh$cluster)
table(test_set_scaled[,1], chosen_pred2test_manh$cluster)

L2_manh[[4]]$centers

#graphical representation - jitter plots
class1train_scaled_manh = subset(train_set_scaled, train_set_scaled[,1] == 1)
class2train_scaled_manh = subset(train_set_scaled, train_set_scaled[,1] == 2)
class3train_scaled_manh = subset(train_set_scaled, train_set_scaled[,1] == 3)

class1train_scaled_manh$sse = apply(class1train[,2:14], 1, function(x) sum( abs(x - L2_manh[[4]]$centers[1,]) ))
class2train_scaled_manh$sse = apply(class2train[,2:14], 1, function(x) sum( abs(x - L2_manh[[4]]$centers[2,]) ))
class3train_scaled_manh$sse = apply(class3train[,2:14], 1, function(x) sum( abs(x - L2_manh[[4]]$centers[3,]) ))

sse_train_scaled_manh = rbind(class1train_scaled_manh, class2train_scaled_manh, class3train_scaled_manh)

sse_train_scaled_manh$cluster = jitter(chosen_pred2train_manh$cluster)
sse_train_scaled_manh$Class = cut(sse_train_scaled_manh$Class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3))
jitplot_train_scaled_manh = qplot(cluster, sse, data = sse_train_scaled_manh, color=Class, alpha = I(2/3), size = I(10))
jitplot_train_scaled_manh + coord_cartesian(ylim=c(0, 5)) + scale_y_continuous(breaks=seq(0, 5, .5)) +
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Distance from Centroid") +
  ggtitle("Distance from Closest Cluster Centroid - Training Set")

#similary, we want a jitter plot for the test data
class1test_scaled_manh = subset(test_set_scaled, test_set_scaled[,1] == 1)
class2test_scaled_manh = subset(test_set_scaled, test_set_scaled[,1] == 2)
class3test_scaled_manh = subset(test_set_scaled, test_set_scaled[,1] == 3)

class1test_scaled_manh$sse = apply(class1test_scaled_manh[,2:14], 1, function(x) sum( abs(x-L2_manh[[4]]$centers[1,]) ))
class2test_scaled_manh$sse = apply(class2test_scaled_manh[,2:14], 1, function(x) sum( abs(x-L2_manh[[4]]$centers[2,]) ))
class3test_scaled_manh$sse = apply(class3test_scaled_manh[,2:14], 1, function(x) sum( abs(x-L2_manh[[4]]$centers[3,]) ))

sse_test_scaled_manh = rbind(class1test_scaled_manh, class2test_scaled_manh, class3test_scaled_manh)

sse_test_scaled_manh$cluster = jitter(chosen_pred2test_manh$cluster)
sse_test_scaled_manh$Class = cut(sse_test_scaled_manh$Class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3))
jitplot_test_scaled_manh = qplot(cluster, sse, data = sse_test_scaled_manh, color=Class, alpha = I(2/3), size = I(10))
jitplot_test_scaled_manh + coord_cartesian(ylim=c(0, 5)) + scale_y_continuous(breaks=seq(0, 5, .5)) +
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Distance from Centroid") +
  ggtitle("Distance from Closest Cluster Centroid - Test Set")


##################################
#Third, we deal with WHITENED DATA
##################################

L3_manh = list()
totw3_manh = list()

for (i in 1:100) {
  set.seed(i)
  L3_manh[[i]] = cclust(as.matrix(K)[,1:13], 3, method = "kmeans", dist = "manhattan")
  totw3_manh[[i]] = sum(L3_manh[[i]]$withinss)
}

min_ss3_manh = min(unlist(totw3_manh))

for (i in 1:100){
  if (totw3_manh[[i]] == min_ss3_manh){
    pred_train3_manh = predict(L3_manh[[i]], newdata = as.matrix(K)[,1:13])
    pred_test3_manh = predict(L3_manh[[i]], newdata = as.matrix(P)[,1:13])
    print(i)
    print(table(training_set[,1],pred_train3_manh$cluster))
    print(table(test_set[,1],pred_test3_manh$cluster))
  }
}

chosen_pred3train_manh = predict(L3_manh[[32]], newdata = as.matrix(K)[,1:13])
chosen_pred3test_manh = predict(L3_manh[[32]], newdata = as.matrix(P)[,1:13])

table(training_set[,1],chosen_pred3train_manh$cluster)
table(test_set[,1], chosen_pred3test_manh$cluster)

L3_manh[[32]]$centers
