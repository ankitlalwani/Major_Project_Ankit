#CLASSIFICATION

#####Training data ###########
pat_ind <- sample(1:nrow(patient),0.8*nrow(patient)) 
train <- new_pat[pat_ind,]
test <- new_pat[-pat_ind,]

#############Decision Tree ################
# decision tree using party package 
ctree <- party::ctree(class~.,train) 
plot(tree)

#decision tree using rpart package
tree_rp <- rpart::rpart(class~.,train, method = "class", minsplit = 1, minbucket = 1, cp = 0.001) 

#decision tree using tree package:
tree <- tree::tree(class~.,train)

############Random Forest ##################
rf <- randomForest(class~., data = train[,c(1:6,10:23)], importance=TRUE, ntree=500) 
library(inTrees)
# extract rules from rf model
rf_rules <- extractRules(RF2List(pat_rf),new_pat[,c(1:6,10:22)])

############# KNN ##############
control <- trainControl(method="repeatedcv", number=10, repeats=3) 
# 10 fold cross validation 
control_knn <- trainControl(method="cv", number=10) 

# 10 fold cross validation 
clus<-makeCluster(spec=8,type="PSOCK")
registerDoParallel(clus)
pat_knn <- train(class ~ ., data = train, method = "knn", trControl=control_knn,tuneLength = 20) 
stopCluster(clus)

################ Neural Network ############ # using caret package 
clus<-makeCluster(spec=8,type="PSOCK") registerDoParallel(clus)
pat_nn <- train(class ~ ., data = train,method="mlp",metric='Accuracy',tuneGrid=expand.grid(.size=1:15))
stopCluster(clus)

#another NN models
# scale
preprocessParams <- preProcess(tn[,1:22], method=c("scale")) 
tn2 <- predict(preprocessParams, tn[,1:22])
tn2$class <- new_pat$readmitted

# normalize
preprocessParams2 <- preProcess(tn[,1:22], method=c("range")) 
tn3 <- predict(preprocessParams2, tn[,1:22])
tn3$class <- tn$readmitted
tn_train <- tn3[pat_ind,] 
tn_test <- tn3[-pat_ind,]

#build another NN package
library(neuralnet)
n <- names(tn_train)
f <- as.formula(paste('class ~', paste(n[!n %in% "class"], collapse = " + "))) 
nnnn <- neuralnet(f,data=tn_train,hidden=15,linear.output=FALSE)
xnnx <- compute(nnnn,tn_test[1:22]) 
xnnx
plot(nnnn)

# another NN package
library(nnet)
ideal <- class.ind(tn$readmitted)
ANN = nnet(tn[pat_ind,1:22], ideal[pat_ind,], size=10) 
nnp <- predict(ANN, tn[-pat_ind,1:22], type="class") 
caret::confusionMatrix(tn_test$class,nnp)

############### valdiating models #################
#valdiate tree
val_tree <- predict(tree, test) 
table(val == test[,23])
val_rp <- predict(tree_rp, test, type= "class") 
table(val_rp == test[,23])
val_ctree <- predict(ctree, test) 
table(val_ctree == test[,23])
val_knn2 <- predict(pat_knn2, test)

# validate knn
val_knn <- predict(pat_knn, test)
table(val_knn == test[,23])
val_knn2 <- predict(pat_knn2, test) 
table(val_knn2 == test[,23])

# validate random forest 
val_rf <- predict(pat_rf, test) 
table(val_rf == test[,23])

# validate NN
val_nn <- predict(pat_nn, test) 
val_nnnn <- predict(nnnn, test2) 
table(val_nn == test[,23])