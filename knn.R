library(class)

df=wine
df$V1 = factor(df$V1)

result = c()
for(k in 1:30){
  idxs <- sample(1:nrow(iris),as.integer(0.5*nrow(df)))
  trainWine <- df[idxs,]
  testWine <- df[-idxs,]
    
  train=trainWine[,2:14]
  test=testWine[,2:14]
  cl_train=trainWine[,1]
  cl_test=testWine[,1]

  nn3 <- knn(train,test,cl=cl_train,k=16)

    
  cm3 = table(nn3,cl_test)
  s = sum(diag(cm3))/sum(cm3)
    
  result <- append(result, s)
  
}

sd(result)
mean(result)



