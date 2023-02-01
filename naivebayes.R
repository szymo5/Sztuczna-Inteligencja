library(naivebayes)

dane = wine
dane$V1 = factor(dane$V1)

q <- c()

for(i in 1:30){
  idx = sample(2,178,replace=T, c(0.5,0.5))
  train=dane[idx==1,]
  test=dane[idx==2,]
  
  model=naive_bayes(V1 ~ .,data=train, usekernel = T)
  p=predict(model,test)
  
  cm=table(p,test$V1)
  s = sum(diag(cm))/sum(cm)
  
  q <- append(q, s)
}
sd(q)
mean(q)
