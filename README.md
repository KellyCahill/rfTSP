# rfTSP

This is an R package for robust binary/multi classification and feature selection using gene expression profiles.

# Required packages
* library(randomForest)
* library(doParallel) 
* registerDoParallel()
* library(foreach) 
* library(truncnorm)

## Install this package from github 
* in R console: 
```R 
library(devtools) 
install_github("KellyCahill/rfTSP") 
library(rfTSP)
```

# Example 

```
Gnull=900
Gsig=100
Ntrain=c(70,130)
Ntest=c(60,40)
DAT = simu.multi(Gnull, Gsig, Ntrain, Ntest, MuShift = U[u], Clow = .2, Cup = .5,
               seed = 15232, label = c(-1,1))
train<-DAT$Xtrain
test<-DAT$Xtest
Y<-DAT$Ytrain
true<-DAT$Ytest
N<-length(Y)
controls<-which(Y == "1") 
case<-which(Y == "2")
ind<-t(combn(nrow(train), 2))
 
  
p<-c()
for(j in 1:nrow(ind)){
  p[j]<-binom_2proptest(j, data = train, N = N, controls =  controls, case = case, ind = ind)
}
  
k<-length(which(p < .005)) 
    
tsp<-getKtsp(train, Y, k) 

dichotomized_train<-data_transform(train, tsp) 
dichotomized_test<-data_transform(test, tsp) 
##Assumes training and testing have the same genes in the same order
fit<-randomForest(as.factor(Y)~., data = dichotomized_train, ntree = 500, replace = FALSE) 
predict_label<-predict(fit, newdata = dichotomized_test, type = "response") 
error<-length(which(predict_label !=true ))/length(true)

}
