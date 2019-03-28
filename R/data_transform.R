#Dichotomize data 

data_transform<-function(train, tsp){
levels <- train[as.numeric(tsp[,1]),]-train[as.numeric(tsp[,2]),]
levels <- matrix(unlist(levels), ncol = ncol(levels), nrow=nrow(levels))
data.trans <- data.frame(t(ifelse(levels >0, replace(levels,values = 1), replace(levels, values = 0))))
return(data.trans)
}

