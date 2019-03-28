##find k TSPs in binary setting 

getKtsp<-function(train, Y, k){
data  <- train
dat  <- t(data)
n <- dim(dat)[1] #number of samples
m <- dim(dat)[2] #number of features

grp_sub <- Y
labels <- as.character(unique(grp_sub))
xx <- as.matrix(cbind(rep(1,n), grp_sub))
hatxx <- solve(t(xx) %*% xx) %*% t(xx) #2 x n matrix #solve just returns the inverse of A
tmp <- foreach(j = c(1:(m-1))) %dopar% {
  yy <- dat[ ,-c(1:j)] < dat[,j] # n x m matrix #compares every gene to each other (pairs)
  t(t((hatxx %*% yy)[2,]))
}

tmp1 <-do.call(rbind, tmp)
out <- tmp1
score <- rowSums(out)
names(score)<-1:length(score)
score_order<-sort(score, decreasing = TRUE)
tmp.thread<-as.numeric(names(score_order))[1:k]
tsp.score <- score[tmp.thread][1:k]
tr.DList<-data
m <- dim(t(tr.DList))[2] #number of features

ind <- foreach(k=1:(m-1)) %dopar% {
  dat  <- t(tr.DList)
  cbind(rep(k, length(k:m)-1), (k+1):m, rep(colnames(dat)[k], length(k:m)-1), colnames(dat)[(k+1):m])
}
ind <- do.call(rbind, ind)
ind <- ind[tmp.thread,]
tsp<-ind
return(tsp)
}

