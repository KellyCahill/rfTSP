#binomial 2 prop test 
#returns p-value strength of pair
binom.p.dist<-function(j, data, N, controls, case, ind){
  p.control<-c()
  p.case<-c()   
  for(i in 1:N){
    
    p.control[i]<- data[as.numeric(ind[j,1]), controls[i]] >= data[as.numeric(ind[j,2]), controls[i]]
    p.case[i]<- data[as.numeric(ind[j,1]), case[i]] > data[as.numeric(ind[j,2]), case[i]]
  }
  
  p1<-length(which(p.control)==TRUE)
  p2<-length(which(p.case)==TRUE)
  
  table<-matrix(c(p1, length(controls)-p1, p2,length(case)-p2 ), ncol = 2, nrow =2)
  
  if(p1 == 0 & p2 == 0 | p1 == length(controls) & p2 == length(case)){
    
    p.value <-"NA"
    
  } else {
    
    bin.test<-prop.test(x= table, alternative = "two.sided", conf.level = .95)
    p.value<-bin.test$p.value
  }
  
  return(p.value)
}
