#binomial 2 prop test 
#ind is pair index (see TSP selection code)
binom.p.dist<-function(j, data, subjects, controls, case, ind){
  p.control<-c()
  p.case<-c()   
  for(i in 1:length(subjects)){
    
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
    ##I believe this is doing the exact test. You can check the prop.test for approx normal
    p.value<-bin.test$p.value
  }
  
  return(p.value)
}
