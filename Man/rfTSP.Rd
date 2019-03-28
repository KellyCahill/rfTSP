\name{rfTSP}
\alias{rfTSP}
\title{rfTSP}

\usage{
simu.multi(Gnull, Gsig, Ntrain, Ntest, SDgenemu = 5, DFgenesd = 1, Clow = 2, Cup = Inf, 
MuShift, SDshift = 0, SDshift = 0.5, seed = 15232, label = c(-1,1)) 
}
\arguments{
  \item{list1}{Gnull - number of null genes}
  \item{list2}{Gsig - number of significant genes}
   \item{list3}{Ntrain - a vector for number of training samples for each class (can be multi-class)}
   \item{list4}{Ntest - a vector for number of testing samples (same length with Ntrain)}
   \item{list5}{SDgenemu - SD for null gene mu value}
  \item{list6}{DFgenesd - degree of free for null gene SD}
   \item{list7}{Clow - significant gene shift lower bound}
  \item{list8}{Cup - significant gene shift upper bound}
  \item{list9}{MuShift - test shift mean value}
  \item{list10}{SDshift - test shift SD value}
   \item{list11}{seed - seed for random number}
   }
  
  \value{
  \item{list1}{Xtrain - training expression table, gene by sample}
 \item{list2}{Ytrain - training outcome label}
  \item{list3}{Xtest - testing expression table, gene by sample}
 \item{list4}{Ytest - testing outcome label}
 \item{list5}{sigIndex - significant gene index, TRUE for sig and FALSE for non-sig}
 \item{list6}{geneMU - gene mu value, a gene-by-group matrix}
 \item{list7}{gneeSD - gene SD value, a vector with length of gene} 
 } 
\description{
A function for generating simulated gene expression data
}
\usage{
getKtsp(train, Y, k)
}
\arguments{
 \item{list1}{train - training data, gene by sample} 
 \item{list2}{Y - class label for subjects, must be a numeric vector or factor} 
  \item{list3}{k - number of k tsps to be selected, can be arbitrary or selected via cross validation or p-value detection} 
  }
  
  \values{ 
   \item{list1}{tsp - a matrix of gene indeces for each top scoring pair corresponding to the input train matrix} 
   }
 \description{ 
 A function that to rank pairs of genes to return the top k pairs of genes
 }
 \usage{ 
 data_transform(data, tsp)
 } 
 \arguments{
 \item{list1}{data- training or testing data, gene by sample} 
 \item{list2}{tsp - object out from getKtsp function} 
 } 
 
\values{ 
\item{transformed data matrix ready for input into random forest containing 0 and 1 values, sample by pair} 
} 
\description{
A function that dichotomizes an expression matrix into 0 and 1 values, according to the observed TSP pattern
} 

\examples{ 
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
tsp<-getKtsp(train, Y, k = 20) 

dichotomized_train<-data_transform(train, tsp) 
dichotomized_test<-data_transform(test, tsp) 
##Assumes training and testing have the same genes in the same order
fit<-randomForest(as.factor(Y)!., data = dichotomized_train, ntree = 500, replace = FALSE) 
predict_label<-predict(fit, newdata = dichotomized_test, type = "response") 
} 

\authors{ 
Kelly Cahill, Masaki Lin, Silvia Liu 
}



 
