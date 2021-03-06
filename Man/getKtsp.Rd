% Generated by roxygen2
\name{getKtsp}
\alias{getKtsp}
\title{getKtsp}


\usage{
getKtsp(train, Y, k)
}
\arguments{
 \item{train}{training data, gene by sample} 
 \item{Y}{class label for subjects, must be a numeric vector or factor} 
  \item{k}{number of k tsps to be selected, can be arbitrary or selected via cross validation or p-value detection} 
  }
  
  \value{ 
   \item{tsp}{a matrix of gene indeces for each top scoring pair corresponding to the input train matrix} 
   }
 \description{ 
 A function that to rank pairs of genes to return the top k pairs of genes
 }

\author{ 
Kelly Cahill, Masaki Lin, Silvia Liu 
}
