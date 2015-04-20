# Functions for different methods

sdTest <- function(binary.response.matrix) {
  
  # sdTest : Standard deviation of total test.
  #          Calculated using point biserial correlation.
  #
  # input: binary.response.matrix : Binary responses to all items.
  
  proportions.correct <- apply(binary.response.matrix, 2, mean, na.rm = T) # Proportion correct per item
  sum.scores          <- apply(binary.response.matrix, 1, sum,  na.rm = T) # Total sum score per user
  
  # Create emty vector to store item total correlations
  r_it = vector()
  
  # Determine number of items in response matrix
  number.of.items = dim(binary.response.matrix)[2]
  
  # Calculate point biserial correlation per item with total sum score
  for(i in 1:number.of.items) { 
    
    r_it[i] =  biserial.cor(sum.scores, binary.response.matrix[,i], level=1, use = "complete.obs") 
    
  }
  
  # Calculate standard deviation of the test
  SD.test <- sum(r_it*sqrt(proportions.correct*(1 - proportions.correct)), na.rm = T)
  
  return(SD.test)  
}

kr20 <- function(n, sd.test, probability_vector) {
  
  # kr20: reliability estimate
  #
  # input: n                  : total items in item bank
  #        sd.test            : standard deviation of the test
  #        probability_vector : vector with proportions correct per item
  
  kr20 = (n/(n-1)) * ( 1 - ( sum(probability_vector*(1-probability_vector)) / sd.test^2 ) )
  
  return(kr20)
}

SpearmanBrownPredictionFormula <- function(k, cronbach.alpha) {
  
  # SpearmanBrownPredictionFormula : Mean reliability on extended or shortened test
  #
  # input : k              : factor for the relative increase or decrease of number of items
  #                          in percentages
  #         cronbach.alpha : Pre determined reliability
  
  mean.scaled.alpha <- ( k * cronbach.alpha ) / ( 1 + (k - 1) * cronbach.alpha )
  
  return(mean.scaled.alpha)  
}

Lopez <- function(item.correlation.matrix, number.of.items) {
  
  # Lopez : Correction on Cronbach's alpha based on inter item correlations
  #
  # input : item.correlation.matrix : matrix with all item correlations
  #         number.of.items         : number of items used
  
  inter.item.cor <- item.correlation.matrix[lower.tri(item.correlation.matrix)]
  
  mean.inter.item.cor = mean(inter.item.cor, na.rm = T)
  
  Lopez.alpha = ( number.of.items * mean.inter.item.cor ) / ( 1+(number.of.items - 1)*mean.inter.item.cor )
  
  return(Lopez.alpha)
}
