# Simulations

## Install packages if needed.
if (!"MASS" %in% installed.packages()) { install.packages("MASS") }
if (!"ltm"  %in% installed.packages()) { install.packages("ltm")  }
if (!"xlsx" %in% installed.packages()) { install.packages("xlsx") }

### Load libraries
library("MASS") # Package for generating multivariate normal data
library("ltm")  # Package for calculating cronbach's alpha
library('xlsx') # Package for writing to xlsx files

# Import functions
source(file = "functions.r")

# Run simulations

## Set seed for fixed random sampling
set.seed(1547)

# Set prameters
lb    = 0.25 # lower bound for discrimination on 0 to 1 scale
ub    = 0.55 # upper bound for discrimination on 0 to 1 scale
r     = 1    # residual variance is assumed to have variance of 1 which is common in IRT 

# Set different situations. This can be extended to different scenarios.
N.j = c(500) # Number of test subjects
K.b = c(400) # Number of items in item bank
K.s = c(40)  # Number of items sampled form item bank

# Set number of iterations of analysis to determine conficance intervals
iterations = 10000

# Create dataframe to store results
results <- data.frame(n.subjects                            = numeric(),
                      k.itemsbank                           = numeric(),
                      k.sampled                             = numeric(),
                      true.alpha                            = numeric(),
                      full.true.positive                    = numeric(),
                      full.true.negative                    = numeric(),
                      full.false.positive                   = numeric(),
                      full.false.negative                   = numeric(),
                      cor.true.sum.full                     = numeric(),
                      cor.true.sum.full.lb                  = numeric(),
                      cor.true.sum.full.ub                  = numeric(),
                      bias.sum.full                         = numeric(),
                      sem.sum.full                          = numeric(),
                      alpha.full                            = numeric(),
                      alpha.full.lb                         = numeric(),
                      alpha.full.ub                         = numeric(),
                      alpha.full.spearman.brown             = numeric(),
                      alpha.full.spearman.brown.lb          = numeric(),
                      alpha.full.spearman.brown.ub          = numeric(),
                      alpha.full.sd                         = numeric(),
                      alpha.full.sd.lb                      = numeric(),
                      alpha.full.sd.ub                      = numeric(),
                      alpha.full.sd.spearman.brown          = numeric(),
                      alpha.full.sd.spearman.brown.lb       = numeric(),
                      alpha.full.sd.spearman.brown.ub       = numeric(),
                      alpha.full.lopez                      = numeric(),
                      alpha.full.lopez.lb                   = numeric(),
                      alpha.full.lopez.ub                   = numeric(),
                      alpha.full.lopez.spearman.brown       = numeric(),
                      alpha.full.lopez.spearman.brown.lb    = numeric(),
                      alpha.full.lopez.spearman.brown.ub    = numeric(),
                      partial.true.positive                 = numeric(),
                      partial.true.negative                 = numeric(),
                      partial.false.positive                = numeric(),
                      partial.false.negative                = numeric(),
                      cor.true.sum.partial                  = numeric(),
                      cor.true.sum.partial.lb               = numeric(),
                      cor.true.sum.partial.ub               = numeric(),
                      bias.sum.partial                      = numeric(),
                      sem.sum.partial                       = numeric(),
                      corrected.true.positive               = numeric(),
                      corrected.true.negative               = numeric(),
                      corrected.false.positive              = numeric(),
                      corrected.false.negative              = numeric(),
                      cor.true.sum.corrected.partial        = numeric(),
                      cor.true.sum.corrected.partial.lb     = numeric(),
                      cor.true.sum.corrected.partial.ub     = numeric(),
                      bias.sum.corrected.partial            = numeric(),
                      sem.sum.corrected.partial             = numeric(),
                      alpha.partial                         = numeric(),
                      alpha.partial.lb                      = numeric(),
                      alpha.partial.ub                      = numeric(),
                      alpha.partial.sd                      = numeric(),
                      alpha.partial.sd.lb                   = numeric(),
                      alpha.partial.sd.ub                   = numeric(),
                      alpha.partial.sd.spearman.brown       = numeric(),
                      alpha.partial.sd.spearman.brown.lb    = numeric(),
                      alpha.partial.sd.spearman.brown.ub    = numeric(),
                      alpha.partial.lopez                   = numeric(),
                      alpha.partial.lopez.lb                = numeric(),
                      alpha.partial.lopez.ub                = numeric(),
                      alpha.partial.lopez.spearman.brown    = numeric(),
                      alpha.partial.lopez.spearman.brown.lb = numeric(),
                      alpha.partial.lopez.spearman.brown.ub = numeric()
)

# Start simulation

# Set counter
counter = 1

for (n.j in N.j) {            # Loop through number of subjects
  
  # Sample theta's for number of test subjects
  N     = n.j
  theta = rnorm(N)  # Sample random normal abilities
  
  # Determine true pass/fail rate
  true.pass.fail <- ( theta > quantile(theta, .6, na.rm = T) ) * 1
  
  for (k.b in K.b) {          # Loop through items in bank
    
    # Sample beta's and discrimination parameters a.
    K     = k.b
    a     = runif(K, lb, ub) # discriminatie parameters from IRT
    beta  = runif(K,-2,2)    # diffuculty parameters from IRT
    
    # Create bata matrix, for each person beta for every item
    beta.matrix <- matrix(rep(beta, N), N, K, byrow = T)
    
    for (k.s in K.s) {       # Loop through sampled items
      
      # Set vectors for temporary storage
      vector.true.alpha                         <- vector()
      vector.full.true.positive                 <- vector()
      vector.full.true.negative                 <- vector()
      vector.full.false.positive                <- vector()
      vector.full.false.negative                <- vector()
      vector.cor.true.sum.full                  <- vector()
      vector.bias.sum.full                      <- vector()
      vector.sem.sum.full                       <- vector()
      vector.alpha.full                         <- vector()
      vector.alpha.full.spearman.brown          <- vector()
      vector.alpha.full.sd                      <- vector()
      vector.alpha.full.sd.spearman.brown       <- vector()
      vector.alpha.full.lopez                   <- vector()
      vector.alpha.full.lopez.spearman.brown    <- vector()
      vector.partial.true.positive              <- vector()
      vector.partial.true.negative              <- vector()
      vector.partial.false.positive             <- vector()
      vector.partial.false.negative             <- vector()
      vector.cor.true.sum.partial               <- vector()
      vector.bias.sum.partial                   <- vector()
      vector.sem.sum.partial                    <- vector()
      vector.corrected.true.positive            <- vector()
      vector.corrected.true.negative            <- vector()
      vector.corrected.false.positive           <- vector()
      vector.corrected.false.negative           <- vector()
      vector.cor.true.sum.corrected.partial     <- vector()
      vector.bias.sum.corrected.partial         <- vector()
      vector.sem.sum.corrected.partial          <- vector()
      vector.alpha.partial                      <- vector()
      vector.alpha.partial.sd                   <- vector()
      vector.alpha.partial.sd.spearman.brown    <- vector()
      vector.alpha.partial.lopez                <- vector()
      vector.alpha.partial.lopez.spearman.brown <- vector()
      
      for (i in 1:iterations) { # Loop through iterations
        
        # Create full matrix
        
        # Determine true cronbach alpha
        lambda = matrix(a)                    # put the discrimination parameters in matrix
        error  = diag(rep(r,K))
        Sigma  = lambda %*% t(lambda) + error # expected covariance matrix of the underlying continuous variable
        
        mv = mean(diag(Sigma))             # mean variance    mean diagonal
        mc = mean(Sigma[lower.tri(Sigma)]) # mean covariance  mean off-diagonal
        nr = k.s                           # Number of random responses
        
        # Calculate true alpha
        true.alpha = (K*mc) / (mv + (K-1)*mc)
        
        # Random continuous responses matrix
        y = theta %*% t(a) + mvrnorm(N, rep(0,K), error)
        
        # Transform to binary responses matrix
        rm <- ( y > beta.matrix ) * 1
        
        # Determine pass/fail rate on full response matrix
        rm.sum.scores  <- apply(rm, 1, sum, na.rm = T)
        full.pass.fail <- ( rm.sum.scores > quantile(rm.sum.scores, .6, na.rm = T) ) * 1
        
        ## Create full pass fail cross tab
        cross.tab.full.pass.fail <- prop.table(table(data.frame(true.pass.fail,  full.pass.fail)))
        
        ## Calculate true reliability, bias and standard error of measurement
        cor.true.sum.full <- cor(theta, rm.sum.scores)
        bias.sum.full     <- mean(scale(theta) - scale(rm.sum.scores))
        sem.sum.full      <-   sd(scale(theta) - scale(rm.sum.scores))
        
        # Calculate alpha's on full matrix
        
        # Alpha from full matrix
        alpha.full <- cronbach.alpha(rm, CI = F)$alpha
        
        # Alpha shortened using Spearman Brown correction
        
        # Determine shortening factor
        k = k.s / k.b
        alpha.full.spearman.brown <- SpearmanBrownPredictionFormula(k, alpha.full)
        
        # Alpha from SD of test
        
        ## First create proportions correct vector for items
        proportion.correct <- apply(rm, 2, mean, na.rm = T)
        
        ## Calculate standard diviation of the test
        SD.test  <- sdTest(rm)
        
        ## Calculate alpha using SD method
        alpha.full.sd <- kr20(N, SD.test, proportion.correct)
        
        ### Alpha shortened using Spearman Brown correction
        
        ### Determine shortening factor
        k = k.s / k.b
        alpha.full.sd.spearman.brown <- SpearmanBrownPredictionFormula(k, alpha.full.sd)
        
        # Alpha using Lopez
        
        ## Calculate item correlation matrix based on partial matrix
        item.correlation.matrix <- cor(rm, use = "pairwise.complete.obs", method = "pearson")
        
        ## Calculate alpha using Lopez method
        alpha.full.lopez <- Lopez(item.correlation.matrix, K)
        
        ### Alpha shortened using Spearman Brown correction
        
        ### Determine shortening factor
        k = k.s / k.b
        alpha.full.lopez.spearman.brown <- SpearmanBrownPredictionFormula(k,  alpha.full.lopez)
        
        # Create partial matrix
        
        # Create partial response matrix
        prm <- matrix(NA, N, K)
        
        for(n in 1:N) {
          
          # Random subset of full matrix
          selected_items <- sample(1:K, nr, replace=FALSE)
          
          # Assign responses on subset
          prm[n,selected_items] <- rm[n,selected_items]
          
        }
        
        # Determine pass/fail rate on partial response matrix
        prm.sum.scores <- apply(prm, 1, sum, na.rm = T)
        partial.pass.fail <- ( prm.sum.scores > quantile(prm.sum.scores, .6, na.rm = T) ) * 1
        
        ## Create partial pass fail cross tab
        cross.tab.partial.pass.fail <- prop.table(table(data.frame(true.pass.fail, partial.pass.fail)))        
        
        ## Calculate true reliability, bias and standard error of measurement
        cor.true.sum.partial <- cor(theta, prm.sum.scores)
        bias.sum.partial     <- mean(scale(theta) - scale(prm.sum.scores))
        sem.sum.partial      <-   sd(scale(theta) - scale(prm.sum.scores))        
        
        # Determine corrected pass/fail rate on partial response matrix
        prm.prop.cor         <- apply(prm, 2, mean, na.rm = T) # Proportion correct per item
        mean.test.difficulty <- mean(prm.prop.cor, na.rm = T)  # Mean difficulty of the test
        
        ## Create matrix with repeated proportion correct for each user
        prop.cor.matrix <- matrix(rep(prm.prop.cor, N), N, K, byrow = T)
        ## Create boolean matrix for all answered items in parial response matix
        answered.matrix <- !is.na(prm)
        ## Multiply answered item boolean matix with proportion correct matrix
        ## This results in only proportions correct for answered items.
        prop.cor.answered <- prop.cor.matrix * answered.matrix
        ## Replace zero values with NA
        prop.cor.answered[prop.cor.answered == 0] <- NA
        
        ## Calculate mean difficulty per student
        mean.student.difficulty <- apply(prop.cor.answered, 1, mean, na.rm = T) # Mean difficulty per student
        
        ## Create correction factor
        correction.factor <- mean.test.difficulty - mean.student.difficulty
        
        ## Calculate corrected sum scores
        ## Convert sum scores to percentages by deviding by number of administered items
        prm.corrected.sum.scores <- prm.sum.scores/nr + correction.factor
        
        ## Calculate corrected pass fail rate
        corrected.partial.pass.fail <- ( prm.corrected.sum.scores > quantile(prm.corrected.sum.scores, .6, na.rm = T) ) * 1          
        
        ## Create corrected pass fail cross tab
        cross.tab.corrected.pass.fail <- prop.table(table(data.frame(true.pass.fail,  corrected.partial.pass.fail)))
        
        ## Calculate true reliability, bias and standard error of measurement
        cor.true.sum.corrected.partial <- cor(theta, prm.corrected.sum.scores)
        bias.sum.corrected.partial     <- mean(scale(theta) - scale(prm.corrected.sum.scores))
        sem.sum.corrected.partial      <-   sd(scale(theta) - scale(prm.corrected.sum.scores))              
        
        # Calculate alpha's on partial matrix
        
        # Alpha from partial matrix
        alpha.partial <- cronbach.alpha(prm, CI = F, na.rm = T)$alpha
        
        # Alpha from SD of test
        
        ## First create proportions correc vector for items
        proportion.correct <- apply(prm, 2, mean, na.rm = T)
        
        ## Calculate standard diviation of the test
        SD.test  <- sdTest(prm)
        
        ## Calculate alpha using SD method
        alpha.partial.sd <- kr20(N, SD.test, proportion.correct)
        
        ### Alpha shortened using Spearman Brown correction
        
        #        ### Determine shortening factor
        k = k.s / k.b
        alpha.partial.sd.spearman.brown <- SpearmanBrownPredictionFormula(k, alpha.partial.sd)
        
        # Alpha using Lopez
        
        ## Calculate item correlation matrix based on partial matrix
        item.correlation.matrix <- cor(prm, use = "pairwise.complete.obs", method = "pearson")
        
        ## Calculate alpha using Lopez method
        alpha.partial.lopez <- Lopez(item.correlation.matrix, K)
        
        ### Alpha shortened using Spearman Brown correction
        
        ### Determine shortening factor
        k = k.s / k.b
        alpha.partial.lopez.spearman.brown <- SpearmanBrownPredictionFormula(k, alpha.partial.lopez)
        
        # Calculate individual alpha based on answered items
        
        ## Determine answered items for individual student. Creates boolean.
        # answered.items <- !is.na(prm[1,])
        
        ## Calculate standard diviation of the test
        # SD.test  <- sdTest(prm[,answered.items])
        
        ## Calculate individual alpha's
        
        # Store results in vector
        
        vector.true.alpha[i]                         <- true.alpha
        vector.full.true.positive[i]                 <- cross.tab.full.pass.fail[2,2]
        vector.full.true.negative[i]                 <- cross.tab.full.pass.fail[1,1]
        vector.full.false.positive[i]                <- cross.tab.full.pass.fail[2,1]
        vector.full.false.negative[i]                <- cross.tab.full.pass.fail[1,2]
        vector.cor.true.sum.full[i]                  <- cor.true.sum.full
        vector.bias.sum.full[i]                      <- bias.sum.full
        vector.sem.sum.full[i]                       <- sem.sum.full
        vector.alpha.full[i]                         <- alpha.full
        vector.alpha.full.spearman.brown[i]          <- alpha.full.spearman.brown
        vector.alpha.full.sd[i]                      <- alpha.full.sd
        vector.alpha.full.sd.spearman.brown[i]       <- alpha.full.sd.spearman.brown
        vector.alpha.full.lopez[i]                   <- alpha.full.lopez
        vector.alpha.full.lopez.spearman.brown[i]    <- alpha.full.lopez.spearman.brown
        vector.partial.true.positive[i]              <- cross.tab.partial.pass.fail[2,2]
        vector.partial.true.negative[i]              <- cross.tab.partial.pass.fail[1,1]
        vector.partial.false.positive[i]             <- cross.tab.partial.pass.fail[2,1]
        vector.partial.false.negative[i]             <- cross.tab.partial.pass.fail[1,2]
        vector.cor.true.sum.partial[i]               <- cor.true.sum.partial
        vector.bias.sum.partial[i]                   <- bias.sum.partial
        vector.sem.sum.partial[i]                    <- sem.sum.partial
        vector.corrected.true.positive[i]            <- cross.tab.corrected.pass.fail[2,2]
        vector.corrected.true.negative[i]            <- cross.tab.corrected.pass.fail[1,1]
        vector.corrected.false.positive[i]           <- cross.tab.corrected.pass.fail[2,1]
        vector.corrected.false.negative[i]           <- cross.tab.corrected.pass.fail[1,2]
        vector.cor.true.sum.corrected.partial[i]     <- cor.true.sum.corrected.partial
        vector.bias.sum.corrected.partial[i]         <- bias.sum.corrected.partial
        vector.sem.sum.corrected.partial[i]          <- sem.sum.corrected.partial
        vector.alpha.partial[i]                      <- alpha.partial
        vector.alpha.partial.sd[i]                   <- alpha.partial.sd
        vector.alpha.partial.sd.spearman.brown[i]    <- alpha.partial.sd.spearman.brown
        vector.alpha.partial.lopez[i]                <- alpha.partial.lopez
        vector.alpha.partial.lopez.spearman.brown[i] <- alpha.partial.lopez.spearman.brown
        
      } # end iterations
      
      # Store results
      results[counter, 'n.subjects']                            = n.j    			
      results[counter, 'k.itemsbank']                           = k.b				
      results[counter, 'k.sampled']                             = k.s				
      results[counter, 'true.alpha']                            = mean(vector.true.alpha,                         na.rm = T)
      results[counter, 'full.true.positive']                    = mean(vector.full.true.positive,                 na.rm = T)
      results[counter, 'full.true.negative']                    = mean(vector.full.true.negative,                 na.rm = T)
      results[counter, 'full.false.positive']                   = mean(vector.full.false.positive,                na.rm = T)
      results[counter, 'full.false.negative']                   = mean(vector.full.false.negative,                na.rm = T)
      results[counter, 'cor.true.sum.full']                     = mean(vector.cor.true.sum.full,                  na.rm = T)
      results[counter, 'cor.true.sum.full.lb']                  = quantile(vector.cor.true.sum.full, .025,        na.rm = T)
      results[counter, 'cor.true.sum.full.ub']                  = quantile(vector.cor.true.sum.full, .975,        na.rm = T)
      results[counter, 'bias.sum.full']                         = mean(vector.bias.sum.full,                      na.rm = T)
      results[counter, 'sem.sum.full']                          = mean(vector.sem.sum.full,                       na.rm = T)
      results[counter, 'alpha.full']                            = mean(vector.alpha.full,                         na.rm = T)
      results[counter, 'alpha.full.lb']                         = quantile(vector.alpha.full, .025,               na.rm = T)
      results[counter, 'alpha.full.ub']                         = quantile(vector.alpha.full, .975,               na.rm = T)
      results[counter, 'alpha.full.spearman.brown']             = mean(vector.alpha.full.spearman.brown,          na.rm = T)
      results[counter, 'alpha.full.spearman.brown.lb']          = quantile(vector.alpha.full.spearman.brown, .025, na.rm = T)
      results[counter, 'alpha.full.spearman.brown.ub']          = quantile(vector.alpha.full.spearman.brown, .975, na.rm = T)
      results[counter, 'alpha.full.sd']                         = mean(vector.alpha.full.sd,                      na.rm = T)
      results[counter, 'alpha.full.sd.lb']                      = quantile(vector.alpha.full.sd, .025,            na.rm = T)
      results[counter, 'alpha.full.sd.ub']                      = quantile(vector.alpha.full.sd, .975,            na.rm = T)
      results[counter, 'alpha.full.sd.spearman.brown']          = mean(vector.alpha.full.sd.spearman.brown,       na.rm = T)
      results[counter, 'alpha.full.sd.spearman.brown.lb']       = quantile(vector.alpha.full.sd.spearman.brown, .025, na.rm = T)
      results[counter, 'alpha.full.sd.spearman.brown.ub']       = quantile(vector.alpha.full.sd.spearman.brown, .975, na.rm = T)
      results[counter, 'alpha.full.lopez']                      = mean(vector.alpha.full.lopez,                   na.rm = T)
      results[counter, 'alpha.full.lopez.lb']                   = quantile(vector.alpha.full.lopez, .025,         na.rm = T)
      results[counter, 'alpha.full.lopez.ub']                   = quantile(vector.alpha.full.lopez, .975,         na.rm = T)
      results[counter, 'alpha.full.lopez.spearman.brown']       = mean(vector.alpha.full.lopez.spearman.brown,    na.rm = T)
      results[counter, 'alpha.full.lopez.spearman.brown.lb']    = quantile(vector.alpha.full.lopez.spearman.brown, .025, na.rm = T)
      results[counter, 'alpha.full.lopez.spearman.brown.ub']    = quantile(vector.alpha.full.lopez.spearman.brown, .975, na.rm = T)
      results[counter, 'partial.true.positive']                 = mean(vector.partial.true.positive,              na.rm = T)
      results[counter, 'partial.true.negative']                 = mean(vector.partial.true.negative,              na.rm = T)
      results[counter, 'partial.false.positive']                = mean(vector.partial.false.positive,             na.rm = T)
      results[counter, 'partial.false.negative']                = mean(vector.partial.false.negative,             na.rm = T)
      results[counter, 'cor.true.sum.partial']                  = mean(vector.cor.true.sum.partial,               na.rm = T)
      results[counter, 'cor.true.sum.partial.lb']               = quantile(vector.cor.true.sum.partial, .025,     na.rm = T)
      results[counter, 'cor.true.sum.partial.ub']               = quantile(vector.cor.true.sum.partial, .975,     na.rm = T)
      results[counter, 'bias.sum.partial']                      = mean(vector.bias.sum.partial,                   na.rm = T)
      results[counter, 'sem.sum.partial']                       = mean(vector.sem.sum.partial,                    na.rm = T)
      results[counter, 'corrected.true.positive']               = mean(vector.corrected.true.positive,            na.rm = T)
      results[counter, 'corrected.true.negative']               = mean(vector.corrected.true.negative,            na.rm = T)
      results[counter, 'corrected.false.positive']              = mean(vector.corrected.false.positive,           na.rm = T)
      results[counter, 'corrected.false.negative']              = mean(vector.corrected.false.negative,           na.rm = T)
      results[counter, 'cor.true.sum.corrected.partial']        = mean(vector.cor.true.sum.corrected.partial,     na.rm = T)
      results[counter, 'cor.true.sum.corrected.partial.lb']     = quantile(vector.cor.true.sum.corrected.partial, .025, na.rm = T)
      results[counter, 'cor.true.sum.corrected.partial.ub']     = quantile(vector.cor.true.sum.corrected.partial, .975, na.rm = T)
      results[counter, 'bias.sum.corrected.partial']            = mean(vector.bias.sum.corrected.partial,         na.rm = T)
      results[counter, 'sem.sum.corrected.partial']             = mean(vector.sem.sum.corrected.partial,          na.rm = T)
      results[counter, 'alpha.partial']                         = mean(vector.alpha.partial,                      na.rm = T)
      results[counter, 'alpha.partial.lb']                      = quantile(vector.alpha.partial, .025,            na.rm = T)
      results[counter, 'alpha.partial.ub']                      = quantile(vector.alpha.partial, .975,            na.rm = T)
      results[counter, 'alpha.partial.sd']                      = mean(vector.alpha.partial.sd,                   na.rm = T)
      results[counter, 'alpha.partial.sd.lb']                   = quantile(vector.alpha.partial.sd, .025,         na.rm = T)
      results[counter, 'alpha.partial.sd.ub']                   = quantile(vector.alpha.partial.sd, .975,         na.rm = T)
      results[counter, 'alpha.partial.sd.spearman.brown']       = mean(vector.alpha.partial.sd.spearman.brown,    na.rm = T)
      results[counter, 'alpha.partial.sd.spearman.brown.lb']    = quantile(vector.alpha.partial.sd.spearman.brown, .025, na.rm = T)
      results[counter, 'alpha.partial.sd.spearman.brown.ub']    = quantile(vector.alpha.partial.sd.spearman.brown, .975, na.rm = T)
      results[counter, 'alpha.partial.lopez']                   = mean(vector.alpha.partial.lopez,                na.rm = T)
      results[counter, 'alpha.partial.lopez.lb']                = quantile(vector.alpha.partial.lopez, .025,      na.rm = T)
      results[counter, 'alpha.partial.lopez.ub']                = quantile(vector.alpha.partial.lopez, .975,      na.rm = T)
      results[counter, 'alpha.partial.lopez.spearman.brown']    = mean(vector.alpha.partial.lopez.spearman.brown, na.rm = T)
      results[counter, 'alpha.partial.lopez.spearman.brown.lb'] = quantile(vector.alpha.partial.lopez.spearman.brown, .025, na.rm = T)
      results[counter, 'alpha.partial.lopez.spearman.brown.ub'] = quantile(vector.alpha.partial.lopez.spearman.brown, .975, na.rm = T)
      
      # Increment counter
      counter = counter + 1
      
    }     # end items sampled
  }       # end item bank size
}         # end nember of students

# Write results to file
write.xlsx(results, file="results-final2.xlsx", sheetName = "Sheet1", row.names = F)
