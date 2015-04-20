# Visualise results form simulation

## Load library for reading xlsx file
library('xlsx')

## Read data from xlsx file
results <- read.xlsx("results-final2.xlsx", sheetName = "Sheet1")

## Create variables needed for visualisation

### Alhpa's
alphas <- c("true.alpha", 
            "cor.true.sum.full", 
            "alpha.full", 
            "alpha.full.spearman.brown", 
            "alpha.full.sd", 
            "alpha.full.sd.spearman.brown", 
            "alpha.full.lopez", 
            "alpha.full.lopez.spearman.brown", 
            "cor.true.sum.partial", 
            "cor.true.sum.corrected.partial", 
            "alpha.partial", 
            "alpha.partial.sd", 
            "alpha.partial.sd.spearman.brown", 
            "alpha.partial.lopez", 
            "alpha.partial.lopez.spearman.brown")

### Lower bound of alpha's
alphas.lb <- c("cor.true.sum.full.lb", 
               "alpha.full.lb", 
               "alpha.full.spearman.brown.lb", 
               "alpha.full.sd.lb", 
               "alpha.full.sd.spearman.brown.lb", 
               "alpha.full.lopez.lb", 
               "alpha.full.lopez.spearman.brown.lb", 
               "cor.true.sum.partial.lb", 
               "cor.true.sum.corrected.partial.lb", 
               "alpha.partial.lb", 
               "alpha.partial.sd.lb", 
               "alpha.partial.sd.spearman.brown.lb", 
               "alpha.partial.lopez.lb", 
               "alpha.partial.lopez.spearman.brown.lb")

### Upper bound of alpha's
alphas.ub <- c("cor.true.sum.full.ub", 
               "alpha.full.ub", 
               "alpha.full.spearman.brown.ub", 
               "alpha.full.sd.ub", 
               "alpha.full.sd.spearman.brown.ub", 
               "alpha.full.lopez.ub", 
               "alpha.full.lopez.spearman.brown.ub", 
               "cor.true.sum.partial.ub", 
               "cor.true.sum.corrected.partial.ub", 
               "alpha.partial.ub", 
               "alpha.partial.sd.ub", 
               "alpha.partial.sd.spearman.brown.ub", 
               "alpha.partial.lopez.ub", 
               "alpha.partial.lopez.spearman.brown.ub")

## Assign lower and upper bounds to vector
lb <- results[1,alphas.lb]
ub <- results[1,alphas.ub]

## Set number of alpha's
n.alphas <- length(alphas)

## Set y positions. One point for each variable
ycor = seq(0.6, 1, length.out = n.alphas)

## Set outer margins
par(mar=c(4,11,1,1))

## Set plotting canvas
plot(ycor, 1:n.alphas, 
     type     = "n",            # We will plot dots later
     xlim     = c(0.6,1),       # x limit starting at .6 through 1
     xlab     = "reliability",  # Set x label
     ylab     = "",             # We will set the y labels later
     yaxt     = "n",            # So no y axis for now
     las      = 1,              # Axis always horizontal
     cex.axis = .7,             # Axis font size 70%
     cex.lab  = .7)             # Label font size 70%

## Plot benchmark line
lines(rep(results[1,"true.alpha"],2),c(0,n.alphas+1), col="red")

## Plot alpha values
points(results[1,alphas], 1:n.alphas)

## Plot confidence intervals
segments(y0 = 2:n.alphas,         # y start position
         x0 = as.numeric(lb[1,]), # x start position is lower bound of alpha
         y1 = 2:n.alphas,         # y end position
         x1 = as.numeric(ub[1,])) # x end position is upper bound of alpha

## Use variable names as labels. First clean it up
### Replace . with space and partial by sparse
label.names = gsub("\\.", " ", alphas, perl=T)
### Use the word sparse instead of parital
label.names = gsub("partial", "sparse", label.names, perl=T)
### Use the word KR20 in stead of sd
label.names = gsub(" sd", " KR20", label.names, perl=T)

# Add y-axis labels
axis(2, 1:n.alphas, labels=label.names, las = 1, cex.axis=.7)
