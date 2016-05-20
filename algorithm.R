install.packages("GA")
library("GA")

myData <- read.csv(file = "sources.csv")

# fitness function
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
processRow <- function(x, y, row) euc.dist(c(row[1], row[2]), c(x,y)) * row[3]
fitnessFunction1 <- function(x) sum( apply(myData, 1, function(row) processRow(x[1], x[2], row)))

GA <- ga(type="real-valued", 
         fitness = function(x) -fitnessFunction1(x), 
         min = c(0,0), 
         max = c(10, 10), 
         popSize = 20, 
         maxiter = 1000, 
         run = 300,
         selection = function(x) gareal_tourSelection(x, k = 2))
