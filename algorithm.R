install.packages("GA")
library("GA")

myData <- read.csv(file = "sources.csv")

# fitness function
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
processRow <- function(x, y, row) euc.dist(c(row[1], row[2]), c(x,y)) * row[3]
fitnessFunction1 <- function(x) sum( apply(myData, 1, function(row) processRow(x[1], x[2], row)))

myClip <- function(x, a, b) {
  ifelse(x <= a,  a, ifelse(x >= b, b, x))
}

# this is gareal_raMutation changed to implement 
# gaussian mutation from https://en.wikipedia.org/wiki/Mutation_(genetic_algorithm)
gareal_gaussianMutation <- function(object, parent, ...) {
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  j <- sample(1:n, size = 1)
  mutate[j] <- myClip(mutate[j] + rnorm(1), object@min[j], object@max[j])
  return(mutate)
}

solutions <- c()

GA <- ga(type="real-valued", 
         fitness = function(x) -fitnessFunction1(x), 
         seed = 0,
         min = c(0,0), 
         max = c(10, 10), 
         popSize = 20, 
         maxiter = 100, 
         run = 20,
         selection = function(x) gareal_tourSelection(x, k = 2),
         pcrossover = 0,
         mutation = gareal_gaussianMutation.
         monitor = function(object) solutions <<- append(solutions, object@population))
