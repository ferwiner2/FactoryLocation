# install.packages("rgp")
# library("rgp")

functionSet1 <- functionSet("+", "-", "*")
inputVariableSet1 <- inputVariableSet("x")
constantFactorySet1 <- constantFactorySet(function() rnorm(1))

interval1 <- seq(from = -pi, to = pi, by = 0.1)
fitnessFunction1 <- function(f) rmse(f(interval1), sin(interval1))

set.seed(1)
gpResult1 <- geneticProgramming(functionSet = functionSet1,
                                inputVariables = inputVariableSet1,
                                constantSet = constantFactorySet1,
                                fitnessFunction = fitnessFunction1,
                                stopCondition = makeTimeStopCondition(30))

bestSolution1 <- gpResult1$population[[which.min(gpResult1$fitnessValues)]]

# bestSolution1 to funkcja ktora przyjmuje 1 argument i zwraca wartosc 
# sin(arg) w zakresie -pi +pi