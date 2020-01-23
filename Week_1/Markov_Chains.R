#This is code from session of 1/22/2020



#install.packages("markovchain")
library(markovchain)
library(Matrix)


virusstrains <- c("A", "B")
byRow <- TRUE
P        <- matrix(c(0.85,0.4,0.15,0.6 ), ncol=2,nrow=2)
P
p0 = c(0.2,0.8)

##Create a new class of the type markovchain
Virus <- new("markovchain", states = virusstrains, byrow = byRow,
                 transitionMatrix = P, name = "Virus")

##Read matrix and calculate some probabilities distributions for future steps
Virus
Virus^2

##Statistc state - it doesn't change anymore
Virus^10

s = c(0.7273656, 0.2726344)

s * Virus
s * Virus^2
s * Virus^10

##Plot transition diagram
plot(Virus)

##Get states probabilities at the second generation
P1 <- p0 * (Virus)
P1

##third gneration
P2 = P1 * Virus
P2

##For a very nice set of video tutorials for an introduction to Markov Chains see 
##(https://www.youtube.com/watch?v=uvYTGEZQTEs&t=8s)
