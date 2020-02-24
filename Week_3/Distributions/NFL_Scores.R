##Derive binomial 

#Let's evaluate toss of a coin 
#heads = p
#tailes - 1-p = q

#toss coin 2 times

#HT
#Pr = pq

##toss 3 times


#Pr[A&B] = pq (tosses are independent)

#HHT
#Pr = p2q

##Probsability two heads in 3 tosses

#THH = p2q
#HTH = p2q
#HHT = p2q

#= P2 = 3p2q

#  3
#=  - * p2q
#  2

#where (3 2) is "3 choose 2" ( the number of ways to choose 2 items out of a collection of 3) ( n choose k)

#pmb binomial dist = (n k) p^k * q^n-k

#E[x] = pn mean
#v[x] = npq variance


###Superbowl score prediction tool Using Poisson Distribution
###Remember to set up your directory wherever your file is located, 
###If you are working on the jupyter environment on the folder where the file
###is contained you don't need to set the environment
setwd("/path/to/file/")

NFL = read.csv("nfl_elo_latest.csv", header = T,stringsAsFactors = F)

##Clean up file, remove last row (with superbowl info), and unnecessary columns
NFL = NFL[c(1:266),c(1,2,5,6,29,30,15,16)]

##I will need to calculate these values
# a. points scored at home by all teams
# b. point scored on the road by all teams

###Ofense
# c. points scored at home by Chiefs
# d. points scored on the road by 49ers
###Defense
# e. points allowed at home by chiefs
# f. points allowed on the road by 49ers

a = sum(NFL$score1)/nrow(NFL) #a
b = sum(NFL$score2)/nrow(NFL) #b

c = sum(NFL$score1[NFL$team1=="KC"])/length(NFL$score1[NFL$team1=="KC"]) #c
d = sum(NFL$score2[NFL$team2=="SF"])/length(NFL$score2[NFL$team2=="SF"]) #d

e = sum(NFL$score2[NFL$team1=="KC"])/length(NFL$score2[NFL$team1=="KC"]) #e
f  = sum(NFL$score1[NFL$team2=="SF"])/length(NFL$score1[NFL$team2=="SF"]) #f

##Get Attack Ratios (team points scored/normalized by league totals) 
A1 = c / a # Chiefs attack
A2  = d / b #49ers attack

#Defense Ratios

D1  = e / b #Chiefs defense
D2  = f / a #49ers defense

# 3. Predict scores:
  
pointsChiefs = A1 * D2 * a ##The chiefs are predicted to score 25.11 points
points49ers = A2 * D1 * b ##The 49ers are predicted to score 26.48 points

##Using the poison distribution, calculate to pdf for the chiefs scoring distribution
##and the 49rs in two different plots (black chiefs, red 49ers)
plot(dpois(0:40, pointsChiefs),type = "l")
lines(dpois(0:40, points49ers), col = "red")

##Plot CDF for both teams
plot(ppois(0:40, pointsChiefs),type = "l")
lines(ppois(0:40, points49ers),type = "l", col = "red")

##What is the probability that the chiefs score 30 points?
dpois(30, pointsChiefs)
#the 49ers?
dpois(30, points49ers)

##What is the probability that the chiefs score at least 30 points (CDF)
ppois(30,pointsChiefs) ##85%
ppois(30,points49ers) #78%
