#These are the packages to use on this script
library(gRain) ##This package requires additional dependencies obtained from BioConductor
library(qgraph)
library(bnlearn)
library(stringr)

#additionally bnstruct has exact algorithms for learning network
#library(bnstruct) 

####---------First We need to get our data in the correct format-----------#############
setwd("/your/directory")

##Read csv file
data2015 = read.csv("2015_CHR_Analytic_Data.csv", stringsAsFactors = F, header = T)

##Remove county names and not ranked counties columns (4 and 5)
data2015B = data2015[,-c(4,5)]

##We also need to remove state totals which is county code == 0
##conditional subsetting
data2015B = data2015B[!(data2015B$COUNTYCODE==0),]

###
##Get the names of the columns
colnames(data2015B)

#Subset columns that have the string "Value" in the name
data2015_value = data2015B[ ,grepl("Value",names(data2015B))]
#grepl( "Value" , names( data2015B ))

#Combine first three columns (Statecode,countycode and State) to the columns with the values
data2015_value =cbind(data2015B[,c(1:3)],data2015_value)

## Randomly select a state
sample(data2015_value$State,1)

##Subset all rows from a state
data2015_ALA = data2015_value[data2015_value$State=="CA",]

####In this case I am only selecting 5 variables to the BN
data2015_ALA_BN_data = data2015_ALA[,c(4,10,18,25,27)]

##I need to remove the , from the values so that I can convert it as numeric (right now is a character)
data2015_ALA_BN_data$Premature.death.Value = 
  gsub(",","",data2015_ALA_BN_data$Premature.death.Value)

###IF you need to impute the data use
# library(mice)
# init = mice(data2015_ALA_BN_data, maxit=0) 
# meth = init$method
# predM = init$predictorMatrix
# set.seed(103)
# imputed = mice(data2015_ALA_BN_data, method=meth, predictorMatrix=predM, m=5)
# imputed <- complete(imputed)


##Convert variable to numeric
data2015_ALA_BN_data$Premature.death.Value = 
  as.numeric(data2015_ALA_BN_data$Premature.death.Value)

##########From here on we start the Bayesian Network pipeline
##

##Because the variables are continuous we need to convert them to discrete, in this easy example 
## I am only generating two levels for each variable, however, there is a lot of lost information doing it this way
## It is better to have fewer nodes but more levels. 

##If you have more levels >2 per node then the fold changes calculations need to address the multiple levels
data2015_ALA_BN_disc = discretize(data2015_ALA_BN_data, 
                                  method = "hartemink", breaks = 2,
                                  ibreaks = 3, idisc = "quantile")

##First lets learn the network using a greedy algorithm (HC)
net_bnlearn = hc(data2015_ALA_BN_disc, score = "bde", iss = 10)

##Plot the Hill Climbing network
plot(net_bnlearn)

##Plot distribution variables
plot(density(data2015_ALA_BN_data$Unemployment.Value))

##Another way to plot the networks as an html object
library(bnviewer)

viewer(net_bnlearn,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "100vh",
       bayesianNetwork.layout = "layout_nicely",
       bayesianNetwork.title="Discrete Bayesian Network - Hill Climbing",
       bayesianNetwork.subtitle = "Pain project",
       bayesianNetwork.footer = "Fig. 1 - Layout with Sugiyama",
       node.font = list(color = "Black", face="Arial", size = 40),
       options.nodesIdSelection = TRUE,
       options.highlightNearest = list(enabled = T, degree = 1, hover = T),
       node.colors = list(background = "black",
                          border = "#2b7ce9",
                          highlight = list(background = "#e91eba",
                                           border = "#2b7ce9"))
)
##################-----Optional---######################
##################----------------######################
##################----------------######################
##################-----Another option is to learn the network using the bnstruct package and save the adjacent matrix
##################-----to fit the best network
##################-----but the data need to be converted to numeric
# data2015_ALA_BN_Num = data2015_ALA_BN_disc
# library(dplyr)
# ##################-----This will convert the factors to integers
# data2015_ALA_BN_Num = data2015_ALA_BN_disc %>% mutate_if(is.factor,as.integer)
# ##################-----Create an object BN from the data
# detach("package:bnlearn", unload=TRUE)
# library(bnstruct)
# data2015_bnstruct <- BNDataset(data = data2015_ALA_BN_Num, 
#                                discreteness = rep(T, ncol(data2015_ALA_BN_Num)),
#                                variables = colnames(data2015_ALA_BN_Num),
#                                starts.from = 1,
#                                node.sizes = c(replicate(ncol(data2015_ALA_BN_Num),2)))
# ##################-----Learn the structure of the object using the exact algorithm
# netdata2015_bnstruct <- learn.network(data2015_bnstruct,algo = "sm")
# plot(netdata2015_bnstruct)
# ##################-----Save the adjacent matrix and use it in bnlearn to fit the data to best structure
# netdata2015_bnstruct_dag = netdata2015_bnstruct@dag
# ##################-----Convert numeric to factor
# data2015_ALA_BN_fact <- data2015_ALA_BN_Num %>% mutate_if(is.integer,as.factor)
# ##################-----Create an empty graph with the variables from the dataset
# detach("package:bnstruct", unload=TRUE)
# library(bnlearn)
# data2015_ALA_BN_fact.net2 = empty.graph(names(data2015_ALA_BN_fact))
# ##################-----Get the adjacent matrix from bnstruct
# amat(data2015_ALA_BN_fact.net2) <- netdata2015_bnstruct_dag
# ##################-----Fit the data to the structure
# net_fit <- bn.fit(data2015_ALA_BN_fact.net2, data2015_ALA_BN_disc, method = "bayes",
#                   iss = 10)
##################----------------######################
##################----------------######################
##################-----Continue here---######################

##Fit data to the structure
net_fit <- bn.fit(net_bnlearn, data2015_ALA_BN_disc, method = "bayes",
                  iss = 10)
##Convert the bn.fit object to a grain object to get propagated probability calculations
net_fit_junction = compile(as.grain(net_fit), propagate = T)

##Create a data frame that contains the pairwise node combination for each edge using a for loop
All_combs_goodNet = data.frame(Comb=character(), stringsAsFactors = F)
for (i in 1:length(names(net_fit_junction[["dag"]]@edgeData@data))){
  All_combs_goodNet[i,1] = names(net_fit_junction[["dag"]]@edgeData@data[i])
}

##Split the column into two columns using the character "|" as delimitator
All_combs_goodNet_Split = data.frame(str_split_fixed(All_combs_goodNet$Comb, 
                                                     fixed('|'), 2), stringsAsFactors = F)

##Generate fold changes for each edge combination
##e.g. if we have nodes A and B with levels 0 and 1 each
##Fold change = ((A(1)|B(1))/(A(1)|B(0)) + (B(1)|A(1))/(B(1)|A(0)) /2)
fold = data.frame(fold=numeric(), stringsAsFactors = F)
for (i in 1:nrow(All_combs_goodNet_Split)){
  fold[i,1] =  (((querygrain(setFinding(net_fit_junction, nodes = All_combs_goodNet_Split[i,1], states = net_fit_junction$universe$levels[All_combs_goodNet_Split[i,1]][[1]][1]), nodes = All_combs_goodNet_Split[i,2])[[1]][[2]]/
                    querygrain(setFinding(net_fit_junction, nodes = All_combs_goodNet_Split[i,1], states = net_fit_junction$universe$levels[All_combs_goodNet_Split[i,1]][[1]][2]), nodes = All_combs_goodNet_Split[i,2])[[1]][[2]])+
                   (querygrain(setFinding(net_fit_junction, nodes = All_combs_goodNet_Split[i,2], states =net_fit_junction$universe$levels[All_combs_goodNet_Split[i,2]][[1]][1]), nodes = All_combs_goodNet_Split[i,1])[[1]][2]/
                      querygrain(setFinding(net_fit_junction, nodes = All_combs_goodNet_Split[i,2],states = net_fit_junction$universe$levels[All_combs_goodNet_Split[i,2]][[1]][2]), nodes = All_combs_goodNet_Split[i,1])[[1]][2]))/2)
}

##Add the vector to the dataframe with all the edge combinations
All_combs_goodNet_Split$fold = fold$fold

##Create a new dataframe just to save the previous one
PanelAlE_netAN2 = All_combs_goodNet_Split
##Add the log of the fold to get negative and possitive values
PanelAlE_netAN2$fold = log(PanelAlE_netAN2$fold)

##Plot Network using the package qgraph

PanelAgrap2_AN2 = qgraph(PanelAlE_netAN2,
                         vsize = 10,colFactor = 0.4, 
                         esize = 10, label.scale=F,label.prop = 0.8,
                         negCol = c("red"),
                         posCol = c("darkblue"),
                         border.width = 2,
                         vTrans = 250)

#######----------Probabilities----------####
##Marginal
##We get values for both states
querygrain(net_fit_junction,nodes = c(All_combs_goodNet_Split$X1[1]), type = "marginal") 

##Joint
##We get values for both states
querygrain(net_fit_junction,nodes = c(All_combs_goodNet_Split$X1[1],All_combs_goodNet_Split$X2[2]), type = "joint")

##Conditional
##We get values for both states
##if we want to calculate P(Premature.death.Value | "Adult.obesity.Value" (1),"Unemployment.Value"(2))

querygrain(setEvidence(net_fit_junction, nodes = c("Adult.obesity.Value","Unemployment.Value"),
                       states = c("(0.25,0.339]","[0.044,0.06]"),
                       propagate = T), 
           nodes = "Premature.death.Value")

##if we want to calculate P(Premature.death.Value | "Adult.obesity.Value" (1))
querygrain(setEvidence(net_fit_junction, nodes = c("Adult.obesity.Value"),
                       states = c("(0.25,0.339]"),
                       propagate = T), 
           nodes = "Premature.death.Value")

##Relative Risk
##We can calulate RR by dividing the conditional present by the conditional absent
cond = querygrain(setEvidence(net_fit_junction, nodes = c("Adult.obesity.Value"),
                              states = c("(0.25,0.339]"),
                              propagate = T), 
                  nodes = "Premature.death.Value")[[1]][2]
cond2 = querygrain(setEvidence(net_fit_junction, nodes = c("Adult.obesity.Value"),
                              states = c("(0.339,0.481]"),
                              propagate = T), 
                  nodes = "Premature.death.Value")[[1]][2]

RR = cond2/cond

##We can also generate bootstraps from our data and generate a confidence score
data2015boot2 <- boot.strength(data2015_ALA_BN_disc, R = 1000, algorithm = "hc",
                             algorithm.args = list(score = "loglik"))

data2015boot2

## You can multiply the strength to the fold change to obtain a more accurate value for the magnitude of the dependencies

