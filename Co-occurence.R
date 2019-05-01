# File for co-ocurence analysis

load("providedData/rotterdampas.RData")
ds.rpas.cooc <- Rotterdampas_2017_2018

library(plyr)
library(reshape2)

View(ds.rpas.cooc)

df.rpas.wide <- dcast(ds.rpas.cooc, Passholder ~ Type)

# Order the instances
# Plot the graph showing that few account for most of the 

library(arules)
myRules <- apriori(as.matrix(df.rpas.wide),
                   parameter = list(support = 0.01,
                                    confidence = 0.1,
                                    minlen = 2, maxlen = 3,
                                    target = "rules"))

inspect(myRules)
summary(myRules)
inspect(head(myRules, by ="lift", 10))