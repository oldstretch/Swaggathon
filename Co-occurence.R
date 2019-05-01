# File for co-ocurence analysis

ds.rpas.cooc <- readRDS(paste0("/Users/ulifretzen/Swaggathon/providedData/ds.rotterdamPas.RData"))

library(plyr)
library(reshape2)

View(ds.rpas.cooc)

# Analysis for activity type
df.rpas.wide <- dcast(ds.rpas.cooc, passH_nb ~ activity_type,
                      fun.aggregate = length)

rownames(df.rpas.wide) <- df.rpas.wide$passH_nb
df.rpas.wide <- df.rpas.wide[, -1]

df.rpas.wide <- as.data.frame(ifelse(df.rpas.wide >= 1, 1, 0))

head(df.rpas.wide)
# Order the instances
# Plot the graph showing that few account for most of the 

library(arules)
myRules <- apriori(as.matrix(df.rpas.wide),
                   parameter = list(support = 0.01,
                                    confidence = 0.1,
                                    minlen = 2, maxlen = 2,
                                    target = "rules"))

inspect(myRules)
summary(myRules)
inspect(head(myRules, by ="lift", 10))


# Analysis for activity category
df.rpas.wide <- dcast(ds.rpas.cooc, passH_nb ~ activity_category,
                      fun.aggregate = length)

rownames(df.rpas.wide) <- df.rpas.wide$passH_nb
df.rpas.wide <- df.rpas.wide[, -1]

df.rpas.wide <- as.data.frame(ifelse(df.rpas.wide >= 1, 1, 0))

head(df.rpas.wide)
# Order the instances
# Plot the graph showing that few account for most of the 

library(arules)
myRules <- apriori(as.matrix(df.rpas.wide),
                   parameter = list(support = 0.01,
                                    confidence = 0.1,
                                    minlen = 2, maxlen = 2,
                                    target = "rules"))

inspect(myRules)
summary(myRules)
inspect(head(myRules, by ="lift", 10))

