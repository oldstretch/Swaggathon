# File for co-ocurence analysis

ds.rpas.cooc <- readRDS(paste0("/Users/ulifretzen/Swaggathon/providedData/ds.rotterdamPas.RData"))

library(plyr)
library(reshape2)
library(ggplot2)

View(ds.rpas.cooc)
# Analyze the distribution of activities based on type
activity.shares <- ddply(ds.rpas.cooc,
                         .(activity_type),
                         summarise,
                         nOccurances = length(activity_type))

activity.shares.ordered <- activity.shares[order(activity.shares$nOccurances,
                                                 decreasing = TRUE), ]

activity.shares.ordered$activity_type <- factor(activity.shares.ordered$activity_type)
activity.shares.ordered$n <- c(30:1)

# Conclusion: Film, Museum and Active activities are the most common activities


# Calculate 1% of activities
one.percent.barrier <- sum(activity.shares.ordered$nOccurances) * 0.01

ggplot(activity.shares.ordered, aes(x = n, y = nOccurances)) +
  geom_point() +
  geom_line() +
  geom_line(y = one.percent.barrier) +
  labs(title = "Number of Observations per activity type", y = "Activity type")

activity.shares..above.barrier <- activity.shares.ordered[
  activity.shares.ordered$nOccurances > one.percent.barrier, ]

activity.shares.ordered.above.barrier$percentage <- 
  activity.shares.ordered.above.barrier$nOccurances / sum(activity.shares.ordered$nOccurances)


ggplot(ds.rpas.cooc, aes(activity_type)) +
  geom_bar()
+
  geom_line() +
  geom_line(y = one.percent.barrier) +
  labs(title = "Number of Observations per activity type", y = "Activity type")


ggsave("/Users/ulifretzen/Swaggathon/results/Number_of_Observations.png")

# Conclusions: Some activities are way more common than others

# Calculate number of activities per person
activities.pp <- ddply(ds.rpas.cooc, 
                       .(passH_nb), 
                       summarise, 
                       nActivities = length(passH_nb))

activities.pp.ordered <- activities.pp[order(activities.pp$nActivities), ]

# tail(activities.pp.ordered)

ggplot(activities.pp.ordered, aes(x = nActivities, y = nOccurances)) +
  geom_point() +
  geom_line() +
  geom_line(y = one.percent.barrier)
# Conclusion: A large number of passholders are using the pass to participate in multiple events


# Calculate number of Passholders per nubmer of activities taken part in
passholders.per.nActivity <- ddply(activities.pp,
                                  .(nActivities),
                                  summarise,
                                  nPassHolders = length(nActivities))

passholders.per.nActivity.ordered <- passholders.per.nActivity[order(
  passholders.per.nActivity$nActivities), ]

head(passholders.per.nActivity.ordered, 40)
passholders.per.nActivity.ordered.40.act <- head(passholders.per.nActivity.ordered, 40)

ggplot(passholders.per.nActivity.ordered.40.act, aes(x = nActivities, y = nPassHolders)) +
  geom_point() +
  geom_line() +
  labs(title = "Number of people per number of activities")
# ggsave("/Users/ulifretzen/Swaggathon/results/Number_of_passholders_with_number_of_activities.png")

# Co-occurance analysis for activity type
df.rpas.wide <- dcast(ds.rpas.cooc, passH_nb ~ activity_type,
                      fun.aggregate = length)

rownames(df.rpas.wide) <- df.rpas.wide$passH_nb
df.rpas.wide <- df.rpas.wide[, -1]

df.rpas.wide <- as.data.frame(ifelse(df.rpas.wide >= 1, 1, 0))

# head(df.rpas.wide)

# Find Co-occurence parameters
table(ds.rpas.cooc$activity_type, ds.rpas.cooc$activity_type)


# Create decision rules
library(arules)
myRules <- apriori(as.matrix(df.rpas.wide),
                   parameter = list(support = 0.01,
                                    confidence = 0.1,
                                    minlen = 2, maxlen = 2,
                                    target = "rules"))

inspect(myRules)
summary(myRules)
inspect(head(myRules, by ="lift", 10))

library(arulesViz)

# Create visualizations
plot(myRules)


plot(myRules, shading = "order",
     control = list(main = "Two-key plot"),
     pch = 19, col = rainbow (5)[c(1,3)])

plot(myRules , method = "grouped")

subRules <- head(sort(myRules, by = "lift"), 15)
plot(subRules , method = "graph")



#################### Analysis for activity category (not so interesting)
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

# save script as pdf
knitr::stitch('Co-occurence.R')
